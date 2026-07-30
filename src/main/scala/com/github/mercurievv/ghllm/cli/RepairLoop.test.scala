package com.github.mercurievv.ghllm.cli

import com.github.mercurievv.ghllm.*
import com.github.mercurievv.ghllm.agent.*
import com.github.mercurievv.ghllm.arrow.*
import com.github.mercurievv.ghllm.cli.*
import com.github.mercurievv.ghllm.git.*
import com.github.mercurievv.ghllm.metrics.*

import cats.data.Kleisli
import cats.effect.IO
import cats.effect.Ref
import cats.syntax.all.*
import munit.CatsEffectSuite

class RepairLoopSuite extends CatsEffectSuite:
  type TestFlow[A, B] = Kleisli[IO, A, B]

  private val boom = RuntimeException("boom")

  test("succeeds without ever consulting routeFailure"):
    Ref[IO].of(0).flatMap { routed =>
      val loop = RepairLoop[TestFlow, Int](
        action = Kleisli(_ => IO.unit),
        routeFailure = Kleisli(_ => routed.update(_ + 1).as(Left(boom))),
        raiseFailure = Kleisli(IO.raiseError)
      )
      loop.run(1) *> routed.get.map(assertEquals(_, 0))
    }

  test("repairs and retries until the action succeeds"):
    Ref[IO].of(0).flatMap { attempts =>
      val loop = RepairLoop[TestFlow, Int](
        action = Kleisli(_ => attempts.updateAndGet(_ + 1).flatMap(n => IO.raiseError(boom).whenA(n < 3))),
        routeFailure = Kleisli { case (state, _) => IO.pure(Right(state)) },
        raiseFailure = Kleisli(IO.raiseError)
      )
      loop.run(1) *> attempts.get.map(assertEquals(_, 3))
    }

  test("a failed step repairs and retries the whole chain"):
    Ref[IO].of(List.empty[String]).flatMap { events =>
      Ref[IO].of(0).flatMap { attempts =>
        val chain = Kleisli { (state: Int) =>
          attempts.updateAndGet(_ + 1).flatMap { attempt =>
            events.update(_ :+ s"commit-$attempt") *>
              IO.raiseError[Unit](boom).whenA(attempt == 1) *>
              events.update(_ :+ s"push-$attempt") *>
              IO.raiseError[Unit](boom).whenA(attempt == 2) *>
              events.update(_ :+ s"gha-$attempt")
          }
        }
        val loop = RepairLoop[TestFlow, Int](
          action = chain,
          routeFailure = Kleisli { case (state, _) =>
            events.update(_ :+ "repair").as(Right(state))
          },
          raiseFailure = Kleisli(IO.raiseError)
        )

        loop.run(1) *> events.get.map { seen =>
          assertEquals(
            seen,
            List(
              "commit-1",
              "repair",
              "commit-2",
              "push-2",
              "repair",
              "commit-3",
              "push-3",
              "gha-3"
            )
          )
        }
      }
    }

  test("a budget carried in the loop state bounds the retries"):
    Ref[IO].of(0).flatMap { attempts =>
      val loop = RepairLoop[TestFlow, Int](
        action = Kleisli(_ => attempts.update(_ + 1) *> IO.raiseError(boom)),
        // The state IS the remaining budget: exhausting it gives up.
        routeFailure = Kleisli { case (budget, error) =>
          IO.pure(if budget > 0 then Right(budget - 1) else Left(error))
        },
        raiseFailure = Kleisli(IO.raiseError)
      )
      loop.run(2).attempt.flatMap { result =>
        attempts.get.map { count =>
          assertEquals(result, Left(boom))
          assertEquals(count, 3)
        }
      }
    }

  test("an unrepairable failure is re-raised on the first attempt"):
    Ref[IO].of(0).flatMap { attempts =>
      val loop = RepairLoop[TestFlow, Int](
        action = Kleisli(_ => attempts.update(_ + 1) *> IO.raiseError(boom)),
        routeFailure = Kleisli { case (_, error) => IO.pure(Left(error)) },
        raiseFailure = Kleisli(IO.raiseError)
      )
      loop.run(1).attempt.flatMap { result =>
        attempts.get.map { count =>
          assertEquals(result, Left(boom))
          assertEquals(count, 1)
        }
      }
    }

class AgentRunArrowsSuite extends CatsEffectSuite:
  type TestFlow[A, B] = Kleisli[IO, A, B]

  private val context = RunContext(os.pwd, AgentInventory(Nil), None)
  private val issue = Issue(TaskNumber(1), IssueTitle("Task"), IssueBody(""), State("open"))
  private val weak = TaskRunner(AgentBinary("cheap"), None, None, None)
  private val strong = TaskRunner(AgentBinary("strong"), None, None, None)
  private def prepared(runner: TaskRunner) =
    PreparedTask(
      ClaimedTask(context, issue, runner, os.pwd, BranchName("task-1"), None),
      None,
      None
    )
  private val boom = RuntimeException("runner failed")

  test("a failed run escalates to the stronger runner exactly once"):
    Ref[IO].of(List.empty[AgentBinary]).flatMap { runners =>
      val runTaskWithRunner = Kleisli { (task: PreparedTask) =>
        val runner = task.claimedTask.runner
        runners.update(_ :+ runner.agent) *>
          (if runner.agent.value == weak.agent.value then IO.raiseError(boom)
           else IO.pure(ExecutedTask(task.claimedTask, AgentOutput("ok"))))
      }
      val retryingRunTask = BusinessLogicRetry.retryRunTaskWithRunner[IO](
        routeFallback = Kleisli { case (task, _) =>
          IO.pure(Right(task.copy(claimedTask = task.claimedTask.copy(runner = strong))))
        },
        raiseFailure = Kleisli(IO.raiseError)
      )(runTaskWithRunner)

      retryingRunTask.run(prepared(weak)).flatMap { result =>
        runners.get.map { attempted =>
          assertEquals(result.output.value, "ok")
          assertEquals(attempted, List(weak.agent, strong.agent))
        }
      }
    }

  test("a red verifier escalates, not just a dead runner"):
    // The agent itself succeeds every time; only the verifier objects, which is
    // what "red" almost always is - it did not compile, or a test failed. Until
    // validation was composed into the retried unit this raised straight past
    // the ladder and killed the task.
    Ref[IO].of(List.empty[AgentBinary]).flatMap { runners =>
      val runTaskWithRunner = Kleisli { (task: PreparedTask) =>
        runners.update(_ :+ task.claimedTask.runner.agent)
          .as(ExecutedTask(task.claimedTask, AgentOutput("ok")))
      }
      val validate = Kleisli { (executed: ExecutedTask) =>
        if executed.run.runner.agent.value == weak.agent.value then
          IO.raiseError(RuntimeException("tests failed"))
        else IO.pure(executed)
      }
      val retryingRunTask = BusinessLogicRetry.retryRunTaskWithRunner[IO](
        routeFallback = Kleisli { case (task, _) =>
          IO.pure(Right(task.copy(claimedTask = task.claimedTask.copy(runner = strong))))
        },
        raiseFailure = Kleisli(IO.raiseError)
      )(runTaskWithRunner.andThen(validate))

      retryingRunTask.run(prepared(weak)).flatMap { result =>
        runners.get.map { attempted =>
          assertEquals(result.output.value, "ok")
          assertEquals(attempted, List(weak.agent, strong.agent))
        }
      }
    }

  test("validation is not left on the post-agent slot as well"):
    // It now runs inside the loop. Left here too it would re-run the whole
    // suite on the winning attempt and record a second sample for that runner.
    val bomb: Kleisli[IO, ExecutedTask, ExecutedTask] =
      Kleisli(_ => IO.raiseError(RuntimeException("validation ran twice")))
    val slot =
      BusinessLogicRetry[IO](_ => IO.unit).executeTaskArrows.runProjectValidation(bomb)

    slot
      .run(ExecutedTask(prepared(weak).claimedTask, AgentOutput("ok")))
      .map(result => assertEquals(result.output.value, "ok"))

  test("with no stronger runner the failure propagates"):
    val retryingRunTask = BusinessLogicRetry.retryRunTaskWithRunner[IO](
      routeFallback = Kleisli { case (_, verdict) => IO.pure(Left(verdict)) },
      raiseFailure = Kleisli(IO.raiseError)
    )(Kleisli(_ => IO.raiseError(boom)))

    // `Failed` must hand back the original throwable, not a rewrapped one.
    retryingRunTask.run(prepared(weak)).attempt.map(result => assertEquals(result, Left(boom)))

  // A ladder needs both rungs available for nextStrongerImplementor to find one.
  private def tool(agent: String) =
    AgentTool(
      id = AgentToolId(agent),
      agent = Agent(agent),
      model = None,
      effort = None,
      version = None,
      roles = List("implementor"),
      jobTypes = Nil,
      strengths = Nil,
      available = Available(true)
    )

  private val ladder = RunContext(os.pwd, AgentInventory(List(tool("cheap"), tool("strong"))), None)

  private def onLadder(runner: TaskRunner, escalationDepth: Int = 0) =
    PreparedTask(
      ClaimedTask(ladder, issue, runner, os.pwd, BranchName("task-1"), None),
      None,
      None,
      escalationDepth
    )

  private def route(task: PreparedTask, verdict: VerificationResult) =
    Ref[IO].of(List.empty[String]).flatMap { messages =>
      BusinessLogicRetry
        .routeRunnerFallback[IO](message => messages.update(_ :+ message))
        .run((task, verdict))
        .flatMap(routed => messages.get.map(routed -> _))
    }

  test("a Red verdict escalates to a stronger runner and records the depth"):
    route(onLadder(weak), VerificationResult.Red("turn cap exceeded", "26 turns")).map {
      case (Right(escalated), seen) =>
        assertEquals(escalated.claimedTask.runner.agent.value, "strong")
        assertEquals(escalated.escalationDepth, 1)
        // The Red's summary, not a throwable's message, is what reaches the log.
        assert(seen.exists(_.contains("turn cap exceeded")))
      case (Left(verdict), _) => fail(s"expected escalation, got $verdict")
    }

  test("escalation stops at depth 2"):
    val verdict = VerificationResult.Red("still failing", "third attempt")
    route(onLadder(weak, escalationDepth = BusinessLogicRetry.MaxEscalationDepth), verdict).map {
      case (Left(returned), seen) =>
        assertEquals(returned, verdict)
        assert(seen.exists(_.contains("Giving up")))
      case (Right(task), _) => fail(s"expected the cap to stop escalation, got ${task.claimedTask.runner.display}")
    }

  test("a Green verdict is returned unchanged rather than escalated"):
    route(onLadder(weak), VerificationResult.Green).map { case (routed, _) =>
      assertEquals(routed, Left(VerificationResult.Green))
    }

class RepairRunnerSequenceSuite extends munit.FunSuite:
  private def tool(agent: String) =
    AgentTool(
      id = AgentToolId(agent),
      agent = Agent(agent),
      model = None,
      effort = None,
      version = None,
      roles = List("implementor"),
      jobTypes = Nil,
      strengths = Nil,
      available = Available(true)
    )

  private val cheap = TaskRunner(AgentBinary("cheap"), None, None, None)

  private def sequence(inventory: AgentInventory, attempts: Int) =
    BusinessLogicRetry.repairRunnerSequence(inventory, cheap, attempts).map(_.agent.value)

  test("a failed repair moves to a different runner, never the same one twice"):
    // The build-check retry used to re-run the runner that had just failed to
    // fix the build, up to MaxRepairBuildCheckAttempts times. Each of those
    // re-runs is paid for and none of them has new information.
    val inventory = AgentInventory(List(tool("cheap"), tool("mid"), tool("strong")))
    assertEquals(sequence(inventory, 3), List("cheap", "mid", "strong"))

  test("the alternates run out before the budget does, and the last one repeats"):
    // Abandoning the repair because the inventory is short would be worse than
    // the behaviour this replaced. Once exhausted it stays on the last runner
    // chosen rather than cycling back to one that already failed earlier.
    val inventory = AgentInventory(List(tool("cheap"), tool("mid")))
    assertEquals(sequence(inventory, 4), List("cheap", "mid", "mid", "mid"))

  test("a lone implementor degrades to the old same-runner retry"):
    val inventory = AgentInventory(List(tool("cheap")))
    assertEquals(sequence(inventory, 3), List("cheap", "cheap", "cheap"))

  test("an empty inventory still runs the runner it was handed"):
    // AgentInventory.load on a repo with no discovered runners must not silence
    // the repair entirely.
    assertEquals(sequence(AgentInventory(Nil), 2), List("cheap", "cheap"))
