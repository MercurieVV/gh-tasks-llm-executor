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

  private def arrows(
      runTaskWithRunner: TestFlow[PreparedTask, ExecutedTask],
      routeRunnerFallback: TestFlow[(PreparedTask, Throwable), Either[Throwable, PreparedTask]]
  ) = AgentRunArrows[TestFlow](
    runTaskWithRunner = runTaskWithRunner,
    routeRunnerFallback = routeRunnerFallback,
    raiseRunnerFailure = Kleisli(IO.raiseError)
  )

  test("a failed run escalates to the stronger runner exactly once"):
    Ref[IO].of(List.empty[AgentBinary]).flatMap { runners =>
      val logic = arrows(
        runTaskWithRunner = Kleisli { task =>
          val runner = task.claimedTask.runner
          runners.update(_ :+ runner.agent) *>
            (if runner.agent.value == weak.agent.value then IO.raiseError(boom)
             else IO.pure(ExecutedTask(task.claimedTask, AgentOutput("ok"))))
        },
        routeRunnerFallback = Kleisli { case (task, _) =>
          IO.pure(Right(task.copy(claimedTask = task.claimedTask.copy(runner = strong))))
        }
      )
      logic.runAgent.run(prepared(weak)).flatMap { result =>
        runners.get.map { attempted =>
          assertEquals(result.output.value, "ok")
          assertEquals(attempted, List(weak.agent, strong.agent))
        }
      }
    }

  test("with no stronger runner the failure propagates"):
    val logic = arrows(
      runTaskWithRunner = Kleisli(_ => IO.raiseError(boom)),
      routeRunnerFallback = Kleisli { case (_, error) => IO.pure(Left(error)) }
    )
    logic.runAgent.run(prepared(weak)).attempt.map(result => assertEquals(result, Left(boom)))
