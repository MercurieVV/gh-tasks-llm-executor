package com.github.mercurievv.ghllm.arrow

import com.github.mercurievv.ghllm.*
import com.github.mercurievv.ghllm.agent.*
import com.github.mercurievv.ghllm.cli.*
import com.github.mercurievv.ghllm.git.*
import com.github.mercurievv.ghllm.metrics.TokenMetrics.*
import com.github.mercurievv.ghllm.metrics.TokenUsage

import cats.data.Kleisli
import cats.effect.IO
import cats.effect.Ref
import cats.syntax.all.*
import munit.CatsEffectSuite

import scala.collection.mutable.ArrayBuffer

class PredictedVsActualExecutionSuite extends CatsEffectSuite:
  private val context = RunContext(os.pwd, AgentInventory(Nil), None)
  private def issue(number: Int, body: String = "") =
    Issue(TaskNumber(number), IssueTitle(s"Task $number"), IssueBody(body), State("open"))
  private def summary(message: String, task: Issue) =
    RunSummary(status = Status("completed"), message = Message5(message), task = Some(task))

  // In-memory TokenMetricsBackend: real queries filter by taskNumber/sinceMillis exactly like
  // the file-backed one, without touching disk.
  private class FakeBackend extends TokenMetricsBackend:
    val events = ArrayBuffer.empty[TokenMetricsEvent]
    def destination: String = "fake"
    def record(event: TokenMetricsEvent): Unit = events += event
    def query(q: TokenMetricsQuery): List[TokenMetricsEvent] = events.toList.filter(q.matches)
    def record(event: ScalaTextToolCallEvent): Unit = ()

  private def usage(input: Long, output: Long) =
    TokenUsage.TokenSnapshot(input = input, output = output, cacheRead = 0, cacheWrite = 0, total = input + output)

  test("predicted and actual are computed from the same visited tree, and pre-run history is excluded"):
    val root = issue(1, "Depends on #2")
    val dependency = issue(2)
    val costModel = TaskTree.CostModel(inputUsdPerMillionTokens = 1.0, outputUsdPerMillionTokens = 2.0)
    val backend = new FakeBackend

    // A run from before this one - must not leak into "actual", since it happened
    // outside this run's [startedAtMillis, now) window.
    backend.record(
      TokenMetricsEvent(
        timestampMillis = 0L,
        vendor = TokenUsage.Vendor.Claude,
        usage = usage(input = 999_999, output = 999_999),
        taskNumber = Some(root.number),
        model = Some("opus"),
        scope = "implement"
      )
    )

    Ref[IO].of(List.empty[TaskNumber]).flatMap { claimed =>
      val recursiveArrows = RecursiveArrows[Flow[RunF[IO]]](
        checkIfCompleted = Kleisli(node => Kleisli(_ => IO.pure(Right(node)))),
        collectPendingDependencies = Kleisli { node =>
          Kleisli { _ =>
            val pending =
              if node.issue.number === root.number then List(TaskNode(context, dependency)) else Nil
            IO.pure(DependencyPlan(node, pending, hasOpenChildren = false))
          }
        },
        recordDependencyOutcome = Kleisli(_ => Kleisli(_ => IO.pure(Right(())))),
        routeDependencyOutcome = Kleisli { case (plan, _) => Kleisli(_ => IO.pure(Right(plan.node))) },
        claimAndRun = Kleisli { node =>
          Kleisli { _ =>
            for
              nowMillis <- IO.realTime.map(_.toMillis)
              _ <- claimed.update(_ :+ node.issue.number)
              _ <- IO(
                backend.record(
                  TokenMetricsEvent(
                    timestampMillis = nowMillis,
                    vendor = TokenUsage.Vendor.Claude,
                    usage =
                      if node.issue.number === root.number then usage(input = 100, output = 10)
                      else usage(input = 40, output = 4),
                    taskNumber = Some(node.issue.number),
                    model = Some("opus"),
                    scope = "implement"
                  )
                )
              )
            yield summary("ran", node.issue)
          }
        }
      )

      for
        openIssues <- Ref[IO].of(Map.empty[TaskNumber, Issue])
        env <- RunEnv.create[IO](openIssues)
        report <- PredictedVsActualExecution
          .runWithPrediction[IO](
            TaskNode(context, root),
            recursiveArrows,
            costModel,
            backend,
            os.pwd
          )
          .run(env)
        order <- claimed.get
      yield
        assertEquals(order, List(dependency.number, root.number))
        assertEquals(report.visited, List(dependency.number, root.number))
        assertEquals(report.result, summary("ran", root))
        // (100 + 40) input @ $1/M + (10 + 4) output @ $2/M, excluding the pre-run event.
        val expectedActualUsd = (140.0 * 1.0 + 14.0 * 2.0) / 1_000_000.0
        assertEqualsDouble(report.actualUsd, expectedActualUsd, 1e-9)
    }
