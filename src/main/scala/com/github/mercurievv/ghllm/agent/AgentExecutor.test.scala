package com.github.mercurievv.ghllm.agent

import com.github.mercurievv.ghllm.{AgentBinary, TaskNumber, TaskRunner}
import com.github.mercurievv.ghllm.metrics.{TokenMetrics, TokenUsage}

import cats.effect.IO
import cats.effect.unsafe.implicits.global

/** The staged event carries the phase; the map key carries the scope.
  *
  * These are separate strings on purpose and were conflated in production: the dispatch staged under `metricsScope`
  * ("implement") and the completion looked up under the task's `Phase:`, so every task whose phase was not literally
  * `implement` had its event silently dropped — no usage sample and no outcome sample, for exactly the phases
  * `successRate`/`meanUsage` need before `selectRunnerFor` can consult a measurement at all.
  */
class AgentExecutorMetricsKeySuite extends munit.FunSuite:

  private val claudeHaiku =
    TaskRunner(AgentBinary("claude"), model = Some("haiku"), effort = None, version = None)

  private def stage(
      root: os.Path,
      runner: TaskRunner,
      phase: Option[String]
  ): TokenMetrics.JsonlTokenMetricsBackend =
    val backend = TokenMetrics.JsonlTokenMetricsBackend(root / "metrics.jsonl")
    AgentExecutor.deferTokenMetrics(
      root,
      TaskNumber(7),
      runner,
      AgentExecutor.MetricsScope.Implement,
      backend,
      TokenMetrics.TokenMetricsEvent(
        timestampMillis = 1L,
        vendor = TokenUsage.Vendor.Claude,
        usage = TokenUsage.TokenSnapshot(input = 10, output = 2, cacheRead = 0, cacheWrite = 0, total = 12),
        taskNumber = Some(TaskNumber(7)),
        model = runner.model,
        scope = AgentExecutor.MetricsScope.Implement.wire,
        phase = phase,
        runner = Some(runner.display)
      )
    )
    backend

  private def recorded(backend: TokenMetrics.JsonlTokenMetricsBackend) =
    backend.query(TokenMetrics.TokenMetricsQuery())

  test("a non-implement phase still records its outcome, because the key is the scope"):
    val dir = os.temp.dir()
    val backend = stage(dir, claudeHaiku, Some("test"))

    AgentExecutor
      .completeTokenMetrics[IO](dir, TaskNumber(7), claudeHaiku, AgentExecutor.MetricsScope.Implement, "green")
      .unsafeRunSync()

    val events = recorded(backend)
    assertEquals(events.map(_.phase), List(Some("test")))
    assertEquals(events.map(_.outcome), List(Some("green")))

  test("completing under a different scope than the dispatch drops the event"):
    val dir = os.temp.dir()
    val backend = stage(dir, claudeHaiku, Some("test"))

    // The shape of the defect, pinned. The original could not be written any
    // more: production passed the task's phase, `"test"`, where a MetricsScope
    // now belongs, and that no longer compiles. A mismatched scope still loses
    // the event, so the loop this dispatch belongs to still has to be named
    // correctly - it just cannot be confused with a phase.
    AgentExecutor
      .completeTokenMetrics[IO](dir, TaskNumber(7), claudeHaiku, AgentExecutor.MetricsScope.Repair, "green")
      .unsafeRunSync()
    assertEquals(recorded(backend), Nil)

    // The staged event is still there, and the right key still finds it.
    AgentExecutor
      .completeTokenMetrics[IO](dir, TaskNumber(7), claudeHaiku, AgentExecutor.MetricsScope.Implement, "green")
      .unsafeRunSync()
    assertEquals(recorded(backend).map(_.outcome), List(Some("green")))

class AgentExecutorSuite extends munit.FunSuite:
  test("billing failures are fatal even when an agent exits successfully"):
    val output =
      """litellm.BadRequestError: DeepseekException - {"error":{"message":"Insufficient
        |Balance","type":"unknown_error","param":null,"code":"invalid_request_error"}}
        |""".stripMargin

    assertEquals(
      AgentExecutor[cats.effect.IO].terminationReason(output).map(_.toLowerCase.contains("insufficient")),
      Some(true)
    )
