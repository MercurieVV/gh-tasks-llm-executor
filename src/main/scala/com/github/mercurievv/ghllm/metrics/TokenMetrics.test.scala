package com.github.mercurievv.ghllm.metrics

import com.github.mercurievv.ghllm.*
import com.github.mercurievv.ghllm.agent.*
import com.github.mercurievv.ghllm.arrow.*
import com.github.mercurievv.ghllm.cli.*
import com.github.mercurievv.ghllm.git.*
import com.github.mercurievv.ghllm.metrics.*

class TokenMetricsSuite extends munit.FunSuite:
  test("jsonl backend persists events and filters queries"):
    val dir = os.temp.dir()
    val path = dir / "metrics.jsonl"
    val backend = TokenMetrics.JsonlTokenMetricsBackend(path)

    backend.record(
      TokenMetrics.TokenMetricsEvent(
        timestampMillis = 1000,
        vendor = TokenUsage.Vendor.Codex,
        usage = TokenUsage.TokenSnapshot(input = 10, output = 5, cacheRead = 3, cacheWrite = 2, total = 20),
        taskNumber = Some(TaskNumber(7)),
        model = Some("gpt-5"),
        scope = "agent-run"
      )
    )
    backend.record(
      TokenMetrics.TokenMetricsEvent(
        timestampMillis = 2000,
        vendor = TokenUsage.Vendor.Claude,
        usage = TokenUsage.TokenSnapshot(input = 2, output = 4, cacheRead = 0, cacheWrite = 0, total = 6),
        taskNumber = Some(TaskNumber(8)),
        model = Some("sonnet"),
        scope = "agent-run"
      )
    )

    val reloaded = TokenMetrics.JsonlTokenMetricsBackend(path)
    val codex = reloaded.query(TokenMetrics.TokenMetricsQuery(vendor = Some(TokenUsage.Vendor.Codex)))

    assertEquals(codex.map(_.taskNumber.map(_.value)), List(Some(7)))
    assertEquals(reloaded.summary(TokenMetrics.TokenMetricsQuery()).total, 26L)

  test("renderEvents provides a simple local viewer"):
    val rendered = TokenMetrics.renderEvents(
      List(
        TokenMetrics.TokenMetricsEvent(
          timestampMillis = 1000,
          vendor = TokenUsage.Vendor.Gemini,
          usage = TokenUsage.TokenSnapshot(input = 1, output = 2, cacheRead = 3, cacheWrite = 4, total = 10),
          taskNumber = None,
          model = None,
          scope = "quota"
        )
      )
    )

    assert(rendered.contains("timestampMillis vendor task model scope input output cacheRead cacheWrite total"))
    assert(rendered.contains("1000 gemini - - quota 1 2 3 4 10"))

  test("Aider output token line parses as a token snapshot"):
    val output =
      """Some earlier output
        |Tokens: 10k sent, 4.9k received. Cost: $0.0088 message, $0.02 session.
        |""".stripMargin

    assertEquals(
      TokenUsage.AiderTokenUsage.parseOutput(output),
      Some(TokenUsage.TokenSnapshot(input = 10000, output = 4900, cacheRead = 0, cacheWrite = 0, total = 14900))
    )

  test("Aider parser uses the last token line"):
    val output =
      """Tokens: 1k sent, 2k received.
        |Tokens: 12 sent, 34 received.
        |""".stripMargin

    assertEquals(
      TokenUsage.AiderTokenUsage.parseOutput(output),
      Some(TokenUsage.TokenSnapshot(input = 12, output = 34, cacheRead = 0, cacheWrite = 0, total = 46))
    )

  test("CLI parses metrics command filters"):
    val root = os.temp.dir()
    val command = Cli.parseMetricsCommand(
      List("metrics", "summary", "--vendor=codex", "--task", "12", "--since=100", "--limit=10"),
      root
    )

    assertEquals(command.map(_.path), Some(root / ".gh-tasks-llm-executor" / "token-metrics.jsonl"))
    assertEquals(command.map(_.backend), Some(TokenMetrics.BackendKind.VictoriaMetrics))
    assertEquals(command.map(_.view), Some(Cli.MetricsView.Summary))
    assertEquals(command.flatMap(_.query.vendor), Some(TokenUsage.Vendor.Codex))
    assertEquals(command.flatMap(_.query.taskNumber).map(_.value), Some(12))
    assertEquals(command.flatMap(_.query.sinceMillis), Some(100L))
    assertEquals(command.flatMap(_.query.limit), Some(10))

  test("defaultRootForWorktree maps task worktrees back to target repo root"):
    val root = os.temp.dir()
    val worktree = root / ".worktrees" / "task-12"

    assertEquals(TokenMetrics.defaultRootForWorktree(worktree), root)
    assertEquals(TokenMetrics.defaultRootForWorktree(root), root)

  test("cacheHitRatio is zero for empty query"):
    val dir = os.temp.dir()
    val path = dir / "metrics.jsonl"
    val backend = TokenMetrics.JsonlTokenMetricsBackend(path)
    assertEquals(backend.cacheHitRatio(TokenMetrics.TokenMetricsQuery()), 0.0)

  test("cacheHitRatio sums across events"):
    val dir = os.temp.dir()
    val path = dir / "metrics.jsonl"
    val backend = TokenMetrics.JsonlTokenMetricsBackend(path)

    val snap1 = TokenUsage.TokenSnapshot(input = 10, output = 0, cacheRead = 3, cacheWrite = 0, total = 13)
    val snap2 = TokenUsage.TokenSnapshot(input = 0, output = 0, cacheRead = 5, cacheWrite = 0, total = 5)

    backend.record(TokenMetrics.TokenMetricsEvent(
      timestampMillis = 1000,
      vendor = TokenUsage.Vendor.Codex,
      usage = snap1,
      taskNumber = None,
      model = None,
      scope = "test"
    ))
    backend.record(TokenMetrics.TokenMetricsEvent(
      timestampMillis = 2000,
      vendor = TokenUsage.Vendor.Codex,
      usage = snap2,
      taskNumber = None,
      model = None,
      scope = "test"
    ))

    // total input = 10, total cacheRead = 3+5=8, ratio = 8/18 ≈ 0.444...
    val expected = 8.0 / 18.0
    assertEqualsDouble(backend.cacheHitRatio(TokenMetrics.TokenMetricsQuery()), expected, 0.001)

  // ---- T08 successRate tests ---------------------------------------------------

  test("successRate returns None for sample below minSample"):
    val dir = os.temp.dir()
    val path = dir / "metrics.jsonl"
    val backend = TokenMetrics.JsonlTokenMetricsBackend(path)

    val phase = "plan"
    val runner = "haiku"

    for i <- 1 to 2 do
      backend.record(
        TokenMetrics.TokenMetricsEvent(
          timestampMillis = 1000 + i,
          vendor = TokenUsage.Vendor.Codex,
          usage = TokenUsage.TokenSnapshot.Zero,
          taskNumber = Some(TaskNumber(i)),
          model = None,
          scope = "agent-run",
          phase = Some(phase),
          runner = Some(runner),
          outcome = Some("green")
        )
      )

    assert(backend.successRate(phase, runner, minSample = 3).isEmpty)

  test("successRate returns Some(0.2) for 8 green of 40"):
    val dir = os.temp.dir()
    val path = dir / "metrics.jsonl"
    val backend = TokenMetrics.JsonlTokenMetricsBackend(path)

    val phase = "implement"
    val runner = "claude"

    for i <- 1 to 40 do
      val outcome = if i <= 8 then "green" else "red"
      backend.record(
        TokenMetrics.TokenMetricsEvent(
          timestampMillis = 1000 + i,
          vendor = TokenUsage.Vendor.Claude,
          usage = TokenUsage.TokenSnapshot.Zero,
          taskNumber = Some(TaskNumber(i)),
          model = None,
          scope = "agent-run",
          phase = Some(phase),
          runner = Some(runner),
          outcome = Some(outcome)
        )
      )

    val result = backend.successRate(phase, runner, minSample = 40)
    assert(result.isDefined)
    assertEquals(result.get, 0.2, 0.01)

  test("events with outcome=None are excluded from successRate"):
    val dir = os.temp.dir()
    val path = dir / "metrics.jsonl"
    val backend = TokenMetrics.JsonlTokenMetricsBackend(path)

    val phase = "test"
    val runner = "gpt"

    // 5 events with defined outcome (2 green, 3 red) → defined‑sample = 5, green = 2 → 0.4
    for i <- 1 to 5 do
      val outcome = if i <= 2 then "green" else "red"
      backend.record(
        TokenMetrics.TokenMetricsEvent(
          timestampMillis = 1000 + i,
          vendor = TokenUsage.Vendor.Codex,
          usage = TokenUsage.TokenSnapshot.Zero,
          taskNumber = Some(TaskNumber(i)),
          model = None,
          scope = "agent-run",
          phase = Some(phase),
          runner = Some(runner),
          outcome = Some(outcome)
        )
      )

    // 5 events with outcome = None — they must be ignored
    for i <- 6 to 10 do
      backend.record(
        TokenMetrics.TokenMetricsEvent(
          timestampMillis = 1000 + i,
          vendor = TokenUsage.Vendor.Codex,
          usage = TokenUsage.TokenSnapshot.Zero,
          taskNumber = Some(TaskNumber(i)),
          model = None,
          scope = "agent-run",
          phase = Some(phase),
          runner = Some(runner),
          outcome = None
        )
      )

    val result = backend.successRate(phase, runner, minSample = 5)
    assert(result.isDefined)
    assertEquals(result.get, 2.0 / 5.0, 0.01)
  test("jsonl round-trips measurement fields"):
    val dir = os.temp.dir()
    val path = dir / "roundtrip.jsonl"
    val backend = TokenMetrics.JsonlTokenMetricsBackend(path)

    val event = TokenMetrics.TokenMetricsEvent(
      timestampMillis = 12345L,
      vendor = TokenUsage.Vendor.Gemini,
      usage = TokenUsage.TokenSnapshot(input = 1, output = 2, cacheRead = 3, cacheWrite = 4, total = 10),
      taskNumber = Some(TaskNumber(42)),
      model = Some("my-model"),
      scope = "agent-run",
      runner = Some("my-runner"),
      turnCount = Some(8),
      escalated = true,
      outcome = Some("green")
    )

    backend.record(event)

    val reloaded = TokenMetrics.JsonlTokenMetricsBackend(path)
    val results = reloaded.query(TokenMetrics.TokenMetricsQuery())
    assertEquals(results.length, 1)

    val result = results.head
    assertEquals(result.timestampMillis, event.timestampMillis)
    assertEquals(result.vendor, event.vendor)
    assertEquals(result.usage, event.usage)
    assertEquals(result.taskNumber, event.taskNumber)
    assertEquals(result.model, event.model)
    assertEquals(result.scope, event.scope)
    assertEquals(result.runner, event.runner)
    assertEquals(result.turnCount, event.turnCount)
    assertEquals(result.escalated, event.escalated)
    assertEquals(result.outcome, event.outcome)

  test("jsonl decodes legacy lines without measurement fields"):
    val dir = os.temp.dir()
    val path = dir / "legacy.jsonl"

    val legacyLine = ujson.write(
      ujson.Obj(
        "timestampMillis" -> ujson.Num(1000.0),
        "vendor" -> "gemini",
        "taskNumber" -> ujson.Null,
        "model" -> ujson.Null,
        "scope" -> "legacy",
        "usage" -> ujson.Obj(
          "input" -> ujson.Num(10),
          "output" -> ujson.Num(20),
          "cacheRead" -> ujson.Num(0),
          "cacheWrite" -> ujson.Num(0),
          "total" -> ujson.Num(30)
        )
      )
    )

    os.write.over(path, legacyLine + System.lineSeparator())

    val backend = TokenMetrics.JsonlTokenMetricsBackend(path)
    val results = backend.query(TokenMetrics.TokenMetricsQuery())

    assertEquals(results.length, 1)

    val event = results.head
    assertEquals(event.timestampMillis, 1000L)
    assertEquals(event.vendor, TokenUsage.Vendor.Gemini)
    assertEquals(event.usage,
      TokenUsage.TokenSnapshot(input = 10, output = 20, cacheRead = 0, cacheWrite = 0, total = 30))
    assertEquals(event.scope, "legacy")

    // new fields must decode to defaults
    assertEquals(event.runner, None)
    assertEquals(event.turnCount, None)
    assertEquals(event.escalated, false)
    assertEquals(event.outcome, None)

  test("simulated failing run records outcome red"):
    val dir = os.temp.dir()
    val path = dir / "failing-run.jsonl"
    val backend = TokenMetrics.JsonlTokenMetricsBackend(path)

    backend.record(
      TokenMetrics.TokenMetricsEvent(
        timestampMillis = 5000L,
        vendor = TokenUsage.Vendor.Claude,
        usage = TokenUsage.TokenSnapshot(input = 100, output = 50, cacheRead = 0, cacheWrite = 0, total = 150),
        taskNumber = Some(TaskNumber(99)),
        model = Some("sonnet"),
        scope = "implement",
        runner = Some("claude-sonnet"),
        turnCount = Some(3),
        outcome = Some("red")
      )
    )

    val reloaded = TokenMetrics.JsonlTokenMetricsBackend(path)
    val results = reloaded.query(TokenMetrics.TokenMetricsQuery())

    assertEquals(results.length, 1)
    val event = results.head
    assertEquals(event.outcome, Some("red"))
    assertEquals(event.turnCount, Some(3))
