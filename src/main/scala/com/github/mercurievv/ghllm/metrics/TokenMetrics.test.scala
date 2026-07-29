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
