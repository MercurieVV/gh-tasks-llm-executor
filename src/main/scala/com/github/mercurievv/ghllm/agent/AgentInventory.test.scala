package com.github.mercurievv.ghllm.agent

import com.github.mercurievv.ghllm.*
import com.github.mercurievv.ghllm.agent.*
import com.github.mercurievv.ghllm.arrow.*
import com.github.mercurievv.ghllm.cli.*
import com.github.mercurievv.ghllm.git.*
import com.github.mercurievv.ghllm.metrics.*

import cats.effect.IO
import munit.CatsEffectSuite

class AgentInventorySuite extends CatsEffectSuite:
  private val phase = "implement"
  private val requiredAbilities = Map("complex-reasoning" -> 1.0)

  private val cheapTool = implementor(
    id = "cheap",
    agent = "cheap-agent",
    priceScale = 1.0,
    strengths = Nil
  )
  private val strongerTool = implementor(
    id = "stronger",
    agent = "stronger-agent",
    priceScale = 10.0,
    strengths = List("complex-reasoning")
  )

  test("derives task costs from configured raw prices and effort") {
    AgentInventory.loadF[IO](os.pwd).map { inventory =>
      val costs = inventory.tools.map(tool => tool.id.value -> tool.cost).toMap

      assertEquals(costs("claude-opus"), Some(0.60))
      assertEquals(costs("claude-sonnet"), Some(0.12))
      assertEquals(costs("claude-haiku"), Some(0.04))
      assertEquals(costs("codex-gpt-5-high"), Some(0.105))
      assertEquals(costs("codex-gpt-5-medium"), Some(0.065))
      assertEquals(costs("codex-gpt-5-low"), Some(0.045))
      assertEquals(costs("aider-deepseek-deepseek-chat"), Some(0.004))
      assertEquals(costs("aider-deepseek-deepseek-reasoner"), Some(0.016))
    }
  }

  test("treats missing or zero raw price fields as cost unknown") {
    val unknown = AgentTool(
      id = AgentToolId("unknown"),
      agent = Agent("unknown"),
      model = Some("unknown"),
      effort = None,
      version = None,
      roles = Nil,
      jobTypes = Nil,
      strengths = Nil,
      available = Available(true),
      inputUsdPerMTok = None,
      outputUsdPerMTok = Some(1.0)
    )

    assertEquals(unknown.cost, None)
    assert(unknown.promptLine.contains("cost=unknown"))
  }

  test("matches bare task-metadata version against full probe version string") {
    val codexTool = AgentTool(
      id = AgentToolId("codex-gpt-5-codex-medium"),
      agent = Agent("codex"),
      model = Some("gpt-5-codex"),
      effort = Some("medium"),
      version = Some("codex-cli 0.144.4"),
      roles = List("implementor"),
      jobTypes = Nil,
      strengths = Nil,
      available = Available(true)
    )
    val claudeTool = AgentTool(
      id = AgentToolId("claude-sonnet"),
      agent = Agent("claude"),
      model = Some("sonnet"),
      effort = None,
      version = Some("2.1.210 (Claude Code)"),
      roles = List("implementor"),
      jobTypes = Nil,
      strengths = Nil,
      available = Available(true)
    )

    assert(
      codexTool.matches(
        TaskRunner(
          agent = AgentBinary("codex"),
          model = Some("gpt-5-codex"),
          effort = Some("medium"),
          version = Some("0.144.4")
        )
      )
    )
    assert(
      claudeTool.matches(
        TaskRunner(
          agent = AgentBinary("claude"),
          model = Some("sonnet"),
          effort = None,
          version = Some("2.1.210")
        )
      )
    )
    assert(
      !codexTool.matches(
        TaskRunner(
          agent = AgentBinary("codex"),
          model = Some("gpt-5-codex"),
          effort = Some("medium"),
          version = Some("0.144.3")
        )
      )
    )
  }

  test("selects a cheap tool above its break-even success rate") {
    val inventory = AgentInventory(List(cheapTool, strongerTool))
    val backend = FixedSuccessRateBackend(
      Map((phase, cheapTool.runner.display) -> Some(0.2))
    )

    val selected = inventory.selectRunnerFor(
      requiredAbilities,
      preferred = Nil,
      phase = Some(phase),
      metricsBackend = Some(backend)
    )

    assertEquals(selected, Some(cheapTool.runner))
  }

  test("skips a cheap tool below break-even in favour of the stronger tool") {
    val inventory = AgentInventory(List(cheapTool, strongerTool))
    val backend = FixedSuccessRateBackend(
      Map((phase, cheapTool.runner.display) -> Some(0.05))
    )

    val selected = inventory.selectRunnerFor(
      requiredAbilities,
      preferred = Nil,
      phase = Some(phase),
      metricsBackend = Some(backend)
    )

    assertEquals(selected, Some(strongerTool.runner))
  }

  test("falls back to Priority.score ordering when the success sample is missing") {
    val inventory = AgentInventory(List(cheapTool, strongerTool))
    val backend = FixedSuccessRateBackend(Map.empty)
    val existingSelection = inventory.selectRunnerFor(requiredAbilities, preferred = Nil)

    val selected = inventory.selectRunnerFor(
      requiredAbilities,
      preferred = Nil,
      phase = Some(phase),
      metricsBackend = Some(backend)
    )

    assertEquals(selected, existingSelection)
    assertEquals(selected, Some(strongerTool.runner))
  }

  test("breakEvenRateAgainst: 20x cheaper tool yields Some(0.05)") {
    val cheap = AgentTool(
      id = AgentToolId("cheap"),
      agent = Agent("cheap"),
      model = Some("cheap"),
      effort = None,
      version = None,
      roles = List("implementor"),
      jobTypes = Nil,
      strengths = Nil,
      available = Available(true),
      inputUsdPerMTok = Some(1.0),
      outputUsdPerMTok = Some(0.0001),
      asOfDate = Some("2035-01-01")
    )
    val strong = AgentTool(
      id = AgentToolId("strong"),
      agent = Agent("strong"),
      model = Some("strong"),
      effort = None,
      version = None,
      roles = List("implementor"),
      jobTypes = Nil,
      strengths = Nil,
      available = Available(true),
      inputUsdPerMTok = Some(20.0),
      outputUsdPerMTok = Some(0.0001),
      asOfDate = Some("2035-01-01")
    )
    val breakEven = cheap.breakEvenRateAgainst(strong)
    assert(breakEven.isDefined)
    // tolerate floating‑point imprecision
    val tol = 1e-9
    val v = breakEven.get
    assert(math.abs(v - 0.05) < tol, s"expected 0.05 within $tol, got $v")
  }

  test("breakEvenRateAgainst: 2x cheaper tool yields Some(0.5)") {
    val cheap = AgentTool(
      id = AgentToolId("cheap"),
      agent = Agent("cheap"),
      model = Some("cheap"),
      effort = None,
      version = None,
      roles = List("implementor"),
      jobTypes = Nil,
      strengths = Nil,
      available = Available(true),
      inputUsdPerMTok = Some(10.0),
      outputUsdPerMTok = Some(0.0001),
      asOfDate = Some("2035-01-01")
    )
    val strong = AgentTool(
      id = AgentToolId("strong"),
      agent = Agent("strong"),
      model = Some("strong"),
      effort = None,
      version = None,
      roles = List("implementor"),
      jobTypes = Nil,
      strengths = Nil,
      available = Available(true),
      inputUsdPerMTok = Some(20.0),
      outputUsdPerMTok = Some(0.0001),
      asOfDate = Some("2035-01-01")
    )
    assertEquals(cheap.breakEvenRateAgainst(strong), Some(0.5))
  }

  test("breakEvenRateAgainst: more expensive tool clamps to Some(1.0)") {
    val expensive = AgentTool(
      id = AgentToolId("expensive"),
      agent = Agent("expensive"),
      model = Some("expensive"),
      effort = None,
      version = None,
      roles = List("implementor"),
      jobTypes = Nil,
      strengths = Nil,
      available = Available(true),
      inputUsdPerMTok = Some(20.0),
      outputUsdPerMTok = Some(0.0001),
      asOfDate = Some("2035-01-01")
    )
    val strong = AgentTool(
      id = AgentToolId("strong"),
      agent = Agent("strong"),
      model = Some("strong"),
      effort = None,
      version = None,
      roles = List("implementor"),
      jobTypes = Nil,
      strengths = Nil,
      available = Available(true),
      inputUsdPerMTok = Some(10.0),
      outputUsdPerMTok = Some(0.0001),
      asOfDate = Some("2035-01-01")
    )
    assertEquals(expensive.breakEvenRateAgainst(strong), Some(1.0))
  }

  test("breakEvenRateAgainst: tool with no cost yields None") {
    val unpriced = AgentTool(
      id = AgentToolId("unpriced"),
      agent = Agent("unpriced"),
      model = None,
      effort = None,
      version = None,
      roles = Nil,
      jobTypes = Nil,
      strengths = Nil,
      available = Available(true),
      inputUsdPerMTok = None,
      outputUsdPerMTok = None,
      asOfDate = None
    )
    val strong = AgentTool(
      id = AgentToolId("strong"),
      agent = Agent("strong"),
      model = Some("strong"),
      effort = None,
      version = None,
      roles = List("implementor"),
      jobTypes = Nil,
      strengths = Nil,
      available = Available(true),
      inputUsdPerMTok = Some(10.0),
      outputUsdPerMTok = Some(0.0001),
      asOfDate = Some("2035-01-01")
    )
    assertEquals(unpriced.breakEvenRateAgainst(strong), None)
  }

  private def implementor(
      id: String,
      agent: String,
      priceScale: Double,
      strengths: List[String]
  ): AgentTool =
    AgentTool(
      id = AgentToolId(id),
      agent = Agent(agent),
      model = Some(id),
      effort = None,
      version = None,
      roles = List("implementor"),
      jobTypes = Nil,
      strengths = strengths,
      available = Available(true),
      inputUsdPerMTok = Some(priceScale),
      outputUsdPerMTok = Some(priceScale)
    )

  private final case class FixedSuccessRateBackend(
      rates: Map[(String, String), Option[Double]]
  ) extends TokenMetrics.TokenMetricsBackend:
    override val destination: String = "fixed-success-rates"

    override def record(event: TokenMetrics.TokenMetricsEvent): Unit = ()

    override def query(
        query: TokenMetrics.TokenMetricsQuery
    ): List[TokenMetrics.TokenMetricsEvent] = Nil

    override def successRate(
        phase: String,
        runner: String,
        minSample: Int
    ): Option[Double] =
      rates.getOrElse((phase, runner), None)
