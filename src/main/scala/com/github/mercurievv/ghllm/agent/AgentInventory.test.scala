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

  test("alternate implementor skips vendors marked exhausted by budget pressure") {
    val root = os.temp.dir()
    os.makeDir.all(root / ".gh-tasks-llm-executor")
    os.write.over(
      root / ".gh-tasks-llm-executor" / "vendor-budgets.json",
      ujson.write(
        ujson.Obj(
          "schemaVersion" -> 1,
          "generatedAtEpochMillis" -> System.currentTimeMillis().toString,
          "budgets" -> ujson.Arr(
            ujson.Obj(
              "vendor" -> "deepseek",
              "usedFraction" -> 0.50,
              "extra" -> ujson.Obj("currentBalanceEur" -> -0.12)
            ),
            ujson.Obj("vendor" -> "codex", "usedFraction" -> 0.10)
          )
        )
      )
    )
    os.write.over(
      root / ".gh-tasks-llm-executor" / "agent-runners.json",
      ujson.write(
        ujson.Obj(
          "tools" -> ujson.Arr(
            ujson.Obj(
              "id" -> "aider-deepseek",
              "agent" -> "aider",
              "model" -> "deepseek/deepseek-reasoner",
              "roles" -> ujson.Arr("implementor"),
              "jobTypes" -> ujson.Arr("implement"),
              "strengths" -> ujson.Arr("focused-fixes"),
              "available" -> true,
              "inputUsdPerMTok" -> 0.1,
              "outputUsdPerMTok" -> 1.0
            ),
            ujson.Obj(
              "id" -> "codex-low",
              "agent" -> "codex",
              "model" -> "gpt-5",
              "effort" -> "low",
              "roles" -> ujson.Arr("implementor"),
              "jobTypes" -> ujson.Arr("implement"),
              "strengths" -> ujson.Arr("focused-fixes"),
              "available" -> true,
              "inputUsdPerMTok" -> 1.25,
              "outputUsdPerMTok" -> 10.0
            )
          )
        )
      )
    )

    val inventory = AgentInventory.load(root)
    val failed = TaskRunner(AgentBinary("aider"), Some("deepseek/deepseek-reasoner"), None, None)

    assertEquals(inventory.defaultImplementor.map(_.agent.value), Some("codex"))
    assertEquals(inventory.alternateImplementor(failed).map(_.agent.value), Some("codex"))
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

  test("a cheap runner that burns the tokens it saved loses the break-even") {
    // The defect this fixes: with assumed volumes on both sides, c/s collapses
    // to the ratio of list prices (0.1 here), so p = 0.2 clears it. Measure the
    // volumes and the cheap runner turns out to spend 10x as many tokens for
    // the same job — its real cost equals the stronger runner's, and no success
    // rate below 1.0 justifies trying it first.
    val inventory = AgentInventory(List(cheapTool, strongerTool))
    val backend = FixedSuccessRateBackend(
      rates = Map((phase, cheapTool.runner.display) -> Some(0.2)),
      usage = Map(
        (phase, cheapTool.runner.display) -> snapshot(input = 200000, output = 40000),
        (phase, strongerTool.runner.display) -> snapshot(input = 20000, output = 4000)
      )
    )

    val selected = inventory.selectRunnerFor(
      requiredAbilities,
      preferred = Nil,
      phase = Some(phase),
      metricsBackend = Some(backend)
    )

    assertEquals(selected, Some(strongerTool.runner))
  }

  test("measured volumes keep a genuinely cheap runner cheap") {
    val inventory = AgentInventory(List(cheapTool, strongerTool))
    val backend = FixedSuccessRateBackend(
      rates = Map((phase, cheapTool.runner.display) -> Some(0.2)),
      usage = Map(
        (phase, cheapTool.runner.display) -> snapshot(input = 20000, output = 4000),
        (phase, strongerTool.runner.display) -> snapshot(input = 20000, output = 4000)
      )
    )

    val selected = inventory.selectRunnerFor(
      requiredAbilities,
      preferred = Nil,
      phase = Some(phase),
      metricsBackend = Some(backend)
    )

    assertEquals(selected, Some(cheapTool.runner))
  }

  test("one side measured is treated as neither side measured") {
    // Comparing a measured cost against a placeholder would systematically
    // favour whichever side happened to have a sample, so both fall back.
    val inventory = AgentInventory(List(cheapTool, strongerTool))
    val backend = FixedSuccessRateBackend(
      rates = Map((phase, cheapTool.runner.display) -> Some(0.2)),
      usage = Map((phase, cheapTool.runner.display) -> snapshot(input = 200000, output = 40000))
    )

    val selected = inventory.selectRunnerFor(
      requiredAbilities,
      preferred = Nil,
      phase = Some(phase),
      metricsBackend = Some(backend)
    )

    // Same verdict as before measurement existed: p = 0.2 > assumed c/s = 0.1.
    assertEquals(selected, Some(cheapTool.runner))
  }

  test("the ladder's first rung is the cheapest to run, not the cheapest per token") {
    // cheapTool has the lowest price per token but is measured to spend 50x the
    // volume on this phase, so it is the most expensive runner to actually use.
    // Ordering candidates on list price alone would put the ladder on it.
    val midTool = implementor(id = "mid", agent = "mid-agent", priceScale = 2.0, strengths = Nil)
    val inventory = AgentInventory(List(cheapTool, midTool, strongerTool))
    val backend = FixedSuccessRateBackend(
      rates = Map.empty,
      usage = Map(
        (phase, cheapTool.runner.display) -> snapshot(input = 1000000, output = 200000),
        (phase, midTool.runner.display) -> snapshot(input = 20000, output = 4000),
        (phase, strongerTool.runner.display) -> snapshot(input = 20000, output = 4000)
      )
    )

    assertEquals(inventory.cheapestImplementorToRun(phase, backend).map(_.runner), Some(midTool.runner))
  }

  test("a partial usage sample leaves the candidate ordering on the assumed volumes") {
    // Same rule as c/s: ranking one measured tool against an unmeasured one
    // compares a real number with a placeholder. Only cheapTool is measured -
    // and measured badly - yet it stays the floor, exactly as before any
    // measurement existed.
    val midTool = implementor(id = "mid", agent = "mid-agent", priceScale = 2.0, strengths = Nil)
    val inventory = AgentInventory(List(cheapTool, midTool, strongerTool))
    val backend = FixedSuccessRateBackend(
      rates = Map.empty,
      usage = Map((phase, cheapTool.runner.display) -> snapshot(input = 1000000, output = 200000))
    )

    assertEquals(inventory.cheapestImplementorToRun(phase, backend).map(_.runner), Some(cheapTool.runner))
  }

  test("cost falls back to the assumed volumes when nothing is measured") {
    assertEquals(cheapTool.costWith(None), cheapTool.cost)
    assert(cheapTool.cost.exists(_ > 0))
  }

  test("a cache read is not billed at the full input rate") {
    // Same token count, once as fresh input and once as a cache read: the read
    // must price lower or the cache is invisible to selection.
    val fresh = cheapTool.costWith(Some(snapshot(input = 100000, output = 0)))
    val cached = cheapTool.costWith(Some(snapshot(input = 0, output = 0, cacheRead = 100000)))
    assert(fresh.exists(f => cached.exists(_ < f)), s"fresh=$fresh cached=$cached")
  }

  private def snapshot(
      input: Long = 0,
      output: Long = 0,
      cacheRead: Long = 0,
      cacheWrite: Long = 0
  ): TokenUsage.TokenSnapshot =
    TokenUsage.TokenSnapshot(input, output, cacheRead, cacheWrite, input + output)

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
      rates: Map[(String, String), Option[Double]],
      usage: Map[(String, String), TokenUsage.TokenSnapshot] = Map.empty
  ) extends TokenMetrics.TokenMetricsBackend:
    override val destination: String = "fixed-success-rates"

    override def record(event: TokenMetrics.TokenMetricsEvent): Unit = ()

    override def record(event: TokenMetrics.ScalaTextToolCallEvent): Unit = ()

    override def query(
        query: TokenMetrics.TokenMetricsQuery
    ): List[TokenMetrics.TokenMetricsEvent] = Nil

    override def successRate(
        phase: String,
        runner: String,
        minSample: Int
    ): Option[Double] =
      rates.getOrElse((phase, runner), None)

    override def meanUsage(
        phase: String,
        runner: String,
        minSample: Int
    ): Option[TokenUsage.TokenSnapshot] =
      usage.get((phase, runner))

/** The ladder must go up. `nextStrongerImplementor` decides both where an escalation lands and the `s` in the
  * break-even predicate `p > c/s`, so a "stronger" runner that is cheaper than the current one does not merely misroute
  * a retry — it inverts the test that decides whether to try the cheap runner at all.
  */
class NextStrongerImplementorSuite extends munit.FunSuite:
  private def tool(
      id: String,
      agent: String,
      model: String,
      effort: Option[String],
      inPrice: Option[Double],
      outPrice: Option[Double],
      strengths: List[String] = Nil
  ) =
    AgentTool(
      id = AgentToolId(id),
      agent = Agent(agent),
      model = Some(model),
      effort = effort,
      version = None,
      roles = List("implementor"),
      jobTypes = Nil,
      strengths = strengths,
      available = Available(true),
      inputUsdPerMTok = inPrice,
      outputUsdPerMTok = outPrice
    )

  // List prices close enough to the real ones that the ordering under test is
  // the ordering the executor actually sees: haiku is cheaper than gpt-5-codex
  // at low effort, which is exactly why escalating from codex to haiku is a
  // step DOWN the ladder rather than up it.
  private val aider = tool("aider-deepseek", "aider", "deepseek/deepseek-reasoner", None, Some(0.28), Some(0.42))
  private val haiku = tool("claude-haiku", "claude", "haiku", None, Some(1.0), Some(5.0))
  private val codexLow = tool("codex-low", "codex", "gpt-5-codex", Some("low"), Some(1.25), Some(10.0))
  private val opus = tool("claude-opus", "claude", "opus", None, Some(15.0), Some(75.0), List("architecture"))

  private val inventory = AgentInventory(List(aider, codexLow, haiku, opus))

  test("escalation goes to the cheapest implementor that costs more, not to the worst-ranked one"):
    // The exact escalation observed on #130: aider must not jump past the rungs
    // above it, and must never land on a runner cheaper than where it started.
    assertEquals(inventory.nextStrongerImplementor(aider.runner), Some(haiku.runner))

  test("a mid-ladder runner escalates upward, never back down to a cheaper vendor"):
    val next = inventory.nextStrongerImplementor(codexLow.runner)
    assertEquals(next, Some(opus.runner))
    // haiku is the tool the old worst-priority rule picked here; it is cheaper.
    assert(next.exists(_ != haiku.runner))

  test("same-agent rungs count: a different effort on the same model is a real escalation"):
    val codexHigh = tool("codex-high", "codex", "gpt-5-codex", Some("high"), Some(1.25), Some(10.0))
    val laddered = AgentInventory(List(aider, codexLow, codexHigh, opus))
    assertEquals(laddered.nextStrongerImplementor(codexLow.runner), Some(codexHigh.runner))

  test("the most expensive implementor is the top of the ladder"):
    assertEquals(inventory.nextStrongerImplementor(opus.runner), None)

  test("an unpriced runner escalates to the best-ranked other implementor"):
    val unpriced = tool("mystery", "mystery", "m", None, None, None)
    val withUnpriced = AgentInventory(List(unpriced, opus))
    assertEquals(withUnpriced.nextStrongerImplementor(unpriced.runner), Some(opus.runner))

  test("a runner outside the inventory has no rung above it"):
    assertEquals(inventory.nextStrongerImplementor(TaskRunner(AgentBinary("nope"), None, None, None)), None)
