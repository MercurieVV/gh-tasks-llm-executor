package com.github.mercurievv.ghllm.agent

import com.github.mercurievv.ghllm.*
import com.github.mercurievv.ghllm.agent.*
import com.github.mercurievv.ghllm.arrow.*
import com.github.mercurievv.ghllm.cli.*
import com.github.mercurievv.ghllm.git.*
import com.github.mercurievv.ghllm.metrics.*

import cats.data.Kleisli
import cats.effect.kernel.Sync

opaque type Agent = String
object Agent:
  def apply(value: String): Agent = value
  extension (self: Agent) def value: String = self

opaque type Available = Boolean
object Available:
  def apply(value: Boolean): Available = value
  extension (self: Available) def value: Boolean = self

/** Stable id of an agent runner entry in agent-runners.json. */
opaque type AgentToolId = String
object AgentToolId:
  def apply(value: String): AgentToolId = value.asInstanceOf[AgentToolId]
  extension (opaqueValue: AgentToolId) def value: String = opaqueValue.asInstanceOf[String]
  given cats.Eq[AgentToolId] = cats.Eq.by(_.value)

final case class AgentTool(
    id: AgentToolId,
    agent: Agent,
    model: Option[String],
    effort: Option[String],
    version: Option[String],
    roles: List[String],
    jobTypes: List[String],
    strengths: List[String],
    available: Available,
    inputUsdPerMTok: Option[Double] = None,
    outputUsdPerMTok: Option[Double] = None,
    source: Option[String] = None,
    asOfDate: Option[String] = None,
    // 0..1 fraction of this tool's vendor budget already consumed (see
    // vendor-budgets.json / discover-vendor-budgets.scala). None means no
    // budget signal was available for this vendor - never penalize what
    // isn't measured.
    budgetPressure: Option[Double] = None
):
  def cost: Option[Double] = costWith(None)

  /** Expected USD for one run of this tool.
    *
    * `usage` is the measured mean for the `(phase, runner)` this decision is
    * about ([[TokenMetrics.TokenMetricsBackend.meanUsage]]). Without it the
    * volumes fall back to `AssumedInputMTok`/`AssumedOutputMTok` — the original
    * behaviour, kept so an unmeasured tool ranks exactly as it used to.
    *
    * The distinction matters because `breakEvenRateAgainst` divides two of
    * these. With assumed volumes on both sides they cancel, and `c/s` collapses
    * to the ratio of list prices — which is only the ratio of COSTS if both
    * runners consume the same tokens to do the same job. They do not: the cheap
    * runner is cheap partly because it is smaller, and a smaller model that
    * needs more turns spends its price advantage on volume. Measuring both
    * sides is what stops the ladder from biasing down on a discount that is not
    * really there.
    */
  def costWith(usage: Option[TokenUsage.TokenSnapshot]): Option[Double] =
    for
      input <- inputUsdPerMTok.filter(_ > 0)
      output <- outputUsdPerMTok.filter(_ > 0)
      if !isPriceStale
    yield
      val inputMTok =
        usage.fold(AgentTool.AssumedInputMTok)(u =>
          // A cache write bills as input; a cache read is charged separately at
          // its own fraction, so it must not be counted at the full rate here.
          (u.input + u.cacheWrite).toDouble / AgentTool.TokensPerMillion +
            u.cacheRead.toDouble / AgentTool.TokensPerMillion * AgentTool.CacheReadPriceRatio
        )
      val outputMTok =
        usage.fold(AgentTool.AssumedOutputMTok)(_.output.toDouble / AgentTool.TokensPerMillion)
      // The effort multiplier is a prior on how much a reasoning tier writes.
      // Once output volume is measured it is already in the number, so applying
      // it again would double-count.
      val effortFactor = if usage.isDefined then 1.0 else effortMultiplier
      val raw = input * inputMTok + output * outputMTok * effortFactor
      math.round(raw * 1000.0) / 1000.0

  // Break-even success rate for trying this tool before `stronger`:
  // the ladder wins iff observed success rate p > c/s.
  // See TOKEN_EFFICIENCY_PLAN.md section 2, Stage 1.
  def breakEvenRateAgainst(stronger: AgentTool): Option[Double] =
    breakEvenRateAgainst(stronger, None, None)

  def breakEvenRateAgainst(
      stronger: AgentTool,
      thisUsage: Option[TokenUsage.TokenSnapshot],
      strongerUsage: Option[TokenUsage.TokenSnapshot]
  ): Option[Double] =
    costWith(thisUsage).flatMap { c =>
      stronger.costWith(strongerUsage).filter(_ > 0).map { s =>
        math.min(1.0, c / s)
      }
    }

  // A price pinned long ago (model repriced upstream, model-prices.json not
  // refreshed) shouldn't silently drive ranking as if it were current.
  // Past this age, cost/priority fall back to "unknown" instead of a wrong
  // number pretending to be right.
  def isPriceStale: Boolean =
    asOfDate
      .flatMap(date => scala.util.Try(java.time.LocalDate.parse(date)).toOption)
      .exists(pinned =>
        java.time.temporal.ChronoUnit.DAYS
          .between(pinned, java.time.LocalDate.now()) > AgentTool.MaxPriceAgeDays
      )

  // No-requirements priority, kept for existing sort/prompt call sites.
  // Formula, tunables, and requirements-aware scoring now live in
  // Priority.scala (see that file for the "lower is better" contract).
  def priority: Long = Priority.score(this, Map.empty, PriorityWeights.Default)

  def runner: TaskRunner =
    TaskRunner(
      agent = AgentBinary(agent.value),
      model = model,
      effort = effort,
      version = version
    )

  def matches(runner: TaskRunner): Boolean =
    agent.value.equalsIgnoreCase(runner.agent.value) &&
      optionMatches(model, runner.model) &&
      optionMatches(effort, runner.effort) &&
      versionMatches(version, runner.version)

  def promptLine: String =
    val modelValue = model.getOrElse("")
    val effortValue = effort.getOrElse("")
    val versionValue = version.getOrElse("")
    val roleValue = roles.mkString(",")
    val jobTypeValue = jobTypes.mkString(",")
    val strengthValue = strengths.mkString(",")
    val costValue =
      if isPriceStale then s"unknown (price stale, asOfDate=${asOfDate.getOrElse("?")})"
      else
        cost
          .map(value => f"$$$value%.3f/task (${value / 0.010}%.0fx)")
          .getOrElse("unknown")
    val budgetValue = budgetPressure.map(value => f"${value * 100}%.0f%% used").getOrElse("unknown")
    s"- $id: agent=$agent model=$modelValue effort=$effortValue version=$versionValue roles=$roleValue jobTypes=$jobTypeValue strengths=$strengthValue cost=$costValue budget=$budgetValue"

  private def optionMatches(
      configured: Option[String],
      requested: Option[String]
  ): Boolean =
    (configured, requested) match
      case (_, None)                 => true
      case (Some(left), Some(right)) => left.equalsIgnoreCase(right)
      case (None, Some(_))           => false

  // Configured versions carry full probe strings (e.g. "codex-cli 0.144.4",
  // "2.1.210 (Claude Code)") while task metadata records a bare version
  // (e.g. "0.144.4", "2.1.210"), so an exact match would never fire.
  private def versionMatches(
      configured: Option[String],
      requested: Option[String]
  ): Boolean =
    (configured, requested) match
      case (_, None) => true
      case (Some(left), Some(right)) =>
        val l = left.toLowerCase
        val r = right.toLowerCase
        l.contains(r) || r.contains(l)
      case (None, Some(_)) => false

  private def effortMultiplier: Double =
    if model.exists(_.toLowerCase.contains("reasoner")) then 2.0
    else
      effort.map(_.toLowerCase) match
        case Some("low")  => 0.5
        case Some("high") => 2.0
        case _            => 1.0

object AgentTool:
  val MaxPriceAgeDays = 180

  val TokensPerMillion = 1000000.0

  /** Volumes used when a (phase, runner) has no measurement yet. These were the
    * only volumes the cost model ever had; they are a prior now, not the model.
    */
  val AssumedInputMTok = 0.020
  val AssumedOutputMTok = 0.004

  /** Cache reads bill at a fraction of the input rate. Kept here rather than
    * read from `TaskTree.CostModel` so pricing a runner does not depend on the
    * plan-estimation module; the two must agree, and this is the value to change
    * if a vendor's ratio moves.
    */
  val CacheReadPriceRatio = 0.1

  // aider is a multi-vendor runner (its `agent` field is always "aider");
  // the actual vendor being billed is encoded in the model id.
  def vendorKeyFor(agent: String, model: Option[String]): String =
    val agentLower = agent.toLowerCase
    if agentLower == "aider" then
      model.map(_.toLowerCase) match
        case Some(m) if m.contains("deepseek") => "deepseek"
        case Some(m)                           => m.takeWhile(_ != '/')
        case None                              => agentLower
    else agentLower

final case class AgentInventory(tools: List[AgentTool], weights: PriorityWeights = PriorityWeights.Default):
  // Ascending: lower AgentTool.priority sorts first (see Priority.scala) - so
  // this list is already best-first, and `.headOption` everywhere below picks
  // the preferred tool, not an arbitrary one.
  lazy val availableTools: List[AgentTool] =
    tools.filter(_.available.value).sortBy(_.priority)

  def defaultImplementor: Option[TaskRunner] =
    availableTools
      .filter(tool => tool.roles.exists(_.equalsIgnoreCase("implementor")))
      .headOption
      .map(_.runner)

  def selectRunner(preferred: List[TaskRunner]): Option[TaskRunner] =
    preferred
      .flatMap(runner => availableTools.find(_.matches(runner)).map(_.runner))
      .headOption
      .orElse(defaultImplementor)

  // Run-time runner choice: an explicit pinned `preferred` runner (evaluator
  // wrote a concrete agent/model/effort/version, e.g. to reproduce a bug tied
  // to one model) always wins outright. Otherwise, when the task instead
  // carries abstract `requiredAbilities` (ability -> importance coefficient),
  // use observed phase/runner success when it is supplied and fully measured.
  // Missing price or sample data falls back wholesale to Priority.score using
  // this inventory's (possibly user-tuned) `weights`. With neither task signal,
  // fall back to `defaultImplementor` exactly as before.
  def selectRunnerFor(
      requiredAbilities: Map[String, Double],
      preferred: List[TaskRunner]
  ): Option[TaskRunner] =
    selectRunnerFor(requiredAbilities, preferred, None, None)

  def selectRunnerFor(
      requiredAbilities: Map[String, Double],
      preferred: List[TaskRunner],
      phase: Option[String],
      metricsBackend: Option[TokenMetrics.TokenMetricsBackend]
  ): Option[TaskRunner] =
    if preferred.nonEmpty then selectRunner(preferred)
    else if requiredAbilities.nonEmpty then
      val priorityFallback = availableImplementors
        .sortBy(tool => Priority.score(tool, requiredAbilities, weights))
        .headOption
      val measuredSelection =
        for
          phaseName <- phase
          backend <- metricsBackend
          candidate <- availableImplementors
            .sortBy(_.cost.getOrElse(Double.PositiveInfinity))
            .headOption
          nextStrongerRunner <- nextStrongerImplementor(candidate.runner)
          nextStronger <- availableImplementors.find(_.matches(nextStrongerRunner))
          // Measured volumes on BOTH sides or neither: mixing a measured cheap
          // runner with an assumed strong one would compare a real cost against
          // a placeholder and systematically favour whichever side is measured.
          candidateUsage = backend.meanUsage(phaseName, candidate.runner.display)
          strongerUsage = backend.meanUsage(phaseName, nextStronger.runner.display)
          bothMeasured = candidateUsage.isDefined && strongerUsage.isDefined
          breakEvenRate <- candidate.breakEvenRateAgainst(
            nextStronger,
            if bothMeasured then candidateUsage else None,
            if bothMeasured then strongerUsage else None
          )
          observedSuccessRate <- backend.successRate(phaseName, candidate.runner.display)
        yield
          if observedSuccessRate > breakEvenRate then candidate
          else nextStronger

      measuredSelection.orElse(priorityFallback).map(_.runner)
    else defaultImplementor

  def nextStrongerImplementor(runner: TaskRunner): Option[TaskRunner] =
    val implementors = availableImplementors
    implementors
      .find(_.matches(runner))
      .flatMap(current =>
        implementors
          .filter(_.agent != current.agent)
          .sortBy(_.priority)
          .lastOption
          .map(_.runner)
      )

  def alternateImplementor(
      runner: TaskRunner,
      alsoExclude: List[TaskRunner] = Nil
  ): Option[TaskRunner] =
    val excluded = runner :: alsoExclude
    availableImplementors
      .filterNot(tool => excluded.exists(tool.matches))
      .headOption
      .map(_.runner)

  def promptBlock: String =
    val lines = availableTools.map(_.promptLine)
    if lines.isEmpty then
      "No available local implementor tools were discovered. Use claude/opus if no better runner is available."
    else lines.mkString("\n")

  private def availableImplementors: List[AgentTool] =
    availableTools.filter(tool => tool.roles.exists(_.equalsIgnoreCase("implementor")))

object AgentInventory:
  private val RelativeConfigPath =
    os.rel / ".gh-tasks-llm-executor" / "agent-runners.json"
  private val RelativeBudgetsPath =
    os.rel / ".gh-tasks-llm-executor" / "vendor-budgets.json"

  // Budget state moves fast (a session can saturate in minutes), unlike
  // pricing. Past this age the snapshot is more likely wrong than useful,
  // so fall back to "no signal" instead of steering on stale pressure.
  private val MaxBudgetAgeMillis = 6L * 60 * 60 * 1000

  private val Fallback = AgentInventory(
    List(
      AgentTool(
        id = AgentToolId("claude-opus"),
        agent = Agent("claude"),
        model = Some("opus"),
        effort = None,
        version = None,
        roles = List("evaluator", "implementor"),
        jobTypes = List("scala", "planning", "debugging", "docs"),
        strengths = List("complex-reasoning", "broad-refactors", "failure-analysis"),
        available = Available(true)
      )
    )
  )

  def loadF[F[_]: Sync]: Kleisli[F, os.Path, AgentInventory] =
    Kleisli.apply { root =>
      Sync[F].blocking(load(root))
    }

  def load(root: os.Path): AgentInventory =
    val path = root / RelativeConfigPath
    val pressures = loadVendorPressures(root)
    val weights = PriorityWeights.load(root)
    if os.exists(path) then parse(os.read(path), pressures, weights).getOrElse(Fallback)
    else Fallback

  private def loadVendorPressures(root: os.Path): Map[String, Double] =
    val path = root / RelativeBudgetsPath
    if os.exists(path) then parseVendorBudgets(os.read(path)) else Map.empty

  private def parseVendorBudgets(value: String): Map[String, Double] =
    scala.util
      .Try {
        val json = ujson.read(value)
        val generatedAtEpochMillis = json.obj
          .get("generatedAtEpochMillis")
          .flatMap(field =>
            field.strOpt
              .flatMap(text => scala.util.Try(text.toLong).toOption)
              .orElse(field.numOpt.map(_.toLong))
          )
          .getOrElse(0L)
        val isStale = (System.currentTimeMillis() - generatedAtEpochMillis) > MaxBudgetAgeMillis
        if isStale then Map.empty
        else
          json.obj
            .get("budgets")
            .toList
            .flatMap(_.arr.toList)
            .flatMap { entry =>
              for
                vendor <- entry.obj.get("vendor").collect { case ujson.Str(value) => value }
                usedFraction <- entry.obj.get("usedFraction").collect { case ujson.Num(value) => value }
              yield vendor.toLowerCase -> effectiveUsedFraction(entry, usedFraction)
            }
            .toMap
      }
      .getOrElse(Map.empty)

  private def effectiveUsedFraction(entry: ujson.Value, usedFraction: Double): Double =
    entry.obj
      .get("extra")
      .flatMap(_.objOpt)
      .flatMap(_.get("currentBalanceEur"))
      .flatMap(_.numOpt)
      .filter(_ <= 0.0)
      .fold(usedFraction)(_ => 1.0)

  private def parse(
      value: String,
      pressures: Map[String, Double],
      weights: PriorityWeights
  ): Option[AgentInventory] =
    scala.util.Try {
      val json = ujson.read(value)
      val tools = json("tools").arr.toList.flatMap(parseTool(_, pressures, weights))
      AgentInventory(tools, weights)
    }.toOption

  private def parseTool(
      value: ujson.Value,
      pressures: Map[String, Double],
      weights: PriorityWeights
  ): Option[AgentTool] =
    value match
      case ujson.Obj(fields) =>
        for id <- stringField(fields, "id")
        yield
          val agent = stringField(fields, "agent").getOrElse(id)
          val model = stringField(fields, "model")
          val vendorKey = AgentTool.vendorKeyFor(agent, model)
          val budgetPressure = pressures.get(vendorKey)
          val jsonAvailable = boolField(fields, "available").getOrElse(false)
          val hardExhausted = budgetPressure.exists(_ >= weights.hardExhaustionThreshold)
          AgentTool(
            id = AgentToolId(id),
            agent = Agent(agent),
            model = model,
            effort = stringField(fields, "effort"),
            version = stringField(fields, "version"),
            roles = stringListField(fields, "roles"),
            jobTypes = stringListField(fields, "jobTypes"),
            strengths = stringListField(fields, "strengths"),
            available = Available(jsonAvailable && !hardExhausted),
            inputUsdPerMTok = positiveNumberField(fields, "inputUsdPerMTok"),
            outputUsdPerMTok = positiveNumberField(fields, "outputUsdPerMTok"),
            source = stringField(fields, "source"),
            asOfDate = stringField(fields, "asOfDate"),
            budgetPressure = budgetPressure
          )
      case _ => None

  private def stringField(
      fields: collection.Map[String, ujson.Value],
      key: String
  ): Option[String] =
    fields
      .get(key)
      .collect { case ujson.Str(value) => value }
      .filter(_.nonEmpty)

  private def boolField(
      fields: collection.Map[String, ujson.Value],
      key: String
  ): Option[Boolean] =
    fields.get(key).collect { case ujson.Bool(value) => value }

  private def positiveNumberField(
      fields: collection.Map[String, ujson.Value],
      key: String
  ): Option[Double] =
    fields.get(key).collect { case ujson.Num(value) if value > 0 => value }

  private def stringListField(
      fields: collection.Map[String, ujson.Value],
      key: String
  ): List[String] =
    fields
      .get(key)
      .collect { case ujson.Arr(values) =>
        values.toList.collect {
          case ujson.Str(value) if value.nonEmpty => value
        }
      }
      .getOrElse(Nil)
