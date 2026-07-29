package com.github.mercurievv.ghllm

import com.github.mercurievv.ghllm.agent.*
import com.github.mercurievv.ghllm.arrow.*
import com.github.mercurievv.ghllm.cli.*
import com.github.mercurievv.ghllm.git.*
import com.github.mercurievv.ghllm.metrics.*

// The single place runner priority is computed (see AgentTool.priority /
// AgentInventory.selectRunnerFor). Kept as its own root-level file, separate
// from AgentInventory.scala's data model, so the formula and its tunables are
// easy to find and retune without wading through parsing/matching code.
//
// Callers sort ascending and take the first/min result everywhere (ties in
// AgentTool.priority, sortBy(_.priority), Priority.score): LOWER IS BETTER.
// A tool that best fits the required abilities, is cheapest, and is under the
// least budget pressure gets the smallest number and is picked first.
final case class PriorityWeights(
                                  // Gap between capability tiers / unmet-ability penalty steps. Must stay
                                  // well above the max plausible cost+pressure+vendor sum for a tier/step
                                  // to never leak into its neighbor - see costRank's unknown-cost fallback.
                                  bandSize: Long = 1_000_000L,
                                  budgetPressureScale: Double = 20000.0,
                                  hardExhaustionThreshold: Double = 0.97,
                                  vendorBonus: Long = 10000L,
                                  unPreferredVendor: String = "claude"
)

object PriorityWeights:
  val Default: PriorityWeights = PriorityWeights()

  private val RelativeConfigPath =
    os.rel / ".gh-tasks-llm-executor" / "priority-weights.json"

  // Optional, run-time-tunable: absence (or a parse failure) reproduces
  // today's hardcoded constants exactly, so this is a strict opt-in - editing
  // or dropping the file retunes selection on the *next* run without
  // touching any already-evaluated task's metadata.
  def load(root: os.Path): PriorityWeights =
    val path = root / RelativeConfigPath
    if !os.exists(path) then Default
    else parse(os.read(path)).getOrElse(Default)

  private def parse(value: String): Option[PriorityWeights] =
    scala.util.Try {
      val json = ujson.read(value)
      PriorityWeights(
        bandSize = numField(json, "bandSize").map(_.toLong).getOrElse(Default.bandSize),
        budgetPressureScale = numField(json, "budgetPressureScale").getOrElse(Default.budgetPressureScale),
        hardExhaustionThreshold =
          numField(json, "hardExhaustionThreshold").getOrElse(Default.hardExhaustionThreshold),
        vendorBonus = numField(json, "vendorBonus").map(_.toLong).getOrElse(Default.vendorBonus),
        unPreferredVendor = strField(json, "preferredVendor").getOrElse(Default.unPreferredVendor)
      )
    }.toOption

  private def numField(json: ujson.Value, key: String): Option[Double] =
    json.obj.get(key).collect { case ujson.Num(v) => v }

  private def strField(json: ujson.Value, key: String): Option[String] =
    json.obj.get(key).collect { case ujson.Str(v) => v }.filter(_.nonEmpty)

object Priority:
  // Legacy, marker-based capability tier - used only when a task carries no
  // "Required abilities/importance" metadata (older tasks, or tasks that
  // never needed anything beyond the coarse tiering). 0 = highest tier.
  private val HighTierMarkers =
    Set("complex-reasoning", "deep-code-reasoning", "architecture", "evaluation")
  private val MidTierMarkers = Set("source-of-truth", "refactoring", "focused-fixes")

  private def legacyTier(tool: AgentTool): Long =
    val markers = tool.strengths.map(_.toLowerCase).toSet
    if markers.exists(HighTierMarkers.contains) then 0L
    else if markers.exists(MidTierMarkers.contains) then 1L
    else 2L

  // Sum of importance coefficients for required abilities this tool does NOT
  // advertise via strengths/jobTypes (case-insensitive). 0 when the tool
  // covers every required ability - such a tool always outranks one that
  // misses even a single required ability, however cheap it is, because this
  // value is scaled by `bandSize` before cost/pressure/vendor are added.
  private def unmetAbilityWeight(tool: AgentTool, requirements: Map[String, Double]): Double =
    val advertised = (tool.strengths ++ tool.jobTypes).map(_.toLowerCase).toSet
    requirements.collect {
      case (ability, coefficient) if !advertised.contains(ability.toLowerCase) => coefficient
    }.sum

  // The one implementation of "how good, how cheap, how safe to use right
  // now" a runner is; every ranking/selection call site reads this. Lower
  // result = preferred (see file-level note). `requirements` empty falls back
  // to the coarse legacy tier for tasks that never posted ability metadata.
  def score(tool: AgentTool, requirements: Map[String, Double], weights: PriorityWeights): Long =
    // Bands: an integer count for the legacy tier, or the (typically small,
    // e.g. 0.0-3.0) sum of unmet-ability coefficients - either way, each whole
    // band costs one `bandSize` and dominates every cost/pressure/vendor term
    // below it, so capability fit always outranks price.
    val bands: Double =
      if requirements.isEmpty then legacyTier(tool).toDouble
      else unmetAbilityWeight(tool, requirements)
    val tierComponent = math.round(bands * weights.bandSize.toDouble)
    val vendorCoef = (if tool.agent.value.equalsIgnoreCase(weights.unPreferredVendor) then 1 else 0) * weights.vendorBonus
    val costRank = tool.cost.map(value => math.round(value * 10000.0)).getOrElse(500000L)
    val pressureRank = tool.budgetPressure
      .map(value => math.round(math.min(1.0, math.max(0.0, value)) * weights.budgetPressureScale))
      .getOrElse(0L)
    tierComponent + costRank + pressureRank + vendorCoef
