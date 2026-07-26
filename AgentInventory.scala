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
  def cost: Option[Double] =
    for
      input <- inputUsdPerMTok.filter(_ > 0)
      output <- outputUsdPerMTok.filter(_ > 0)
      if !isPriceStale
    yield
      val raw = input * 0.020 + output * 0.004 * effortMultiplier
      math.round(raw * 1000.0) / 1000.0

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

  // The single place priority is computed: right before an agent gets
  // selected/run (sorting, fallback, escalation, and prompt display all read
  // this). Tier is derived from `strengths` (not a second hand-picked
  // switch) and cost comes straight from the raw price fields, so there is
  // exactly one implementation of "how good" and "how cheap" to keep in sync.
  def priority: Int =
    val vendorCoef = (if agent == Agent("claude") then 1 else 0) * 10000
    val costRank = cost.map(value => math.round(value * 10000.0).toInt).getOrElse(500000)
    // Scaled well below the 1,000,000-wide tier gap so a saturated vendor
    // sinks to the bottom of its own tier (loses ties, tried last) but never
    // gets bumped into a worse tier - a heavily-used vendor is still
    // preferable to an incapable one.
    val pressureRank = budgetPressure
      .map(value => math.round(math.min(1.0, math.max(0.0, value)) * AgentTool.BudgetPressureScale).toInt)
      .getOrElse(0)
    tier * 1000000 + costRank + pressureRank + vendorCoef

  private def tier: Int =
    val markers = strengths.map(_.toLowerCase).toSet
    val highTierMarkers =
      Set("complex-reasoning", "deep-code-reasoning", "architecture", "evaluation")
    val midTierMarkers = Set("source-of-truth", "refactoring", "focused-fixes")
    if markers.exists(highTierMarkers.contains) then 0
    else if markers.exists(midTierMarkers.contains) then 1
    else 2

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
    s"- $id: agent=$agent model=$modelValue effort=$effortValue version=$versionValue roles=$roleValue jobTypes=$jobTypeValue strengths=$strengthValue cost=$costValue budget=$budgetValue priority=$priority"

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

  // Max cost-rank observed in practice is a few tens of thousands (see
  // `priority`); keeping the pressure penalty in that same range means a
  // saturated vendor loses tiebreaks against cheaper-but-similarly-priced
  // tools without ever crossing a tier boundary on its own.
  val BudgetPressureScale = 20000.0

  // Past this fraction of a vendor's budget consumed, treat the tool as
  // unavailable outright rather than merely deprioritized - avoids kicking
  // off a task on a vendor that will run out mid-task.
  val HardExhaustionThreshold = 0.97

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

final case class AgentInventory(tools: List[AgentTool]):
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

  def nextStrongerImplementor(runner: TaskRunner): Option[TaskRunner] =
    val implementors = availableImplementors
    implementors
      .find(_.matches(runner))
      .flatMap(current =>
        implementors
          .filter(tool => tool.priority < current.priority)
          .sortBy(_.priority)
          .lastOption
          .map(_.runner)
      )

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
    if os.exists(path) then parse(os.read(path), pressures).getOrElse(Fallback)
    else Fallback

  private def loadVendorPressures(root: os.Path): Map[String, Double] =
    val path = root / RelativeBudgetsPath
    if os.exists(path) then parseVendorBudgets(os.read(path)) else Map.empty

  private def parseVendorBudgets(value: String): Map[String, Double] =
    scala.util.Try {
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
            yield vendor.toLowerCase -> usedFraction
          }
          .toMap
    }.getOrElse(Map.empty)

  private def parse(value: String, pressures: Map[String, Double]): Option[AgentInventory] =
    scala.util.Try {
      val json = ujson.read(value)
      val tools = json("tools").arr.toList.flatMap(parseTool(_, pressures))
      AgentInventory(tools)
    }.toOption

  private def parseTool(value: ujson.Value, pressures: Map[String, Double]): Option[AgentTool] =
    value match
      case ujson.Obj(fields) =>
        for id <- stringField(fields, "id")
        yield
          val agent = stringField(fields, "agent").getOrElse(id)
          val model = stringField(fields, "model")
          val vendorKey = AgentTool.vendorKeyFor(agent, model)
          val budgetPressure = pressures.get(vendorKey)
          val jsonAvailable = boolField(fields, "available").getOrElse(false)
          val hardExhausted = budgetPressure.exists(_ >= AgentTool.HardExhaustionThreshold)
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
