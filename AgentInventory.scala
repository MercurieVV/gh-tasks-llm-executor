import cats.effect.kernel.Sync

final case class AgentTool(
    id: String,
    agent: String,
    model: Option[String],
    effort: Option[String],
    version: Option[String],
    roles: List[String],
    jobTypes: List[String],
    strengths: List[String],
    available: Boolean,
    priority: Int,
    inputUsdPerMTok: Option[Double] = None,
    outputUsdPerMTok: Option[Double] = None,
    source: Option[String] = None,
    asOfDate: Option[String] = None
):
  def cost: Option[Double] =
    for
      input <- inputUsdPerMTok.filter(_ > 0)
      output <- outputUsdPerMTok.filter(_ > 0)
    yield
      val raw = input * 0.020 + output * 0.004 * effortMultiplier
      math.round(raw * 1000.0) / 1000.0

  def runner: TaskRunner =
    TaskRunner(agent = agent, model = model, effort = effort, version = version)

  def matches(runner: TaskRunner): Boolean =
    agent.equalsIgnoreCase(runner.agent) &&
      optionMatches(model, runner.model) &&
      optionMatches(effort, runner.effort) &&
      optionMatches(version, runner.version)

  def promptLine: String =
    val modelValue = model.getOrElse("")
    val effortValue = effort.getOrElse("")
    val versionValue = version.getOrElse("")
    val roleValue = roles.mkString(",")
    val jobTypeValue = jobTypes.mkString(",")
    val strengthValue = strengths.mkString(",")
    val costValue = cost
      .map(value => f"$$$value%.3f/task (${value / 0.010}%.0fx)")
      .getOrElse("unknown")
    s"- $id: agent=$agent model=$modelValue effort=$effortValue version=$versionValue roles=$roleValue jobTypes=$jobTypeValue strengths=$strengthValue cost=$costValue priority=$priority"

  private def optionMatches(
      configured: Option[String],
      requested: Option[String]
  ): Boolean =
    (configured, requested) match
      case (_, None)                 => true
      case (Some(left), Some(right)) => left.equalsIgnoreCase(right)
      case (None, Some(_))           => false

  private def effortMultiplier: Double =
    if model.exists(_.toLowerCase.contains("reasoner")) then 2.0
    else
      effort.map(_.toLowerCase) match
        case Some("low")  => 0.5
        case Some("high") => 2.0
        case _             => 1.0

final case class AgentInventory(tools: List[AgentTool]):
  lazy val availableTools: List[AgentTool] =
    tools.filter(_.available).sortBy(_.priority)

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
    availableTools.filter(tool =>
      tool.roles.exists(_.equalsIgnoreCase("implementor"))
    )

object AgentInventory:
  private val RelativeConfigPath =
    os.rel / ".gh-tasks-llm-executor" / "agent-runners.json"

  private val Fallback = AgentInventory(
    List(
      AgentTool(
        id = "claude-opus",
        agent = "claude",
        model = Some("opus"),
        effort = None,
        version = None,
        roles = List("evaluator", "implementor"),
        jobTypes = List("scala", "planning", "debugging", "docs"),
        strengths =
          List("complex-reasoning", "broad-refactors", "failure-analysis"),
        available = true,
        priority = 100
      )
    )
  )

  def loadF[F[_]: Sync](root: os.Path): F[AgentInventory] =
    Sync[F].blocking(load(root))

  def load(root: os.Path): AgentInventory =
    val path = root / RelativeConfigPath
    if os.exists(path) then parse(os.read(path)).getOrElse(Fallback)
    else Fallback

  private def parse(value: String): Option[AgentInventory] =
    scala.util.Try {
      val json = ujson.read(value)
      val tools = json("tools").arr.toList.flatMap(parseTool)
      AgentInventory(tools)
    }.toOption

  private def parseTool(value: ujson.Value): Option[AgentTool] =
    value match
      case ujson.Obj(fields) =>
        for id <- stringField(fields, "id")
        yield AgentTool(
          id = id,
          agent = stringField(fields, "agent").getOrElse(id),
          model = stringField(fields, "model"),
          effort = stringField(fields, "effort"),
          version = stringField(fields, "version"),
          roles = stringListField(fields, "roles"),
          jobTypes = stringListField(fields, "jobTypes"),
          strengths = stringListField(fields, "strengths"),
          available = boolField(fields, "available").getOrElse(false),
          priority = intField(fields, "priority").getOrElse(1000),
          inputUsdPerMTok = positiveNumberField(fields, "inputUsdPerMTok"),
          outputUsdPerMTok = positiveNumberField(fields, "outputUsdPerMTok"),
          source = stringField(fields, "source"),
          asOfDate = stringField(fields, "asOfDate")
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

  private def intField(
      fields: collection.Map[String, ujson.Value],
      key: String
  ): Option[Int] =
    fields.get(key).collect { case ujson.Num(value) => value.toInt }

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
