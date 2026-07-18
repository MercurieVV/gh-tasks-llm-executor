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
    cost: Option[Double]
):
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

  private val TaskCosts: Map[(String, String, Option[String]), Double] = Map(
    ("claude", "opus", None) -> 0.60,
    ("claude", "sonnet", None) -> 0.12,
    ("claude", "haiku", None) -> 0.04,
    ("codex", "gpt-5", Some("high")) -> 0.105,
    ("codex", "gpt-5", Some("medium")) -> 0.065,
    ("codex", "gpt-5", Some("low")) -> 0.045,
    ("codex", "gpt-5-codex", Some("high")) -> 0.105,
    ("codex", "gpt-5-codex", Some("medium")) -> 0.065,
    ("codex", "gpt-5-codex", Some("low")) -> 0.045,
    ("aider", "deepseek/deepseek-chat", None) -> 0.010,
    ("aider", "deepseek/deepseek-reasoner", None) -> 0.029
  )

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
        priority = 100,
        cost = costFor("claude", Some("opus"), None)
      )
    )
  )

  def load[F[_]: Sync](root: os.Path): F[AgentInventory] =
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
          cost = costFor(
            stringField(fields, "agent").getOrElse(id),
            stringField(fields, "model"),
            stringField(fields, "effort")
          )
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

  private def costFor(
      agent: String,
      model: Option[String],
      effort: Option[String]
  ): Option[Double] =
    model.flatMap(modelName =>
      TaskCosts.get(
        (
          agent.toLowerCase,
          modelName.toLowerCase,
          effort.map(_.toLowerCase)
        )
      )
    )

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
