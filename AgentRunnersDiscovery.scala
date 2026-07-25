import scala.util.Try

// In-process copy of scripts/discover-agent-runners.scala, following the same
// split as VendorBudgets: `project.scala` has `//> using exclude scripts`, so
// nothing under scripts/ is ever part of this compiled binary. That script
// stays the manual/documented probe (`scala-cli run
// scripts/discover-agent-runners.scala`, see README); this object is the
// in-process copy resolveContext calls for the TTL-gated auto-refresh. Keep
// the two in sync by hand if the probe logic changes.
object AgentRunnersDiscovery:
  final case class ModelPrice(
      inputUsdPerMTok: Double,
      outputUsdPerMTok: Double,
      source: String,
      asOfDate: String
  )

  final case class Probe(
      name: String,
      path: Option[String],
      version: Option[String]
  ):
    val available: Boolean = path.nonEmpty

  val RelativePath: os.RelPath = os.rel / ".gh-tasks-llm-executor" / "agent-runners.json"
  val PricesRelativePath: os.RelPath = os.rel / ".gh-tasks-llm-executor" / "model-prices.json"

  // How long ago the on-disk snapshot was generated, if it exists and parses.
  def ageMillis(root: os.Path): Option[Long] =
    Try {
      val json = ujson.read(os.read(root / RelativePath))
      val generatedAt = field(json, "generatedAtEpochMillis")
        .flatMap(value =>
          value.strOpt
            .flatMap(text => Try(text.toLong).toOption)
            .orElse(value.numOpt.map(_.toLong))
        )
        .getOrElse(0L)
      System.currentTimeMillis() - generatedAt
    }.toOption

  // Never throws - a probe failure degrades to "no runners collected" rather
  // than blocking whatever called it; AgentInventory falls back to a single
  // claude/opus runner when the file is absent or empty (see README).
  def collectAndWrite(root: os.Path): Unit =
    Try {
      val configDir = root / ".gh-tasks-llm-executor"
      val outputPath = root / RelativePath
      val modelPrices = readModelPrices(root / PricesRelativePath)
      val claude = probe("claude")
      val codex = probe("codex")
      val aider = probe("aider")

      val claudeModels =
        envList("AGENT_RUNNER_CLAUDE_MODELS", List("opus", "sonnet", "haiku"))
      val codexModels =
        envList("AGENT_RUNNER_CODEX_MODELS", List("gpt-5", "gpt-5-codex"))
      val codexEfforts =
        envList("AGENT_RUNNER_CODEX_EFFORTS", List("high", "medium", "low"))
      val aiderDeepseekModels = envList(
        "AGENT_RUNNER_AIDER_DEEPSEEK_MODELS",
        List("deepseek/deepseek-chat", "deepseek/deepseek-reasoner")
      )

      val claudeTools =
        claudeModels.map { model =>
          val strengths = model.toLowerCase match
            case "opus" =>
              List(
                "evaluation",
                "complex-reasoning",
                "architecture",
                "failure-analysis",
                "plan",
                "source-of-truth"
              )
            case "sonnet" =>
              List(
                "scala-code",
                "debugging",
                "refactoring",
                "docs",
                "source-of-truth",
                "implement"
              )
            case "haiku" =>
              List("small-edits", "docs", "mechanical-changes", "implement", "test")
            case _ =>
              List("scala-code", "implement")
          val price = modelPrices.get(priceKey("claude", model))
          tool(
            id = s"claude-$model",
            agent = "claude",
            model = model,
            effort = None,
            version = claude.version,
            roles =
              if model == "opus" then List("evaluator", "implementor")
              else List("implementor"),
            jobTypes = List(
              "scala",
              "tests",
              "docs",
              "github-issues",
              "plan",
              "source-of-truth",
              "implement",
              "test"
            ),
            strengths = strengths,
            available = claude.available,
            probe = claude,
            price = price
          )
        }

      val codexTools =
        for
          model <- codexModels
          effort <- codexEfforts
        yield
          val strengths =
            if effort == "high" then
              List(
                "deep-code-reasoning",
                "multi-file-edits",
                "tests",
                "plan",
                "source-of-truth",
                "implement"
              )
            else if effort == "medium" then List("scala-code", "focused-fixes", "tests", "implement", "test")
            else List("small-edits", "mechanical-changes", "implement", "test")
          val price = modelPrices.get(priceKey("codex", model))
          tool(
            id = s"codex-$model-$effort",
            agent = "codex",
            model = model,
            effort = Some(effort),
            version = codex.version,
            roles = List("implementor"),
            jobTypes = List(
              "scala",
              "tests",
              "repo-editing",
              "debugging",
              "plan",
              "source-of-truth",
              "implement",
              "test"
            ),
            strengths = strengths,
            available = codex.available,
            probe = codex,
            price = price
          )

      val aiderTools =
        aiderDeepseekModels.map { model =>
          val strengths =
            if model.contains("reasoner") then
              List(
                "complex-reasoning",
                "scala-code",
                "debugging",
                "plan",
                "source-of-truth",
                "implement"
              )
            else
              List(
                "scala-code",
                "focused-fixes",
                "mechanical-changes",
                "implement",
                "test"
              )
          val price = modelPrices.get(priceKey("aider", model))
          tool(
            id = s"aider-${model.replace('/', '-')}",
            agent = "aider",
            model = model,
            effort = None,
            version = aider.version,
            roles = List("implementor"),
            jobTypes = List(
              "scala",
              "tests",
              "repo-editing",
              "debugging",
              "plan",
              "source-of-truth",
              "implement",
              "test"
            ),
            strengths = strengths,
            available = aider.available,
            probe = aider,
            price = price
          )
        }

      val json = ujson.Obj(
        "schemaVersion" -> 1,
        "generatedBy" -> "AgentRunnersDiscovery.collectAndWrite (in-process auto-refresh)",
        "generatedAtEpochMillis" -> System.currentTimeMillis(),
        "metadataFormat" -> "preferred llms/models/efforts/versions",
        "tools" -> (claudeTools ++ codexTools ++ aiderTools)
      )

      os.makeDir.all(configDir)
      os.write.over(outputPath, ujson.write(json, indent = 2) + "\n")
    }
    ()

  private def field(json: ujson.Value, key: String): Option[ujson.Value] =
    json.objOpt.flatMap(_.get(key))

  private def commandOutput(command: Seq[String]): Option[String] =
    Try(
      os.proc(command)
        .call(stdout = os.Pipe, stderr = os.Pipe, check = false)
        .out
        .text()
        .trim
    ).toOption.filter(_.nonEmpty)

  private def commandPath(command: String): Option[String] =
    commandOutput(Seq("/bin/sh", "-lc", s"command -v $command"))

  private def probe(command: String): Probe =
    Probe(
      name = command,
      path = commandPath(command),
      version = commandOutput(Seq(command, "--version"))
        .orElse(commandOutput(Seq(command, "version")))
    )

  private def envList(name: String, fallback: List[String]): List[String] =
    sys.env
      .get(name)
      .map(_.split(",").toList.map(_.trim).filter(_.nonEmpty))
      .filter(_.nonEmpty)
      .getOrElse(fallback)

  private def priceKey(agent: String, model: String): (String, String) =
    (agent.toLowerCase, model.toLowerCase)

  private def readModelPrices(path: os.Path): Map[(String, String), ModelPrice] =
    Try {
      ujson
        .read(os.read(path))
        .obj
        .get("prices")
        .toList
        .flatMap(_.arr.toList)
        .flatMap { value =>
          for
            agent <- value.obj.get("agent").collect { case ujson.Str(name) => name }
            model <- value.obj.get("model").collect { case ujson.Str(name) => name }
            input <- value.obj
              .get("inputUsdPerMTok")
              .collect { case ujson.Num(amount) if amount > 0 => amount }
            output <- value.obj
              .get("outputUsdPerMTok")
              .collect { case ujson.Num(amount) if amount > 0 => amount }
            source <- value.obj.get("source").collect { case ujson.Str(name) => name }
            asOfDate <- value.obj.get("asOfDate").collect { case ujson.Str(date) => date }
          yield priceKey(agent, model) -> ModelPrice(input, output, source, asOfDate)
        }
    }.getOrElse(Nil).toMap

  private def tool(
      id: String,
      agent: String,
      model: String,
      effort: Option[String],
      version: Option[String],
      roles: List[String],
      jobTypes: List[String],
      strengths: List[String],
      available: Boolean,
      probe: Probe,
      price: Option[ModelPrice]
  ): ujson.Obj =
    val inputUsdPerMTok = price.fold[ujson.Value](ujson.Null)(value => ujson.Num(value.inputUsdPerMTok))
    val outputUsdPerMTok = price.fold[ujson.Value](ujson.Null)(value => ujson.Num(value.outputUsdPerMTok))
    val source = price.fold[ujson.Value](ujson.Null)(value => ujson.Str(value.source))
    val asOfDate = price.fold[ujson.Value](ujson.Null)(value => ujson.Str(value.asOfDate))
    ujson.Obj(
      "id" -> id,
      "agent" -> agent,
      "model" -> model,
      "effort" -> effort.fold[ujson.Value](ujson.Null)(ujson.Str(_)),
      "version" -> version.fold[ujson.Value](ujson.Null)(ujson.Str(_)),
      "roles" -> roles.map(ujson.Str(_)),
      "jobTypes" -> jobTypes.map(ujson.Str(_)),
      "strengths" -> strengths.map(ujson.Str(_)),
      "available" -> available,
      "inputUsdPerMTok" -> inputUsdPerMTok,
      "outputUsdPerMTok" -> outputUsdPerMTok,
      "source" -> source,
      "asOfDate" -> asOfDate,
      "probe" -> ujson.Obj(
        "command" -> probe.name,
        "path" -> probe.path.fold[ujson.Value](ujson.Null)(ujson.Str(_)),
        "version" -> probe.version.fold[ujson.Value](ujson.Null)(ujson.Str(_))
      )
    )
