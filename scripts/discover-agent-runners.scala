//> using scala 3.8.4
//> using dep com.lihaoyi::os-lib:0.11.8
//> using dep com.lihaoyi::ujson:4.4.3

import scala.util.Try

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

def commandOutput(command: Seq[String]): Option[String] =
  Try(
    os.proc(command)
      .call(stdout = os.Pipe, stderr = os.Pipe, check = false)
      .out
      .text()
      .trim
  ).toOption.filter(_.nonEmpty)

def commandPath(command: String): Option[String] =
  commandOutput(Seq("/bin/sh", "-lc", s"command -v $command"))

def probe(command: String): Probe =
  Probe(
    name = command,
    path = commandPath(command),
    version = commandOutput(Seq(command, "--version"))
      .orElse(commandOutput(Seq(command, "version")))
  )

def envList(name: String, fallback: List[String]): List[String] =
  sys.env
    .get(name)
    .map(_.split(",").toList.map(_.trim).filter(_.nonEmpty))
    .filter(_.nonEmpty)
    .getOrElse(fallback)

def priceKey(agent: String, model: String): (String, String) =
  (agent.toLowerCase, model.toLowerCase)

def readModelPrices(path: os.Path): Map[(String, String), ModelPrice] =
  Try {
    ujson
      .read(os.read(path))
      .obj
      .get("prices")
      .toList
      .flatMap(_.arr.toList)
      .flatMap { value =>
        for
          agent <- value.obj.get("agent").collect { case ujson.Str(name) =>
            name
          }
          model <- value.obj.get("model").collect { case ujson.Str(name) =>
            name
          }
          input <- value.obj
            .get("inputUsdPerMTok")
            .collect { case ujson.Num(amount) if amount > 0 => amount }
          output <- value.obj
            .get("outputUsdPerMTok")
            .collect { case ujson.Num(amount) if amount > 0 => amount }
          source <- value.obj.get("source").collect { case ujson.Str(name) =>
            name
          }
          asOfDate <- value.obj.get("asOfDate").collect { case ujson.Str(date) =>
            date
          }
        yield priceKey(agent, model) ->
          ModelPrice(input, output, source, asOfDate)
      }
  }.getOrElse(Nil).toMap

def tool(
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
  val source =
    price.fold[ujson.Value](ujson.Null)(value => ujson.Str(value.source))
  val asOfDate =
    price.fold[ujson.Value](ujson.Null)(value => ujson.Str(value.asOfDate))
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

@main def discoverAgentRunners(): Unit =
  val root = os.pwd
  val configDir = root / ".gh-tasks-llm-executor"
  val outputPath = configDir / "agent-runners.json"
  val pricesPath = configDir / "model-prices.json"
  val modelPrices = readModelPrices(pricesPath)
  val claude = probe("claude")
  val codex = probe("codex")
  val aider = probe("aider")
  val gemini = probe("gemini")
  val agy = probe("agy")

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
  val geminiModels =
    envList("AGENT_RUNNER_GEMINI_MODELS", List("gemini-3.1-pro", "gemini-3.6-flash"))
  // agy (antigravity-cli) multiplexes several backend vendors behind one
  // binary; `agy models` is the authoritative list (effort is baked into the
  // gemini ids, absent from the rest) so ids are taken verbatim rather than
  // cross-joined with a separate efforts list like codex.
  val agyModels = envList(
    "AGENT_RUNNER_AGY_MODELS",
    List(
      "gemini-3.6-flash-high",
      "gemini-3.6-flash-medium",
      "gemini-3.6-flash-low",
      "gemini-3.5-flash-high",
      "gemini-3.5-flash-medium",
      "gemini-3.5-flash-low",
      "gemini-3.1-pro-high",
      "gemini-3.1-pro-low",
      "claude-sonnet-4-6",
      "claude-opus-4-6-thinking",
      "gpt-oss-120b-medium"
    )
  )

  val claudeTools =
    claudeModels.zipWithIndex.map { case (model, index) =>
      // Phase strengths encode the phase -> capability-tier routing table
      // (see PROJECT.md). Each runner lists only the phases it is capable of;
      // AgentInventory derives tier/priority from these plus raw price at
      // selection time, so no ranking is computed here.
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
      (model, modelIndex) <- codexModels.zipWithIndex
      (effort, effortIndex) <- codexEfforts.zipWithIndex
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
    aiderDeepseekModels.zipWithIndex.map { case (model, index) =>
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

  val geminiTools =
    geminiModels.zipWithIndex.map { case (model, index) =>
      val strengths =
        if model.contains("pro") then
          List("complex-reasoning", "scala-code", "plan", "source-of-truth", "implement")
        else List("scala-code", "focused-fixes", "implement", "test")
      val price = modelPrices.get(priceKey("gemini", model))
      tool(
        id = s"gemini-$model",
        agent = "gemini",
        model = model,
        effort = None,
        version = gemini.version,
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
        available = gemini.available,
        probe = gemini,
        price = price
      )
    }

  val agyTools =
    agyModels.zipWithIndex.map { case (model, index) =>
      val lower = model.toLowerCase
      val strengths =
        if lower.contains("opus") || lower.contains("thinking") then
          List("complex-reasoning", "architecture", "plan", "source-of-truth", "implement")
        else if lower.contains("pro") || lower.contains("sonnet") then
          List("scala-code", "debugging", "source-of-truth", "implement")
        else List("small-edits", "mechanical-changes", "implement", "test")
      val price = modelPrices.get(priceKey("agy", model))
      tool(
        id = s"agy-$model",
        agent = "agy",
        model = model,
        effort = None,
        version = agy.version,
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
        available = agy.available,
        probe = agy,
        price = price
      )
    }

  val json = ujson.Obj(
    "schemaVersion" -> 1,
    "generatedBy" -> "scripts/discover-agent-runners.scala",
    "generatedAtEpochMillis" -> System.currentTimeMillis(),
    "metadataFormat" -> "preferred llms/models/efforts/versions",
    "tools" -> (claudeTools ++ codexTools ++ aiderTools ++ geminiTools ++ agyTools)
  )

  os.makeDir.all(configDir)
  os.write.over(outputPath, ujson.write(json, indent = 2) + "\n")
  println(s"Wrote $outputPath")
