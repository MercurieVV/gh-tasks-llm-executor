//> using scala 3.8.4
//> using dep com.lihaoyi::os-lib:0.11.8
//> using dep com.lihaoyi::ujson:4.4.3

import scala.util.Try

final case class Probe(name: String, path: Option[String], version: Option[String]):
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
    priority: Int,
    probe: Probe
): ujson.Obj =
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
    "priority" -> priority,
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
  val claude = probe("claude")
  val codex = probe("codex")
  val aider = probe("aider")

  val claudeModels = envList("AGENT_RUNNER_CLAUDE_MODELS", List("opus", "sonnet", "haiku"))
  val codexModels = envList("AGENT_RUNNER_CODEX_MODELS", List("gpt-5", "gpt-5-codex"))
  val codexEfforts = envList("AGENT_RUNNER_CODEX_EFFORTS", List("high", "medium", "low"))
  val aiderDeepseekModels = envList(
    "AGENT_RUNNER_AIDER_DEEPSEEK_MODELS",
    List("deepseek/deepseek-chat", "deepseek/deepseek-reasoner")
  )

  val claudeTools =
    claudeModels.zipWithIndex.map { case (model, index) =>
      val priority = model.toLowerCase match
        case "opus"   => 10
        case "sonnet" => 30
        case "haiku"  => 60
        case _        => 80 + index
      // Phase strengths follow the phase -> capability-tier routing: high tier
      // (opus) owns plan/source-of-truth, medium (sonnet) owns
      // source-of-truth/implement, low (haiku) owns implement/test.
      val phaseStrengths = model.toLowerCase match
        case "opus"   => List("plan", "source-of-truth")
        case "sonnet" => List("source-of-truth", "implement", "test")
        case "haiku"  => List("implement", "test")
        case _        => List("implement")
      val strengths = (model.toLowerCase match
        case "opus" =>
          List("evaluation", "complex-reasoning", "architecture", "failure-analysis")
        case "sonnet" =>
          List("scala-code", "debugging", "refactoring", "docs")
        case "haiku" =>
          List("small-edits", "docs", "mechanical-changes")
        case _ =>
          List("scala-code")
      ) ++ phaseStrengths
      tool(
        id = s"claude-$model",
        agent = "claude",
        model = model,
        effort = None,
        version = claude.version,
        roles = if model == "opus" then List("evaluator", "implementor") else List("implementor"),
        jobTypes = List("scala", "tests", "docs", "github-issues", "plan", "source-of-truth", "implement", "test"),
        strengths = strengths,
        available = claude.available,
        priority = priority,
        probe = claude
      )
    }

  val codexTools =
    for
      (model, modelIndex) <- codexModels.zipWithIndex
      (effort, effortIndex) <- codexEfforts.zipWithIndex
    yield
      tool(
        id = s"codex-$model-$effort",
        agent = "codex",
        model = model,
        effort = Some(effort),
        version = codex.version,
        roles = List("implementor"),
        jobTypes = List("scala", "tests", "repo-editing", "debugging", "plan", "source-of-truth", "implement", "test"),
        strengths =
          if effort == "high" then List("deep-code-reasoning", "multi-file-edits", "tests", "plan", "source-of-truth")
          else if effort == "medium" then List("scala-code", "focused-fixes", "tests", "source-of-truth", "implement", "test")
          else List("small-edits", "mechanical-changes", "implement", "test"),
        available = codex.available,
        priority = 100 + modelIndex * 10 + effortIndex,
        probe = codex
      )

  val aiderTools =
    aiderDeepseekModels.zipWithIndex.map { case (model, index) =>
      val strengths =
        if model.contains("reasoner") then
          List("complex-reasoning", "scala-code", "debugging", "source-of-truth", "implement", "test")
        else List("scala-code", "focused-fixes", "mechanical-changes", "implement", "test")
      tool(
        id = s"aider-${model.replace('/', '-')}",
        agent = "aider",
        model = model,
        effort = None,
        version = aider.version,
        roles = List("implementor"),
        jobTypes = List("scala", "tests", "repo-editing", "debugging", "plan", "source-of-truth", "implement", "test"),
        strengths = strengths,
        available = aider.available,
        priority = 200 + index,
        probe = aider
      )
    }

  val json = ujson.Obj(
    "schemaVersion" -> 1,
    "generatedBy" -> "scripts/discover-agent-runners.scala",
    "generatedAtEpochMillis" -> System.currentTimeMillis(),
    "metadataFormat" -> "preferred llms/models/efforts/versions",
    "tools" -> (claudeTools ++ codexTools ++ aiderTools)
  )

  os.makeDir.all(configDir)
  os.write.over(outputPath, ujson.write(json, indent = 2) + "\n")
  println(s"Wrote $outputPath")
