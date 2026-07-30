package com.github.mercurievv.ghllm

import com.github.mercurievv.ghllm.agent.*

import scala.util.Try

/** Concrete agent invocation choice, including optional model, effort, and version.
  */
final case class TaskRunner(
    agent: AgentBinary,
    model: Option[String],
    effort: Option[String],
    version: Option[String],
    extendedCacheTtl: Boolean = false
):
  def display: String =
    val modelPart = model.fold("")(value => s", model: $value")
    val effortPart = effort.fold("")(value => s", effort: $value")
    val versionPart = version.fold("")(value => s", version: $value")
    val cacheTtlPart = Option.when(extendedCacheTtl)(", cache TTL: 1h").getOrElse("")
    s"agent: $agent$modelPart$effortPart$versionPart$cacheTtlPart"

  def invocationEnvironment: Map[String, String] =
    Option
      .when(agent.value == "claude" && extendedCacheTtl)(
        "ENABLE_PROMPT_CACHING_1H" -> "1"
      )
      .toMap

  def command(
      prompt: AgentPrompt,
      allowedTools: Seq[String] = Nil,
      jsonSchema: Option[String] = None,
      cwd: Option[os.Path] = None,
      contextFiles: Seq[String] = Nil
  ): Seq[String] =
    val promptForRun = effectivePrompt(prompt, allowedTools, cwd)
    agent.value match
      case "claude" =>
        val mcpConfig = workspaceFile(cwd, os.rel / ".agents" / "mcp_config.json")
        val effectiveAllowedTools =
          allowedTools ++ mcpConfig.toList.flatMap(_ => ScalaSemanticClaudeTools)
        Seq(agent.value) ++ model.toList.flatMap(value => Seq("--model", value)) ++
          mcpConfig.toList.flatMap(path => Seq("--mcp-config", path.toString)) ++
          (if effectiveAllowedTools.isEmpty then Nil
           else Seq("--allowedTools") ++ effectiveAllowedTools) ++
          jsonSchema.toList.flatMap(schema => Seq("--json-schema", schema)) ++
          Seq("-p", promptForRun.value)
      case "codex" =>
        val mappedModel = (model, effort) match
          case (Some("gpt-5") | Some("gpt-5-codex"), Some("medium")) =>
            Some("gpt-5.6-terra")
          case (Some("gpt-5") | Some("gpt-5-codex"), Some("high")) =>
            Some("gpt-5.6-sol")
          case (Some("gpt-5") | Some("gpt-5-codex"), Some("low")) =>
            Some("gpt-5.6-luna")
          case _ => model
        Seq(agent.value, "exec") ++
          mappedModel.toList.flatMap(value => Seq("--model", value)) ++
          effort.toList.flatMap(value => Seq("--config", s"model_reasoning_effort=$value")) ++
          codexMcpConfigArgs(cwd) ++
          Seq(promptForRun.value)
      case "aider" =>
        // DeepSeek retired deepseek-chat/deepseek-reasoner in favor of
        // deepseek-v4-flash/deepseek-v4-pro. Runner ids/model fields keep the
        // old names so previously-persisted task metadata still resolves via
        // AgentInventory.selectRunner; only the invoked API model changes.
        val mappedModel = model match
          case Some("deepseek/deepseek-chat")     => Some("deepseek/deepseek-v4-flash")
          case Some("deepseek/deepseek-reasoner") => Some("deepseek/deepseek-v4-pro")
          case other                              => other
        Seq(agent.value) ++ mappedModel.toList.flatMap(value => Seq("--model", value)) ++
          Seq("--yes-always", "--no-auto-commits", "--message", promptForRun.value) ++
          contextFiles
      case "gemini" =>
        Seq(agent.value) ++ model.toList.flatMap(value => Seq("-m", value)) ++
          Seq("-p", promptForRun.value)
      case "agy" =>
        Seq(agent.value) ++ model.toList.flatMap(value => Seq("--model", value)) ++
          effort.toList.flatMap(value => Seq("--effort", value)) ++
          Seq("--print", promptForRun.value)
      case _ =>
        Seq(agent.value) ++ model.toList.flatMap(value => Seq("-m", value)) ++
          Seq("-p", promptForRun.value)

  def effectivePrompt(
      prompt: AgentPrompt,
      allowedTools: Seq[String] = Nil,
      cwd: Option[os.Path] = None
  ): AgentPrompt =
    if shouldInjectScalaSemanticInstruction(allowedTools, cwd) &&
      !prompt.value.contains(ScalaSemanticInstructionHeader)
    then AgentPrompt(s"$ScalaSemanticInstruction\n\n${prompt.value}")
    else prompt

  private def workspaceFile(cwd: Option[os.Path], path: os.RelPath): Option[os.Path] =
    cwd.map(_ / path).filter(os.exists(_))

  private def shouldInjectScalaSemanticInstruction(
      allowedTools: Seq[String],
      cwd: Option[os.Path]
  ): Boolean =
    Set("claude", "codex").contains(agent.value) &&
      allowedTools.nonEmpty &&
      workspaceFile(cwd, os.rel / ".agents" / "mcp_config.json").nonEmpty

  private def codexMcpConfigArgs(cwd: Option[os.Path]): Seq[String] =
    workspaceFile(cwd, os.rel / ".agents" / "mcp_config.json").toList.flatMap { path =>
      val servers =
        for
          json <- Try(ujson.read(os.read(path))).toOption
          servers <- json.obj.get("mcpServers").map(_.obj)
        yield servers.toSeq.flatMap { case (name, server) =>
          val obj = server.obj
          val command = obj.get("command").map(_.str).toSeq.flatMap { value =>
            Seq("--config", s"mcp_servers.$name.command=${tomlString(value)}")
          }
          val args = obj.get("args").map(_.arr.map(_.str).toSeq).toSeq.flatMap { values =>
            Seq("--config", s"mcp_servers.$name.args=${tomlStringArray(values)}")
          }
          command ++ args
        }
      servers.getOrElse(Nil)
    }

  private def tomlString(value: String): String = ujson.write(value)

  private def tomlStringArray(values: Seq[String]): String =
    values.map(tomlString).mkString("[", ",", "]")

  private val ScalaSemanticInstructionHeader =
    "ScalaSemantic MCP requirement:"

  private val ScalaSemanticInstruction =
    s"""$ScalaSemanticInstructionHeader
       |- Before inspecting or editing Scala source, call the ScalaSemantic MCP tools.
       |- Use `set_workspace_root` for the current worktree first.
       |- Use `annotated_source` to read `.scala` files and semantic tools such as `find_symbol`, `find_usages`, `type_at_position`, `method_signature`, `members`, `class_hierarchy`, `resolve_implicits`, or `call_path` for Scala code questions.
       |- Do not use shell text tools such as `cat`, `sed`, `rg`, or `grep` to inspect `.scala` source unless ScalaSemantic MCP is unavailable or failing; if that happens, state the failure in your final answer.
       |""".stripMargin

  private val ScalaSemanticClaudeTools = Seq(
    "mcp__scala-semantic__annotated_source",
    "mcp__scala-semantic__set_workspace_root",
    "mcp__scala-semantic__refresh_workspace",
    "mcp__scala-semantic__smart_code_duplications",
    "mcp__scala-semantic__batch_rename_plan",
    "mcp__scala-semantic__find_symbol",
    "mcp__scala-semantic__find_usages",
    "mcp__scala-semantic__class_hierarchy",
    "mcp__scala-semantic__method_signature",
    "mcp__scala-semantic__find_overloads",
    "mcp__scala-semantic__members",
    "mcp__scala-semantic__resolve_implicits",
    "mcp__scala-semantic__trace_implicit_chain",
    "mcp__scala-semantic__call_path",
    "mcp__scala-semantic__type_at_position",
    "mcp__scala-semantic__document_outline",
    "mcp__scala-semantic__rename_plan",
    "mcp__scala-semantic__move_plan",
    "mcp__scala-semantic__extract_method_plan",
    "mcp__scala-semantic__value_flow"
  )

object TaskRunner:
  def unapply(
      runner: TaskRunner
  ): (AgentBinary, Option[String], Option[String], Option[String]) =
    (runner.agent, runner.model, runner.effort, runner.version)
