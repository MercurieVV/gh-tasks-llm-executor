package com.github.mercurievv.ghllm.cli

import com.github.mercurievv.ghllm.*
import com.github.mercurievv.ghllm.agent.*
import com.github.mercurievv.ghllm.arrow.*
import com.github.mercurievv.ghllm.cli.*
import com.github.mercurievv.ghllm.git.*
import com.github.mercurievv.ghllm.metrics.*

class TaskRunnerCommandSuite extends munit.FunSuite:
  private val prompt = AgentPrompt("finish the task")

  test("claude command attaches workspace MCP config and allows ScalaSemantic tools"):
    val root = os.temp.dir()
    os.makeDir.all(root / ".agents")
    os.write(root / ".agents" / "mcp_config.json", """{"mcpServers":{}}""")

    val command =
      TaskRunner(AgentBinary("claude"), Some("sonnet"), None, None)
        .command(prompt, allowedTools = Seq("Read"), cwd = Some(root))

    assert(command.contains("--mcp-config"))
    assert(command.contains((root / ".agents" / "mcp_config.json").toString))
    assert(command.contains("--allowedTools"))
    assert(command.contains("Read"))
    assert(command.contains("mcp__scala-semantic__annotated_source"))
    assert(command.contains("mcp__scala-semantic__find_symbol"))
    assert(command.last.contains("SCALA SEMANTIC NAVIGATION RULE (must be obeyed):"))
    assert(command.last.contains("Call `set_workspace_root` ONCE with the absolute `Worktree:` path"))
    assert(command.last.contains(prompt.value))

  test("claude command does not add MCP args without workspace MCP config"):
    val root = os.temp.dir()

    val command =
      TaskRunner(AgentBinary("claude"), None, None, None)
        .command(prompt, allowedTools = Seq("Read"), cwd = Some(root))

    assert(!command.contains("--mcp-config"))
    assert(!command.contains("mcp__scala-semantic__annotated_source"))
    assert(!command.exists(_.contains("SCALA SEMANTIC NAVIGATION RULE (must be obeyed):")))
    // The cache flag is unconditional (ADR-0001), so it belongs in the exact
    // shape too rather than being filtered out of it.
    assertEquals(
      command,
      Seq(
        "claude",
        "--exclude-dynamic-system-prompt-sections",
        "--allowedTools",
        "Read",
        "-p",
        prompt.value
      )
    )

  test("codex command maps workspace MCP JSON to config overrides and injects ScalaSemantic prompt"):
    val root = os.temp.dir()
    os.makeDir.all(root / ".agents")
    os.write(
      root / ".agents" / "mcp_config.json",
      """{"mcpServers":{"scala-semantic":{"command":"/repo/scalasemantic-mcp.sh","args":["serve","."]}}}"""
    )

    val command =
      TaskRunner(AgentBinary("codex"), Some("gpt-5"), Some("low"), None)
        .command(prompt, allowedTools = Seq("Read"), cwd = Some(root))

    assert(command.contains("--config"))
    assert(command.contains("mcp_servers.scala-semantic.command=\"/repo/scalasemantic-mcp.sh\""))
    assert(command.contains("""mcp_servers.scala-semantic.args=["serve","."]"""))
    assert(command.last.contains("SCALA SEMANTIC NAVIGATION RULE (must be obeyed):"))
    assert(command.last.contains("before falling back to shell text tools"))
    assert(command.last.contains(prompt.value))

  test("ScalaSemantic prompt injection is idempotent"):
    val root = os.temp.dir()
    os.makeDir.all(root / ".agents")
    os.write(root / ".agents" / "mcp_config.json", """{"mcpServers":{}}""")
    val runner = TaskRunner(AgentBinary("claude"), Some("sonnet"), None, None)

    val once = runner.effectivePrompt(prompt, allowedTools = Seq("Read"), cwd = Some(root))
    val twice = runner.effectivePrompt(once, allowedTools = Seq("Read"), cwd = Some(root))

    assertEquals(twice.value, once.value)

  test("a prompt that already carries Impl's mandate is not given a second one"):
    // Both injections fire on a Scala task in an MCP-configured worktree: Impl
    // appends on `taskTouchesScala`, TaskRunner prepends on `mcp_config.json`.
    // They must share one wording, or the agent pays for the rule twice.
    val root = os.temp.dir()
    os.makeDir.all(root / ".agents")
    os.write(root / ".agents" / "mcp_config.json", """{"mcpServers":{}}""")
    val runner = TaskRunner(AgentBinary("claude"), Some("sonnet"), None, None)

    val fromImpl = AgentPrompt(s"${prompt.value}\n\n${Impl.ScalaSemanticMandate}")
    val injected = runner.effectivePrompt(fromImpl, allowedTools = Seq("Read"), cwd = Some(root))

    assertEquals(injected.value, fromImpl.value)
    assertEquals(
      injected.value.sliding(Impl.ScalaSemanticMandateHeader.length).count(_ == Impl.ScalaSemanticMandateHeader),
      1
    )

  test("aider command appends explicit context files"):
    val command =
      TaskRunner(AgentBinary("aider"), Some("deepseek/deepseek-reasoner"), None, None)
        .command(prompt, contextFiles = Seq(".gitignore", "build.mill"))

    assertEquals(command.takeRight(2), Seq(".gitignore", "build.mill"))
    assert(command.contains("--message"))
    assert(command.contains(prompt.value))
    assert(command.contains("deepseek/deepseek-v4-pro"))

  test("non-aider commands ignore context files"):
    val command =
      TaskRunner(AgentBinary("gemini"), Some("gemini-pro"), None, None)
        .command(prompt, contextFiles = Seq(".gitignore"))

    assert(!command.contains(".gitignore"))

  test("metricsIdentity is stable across a CLI upgrade"):
    // `display` carries the version, and keying measurements on it partitioned
    // every series by installed CLI build. Since minSample is 20 and the claude/
    // codex CLIs ship far more often than 20 runs of one phase accumulate,
    // successRate and meanUsage returned None forever and selectRunnerFor never
    // left the Priority.score fallback.
    val before = TaskRunner(AgentBinary("claude"), Some("haiku"), None, Some("2.1.220 (Claude Code)"))
    val after = before.copy(version = Some("2.1.221 (Claude Code)"))

    assertEquals(before.metricsIdentity, after.metricsIdentity)
    assertNotEquals(before.display, after.display)

  test("metricsIdentity still separates model and effort"):
    val low = TaskRunner(AgentBinary("codex"), Some("gpt-5"), Some("low"), None)
    val high = low.copy(effort = Some("high"))
    val other = low.copy(model = Some("gpt-5-codex"))

    assertNotEquals(low.metricsIdentity, high.metricsIdentity)
    assertNotEquals(low.metricsIdentity, other.metricsIdentity)

  test("only MCP-wired agents are asked to obey the ScalaSemantic mandate"):
    assert(TaskRunner(AgentBinary("claude"), Some("opus"), None, None).supportsScalaSemanticMcp)
    assert(TaskRunner(AgentBinary("codex"), Some("gpt-5"), None, None).supportsScalaSemanticMcp)
    assert(!TaskRunner(AgentBinary("aider"), Some("deepseek/deepseek-reasoner"), None, None).supportsScalaSemanticMcp)
    assert(!TaskRunner(AgentBinary("gemini"), Some("gemini-3-pro"), None, None).supportsScalaSemanticMcp)
