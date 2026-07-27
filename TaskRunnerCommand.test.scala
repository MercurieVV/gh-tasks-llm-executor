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
    assert(command.last.contains("ScalaSemantic MCP requirement:"))
    assert(command.last.contains("Use `set_workspace_root` for the current worktree first."))
    assert(command.last.contains(prompt.value))

  test("claude command does not add MCP args without workspace MCP config"):
    val root = os.temp.dir()

    val command =
      TaskRunner(AgentBinary("claude"), None, None, None)
        .command(prompt, allowedTools = Seq("Read"), cwd = Some(root))

    assert(!command.contains("--mcp-config"))
    assert(!command.contains("mcp__scala-semantic__annotated_source"))
    assert(!command.exists(_.contains("ScalaSemantic MCP requirement:")))
    assertEquals(command, Seq("claude", "--allowedTools", "Read", "-p", prompt.value))

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
    assert(command.last.contains("ScalaSemantic MCP requirement:"))
    assert(command.last.contains("Do not use shell text tools"))
    assert(command.last.contains(prompt.value))

  test("ScalaSemantic prompt injection is idempotent"):
    val root = os.temp.dir()
    os.makeDir.all(root / ".agents")
    os.write(root / ".agents" / "mcp_config.json", """{"mcpServers":{}}""")
    val runner = TaskRunner(AgentBinary("claude"), Some("sonnet"), None, None)

    val once = runner.effectivePrompt(prompt, allowedTools = Seq("Read"), cwd = Some(root))
    val twice = runner.effectivePrompt(once, allowedTools = Seq("Read"), cwd = Some(root))

    assertEquals(twice.value, once.value)

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
