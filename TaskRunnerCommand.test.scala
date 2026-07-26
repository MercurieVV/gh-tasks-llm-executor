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

  test("claude command does not add MCP args without workspace MCP config"):
    val root = os.temp.dir()

    val command =
      TaskRunner(AgentBinary("claude"), None, None, None)
        .command(prompt, allowedTools = Seq("Read"), cwd = Some(root))

    assert(!command.contains("--mcp-config"))
    assert(!command.contains("mcp__scala-semantic__annotated_source"))
    assertEquals(command, Seq("claude", "--allowedTools", "Read", "-p", prompt.value))

  test("codex command maps workspace MCP JSON to config overrides"):
    val root = os.temp.dir()
    os.makeDir.all(root / ".agents")
    os.write(
      root / ".agents" / "mcp_config.json",
      """{"mcpServers":{"scala-semantic":{"command":"/repo/scalasemantic-mcp.sh","args":["serve","."]}}}"""
    )

    val command =
      TaskRunner(AgentBinary("codex"), Some("gpt-5"), Some("low"), None)
        .command(prompt, cwd = Some(root))

    assert(command.contains("--config"))
    assert(command.contains("mcp_servers.scala-semantic.command=\"/repo/scalasemantic-mcp.sh\""))
    assert(command.contains("""mcp_servers.scala-semantic.args=["serve","."]"""))
    assertEquals(command.last, prompt.value)
