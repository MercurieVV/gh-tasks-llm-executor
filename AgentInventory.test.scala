class AgentInventorySuite extends munit.FunSuite:
  test("loads the agreed USD-per-task costs for every discovered runner") {
    val inventory: AgentInventory = AgentInventory.load(os.pwd)
    val costs = inventory.tools.map(tool => tool.id -> tool.cost).toMap

    assertEquals(
      costs,
      Map(
        "claude-opus" -> Some(0.60),
        "claude-sonnet" -> Some(0.12),
        "claude-haiku" -> Some(0.04),
        "codex-gpt-5-high" -> Some(0.105),
        "codex-gpt-5-medium" -> Some(0.065),
        "codex-gpt-5-low" -> Some(0.045),
        "codex-gpt-5-codex-high" -> Some(0.105),
        "codex-gpt-5-codex-medium" -> Some(0.065),
        "codex-gpt-5-codex-low" -> Some(0.045),
        "aider-deepseek-deepseek-chat" -> Some(0.010),
        "aider-deepseek-deepseek-reasoner" -> Some(0.029)
      ),
      "all discovered runners have their agreed cost"
    )
  }

  test("includes known and unknown costs in runner prompt lines") {
    val inventory: AgentInventory = AgentInventory.load(os.pwd)
    val cheapest = inventory.tools.find(_.id == "aider-deepseek-deepseek-chat")
    val unknown = AgentTool(
      id = "unknown",
      agent = "unknown",
      model = Some("unknown"),
      effort = None,
      version = None,
      roles = Nil,
      jobTypes = Nil,
      strengths = Nil,
      available = true,
      priority = 1,
      cost = None
    )

    assertEquals(
      cheapest.map(_.promptLine.contains("cost=$0.010/task (1x)")),
      Some(true),
      "known cost is included in the prompt line"
    )
    assert(unknown.promptLine.contains("cost=unknown"))
  }
