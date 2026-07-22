class AgentInventoryConfigDrivenSuite extends munit.FunSuite:
  private val expectedCosts = Map(
    "claude-opus" -> 0.6,
    "claude-sonnet" -> 0.12,
    "claude-haiku" -> 0.04,
    "codex-gpt-5-high" -> 0.105,
    "codex-gpt-5-medium" -> 0.065,
    "codex-gpt-5-low" -> 0.045,
    "codex-gpt-5-codex-high" -> 0.105,
    "codex-gpt-5-codex-medium" -> 0.065,
    "codex-gpt-5-codex-low" -> 0.045,
    "aider-deepseek-deepseek-chat" -> 0.01,
    "aider-deepseek-deepseek-reasoner" -> 0.029
  ).map{ case (str, d) => Id(str) -> d}

  private val tolerance = 0.000001

  test("config raw prices reproduce the pinned #12 $/task values"):
    val tools = AgentInventory.load(os.pwd).tools
    assertEquals(tools.map(_.id).toSet, expectedCosts.keySet)
    tools.foreach { tool =>
      val actual = tool.cost
      assert(actual.nonEmpty, s"${tool.id} should have a priced model")
      assert(
        math.abs(actual.get - expectedCosts(tool.id)) <= tolerance,
        s"${tool.id}: expected ${expectedCosts(tool.id)}, got $actual"
      )
      assert(tool.inputUsdPerMTok.nonEmpty)
      assert(tool.outputUsdPerMTok.nonEmpty)
      assert(tool.source.nonEmpty)
      assert(tool.asOfDate.nonEmpty)
    }

  test("the pinned 20k/4k budget and effort multipliers are reflected in cost"):
    def cost(effort: String): Double =
      AgentTool(
        id = Id(effort),
        agent = "test",
        model = Some("model"),
        effort = Some(effort),
        version = None,
        roles = Nil,
        jobTypes = Nil,
        strengths = Nil,
        available = true,
        priority = 1,
        inputUsdPerMTok = Some(1.0),
        outputUsdPerMTok = Some(1.0)
      ).cost.get

    val low = cost("low")
    val medium = cost("medium")
    val high = cost("high")
    assertEquals(medium, 0.024)
    assertEquals(low, 0.022)
    assertEquals(high, 0.028)

  test("missing raw prices remain unknown and do not alter #12 selection"):
    val unknown = AgentTool(
      id = Id("unpriced"),
      agent = "new-agent",
      model = Some("new-model"),
      effort = None,
      version = None,
      roles = List("implementor"),
      jobTypes = List("test"),
      strengths = Nil,
      available = true,
      priority = 1
    )
    val inventory = AgentInventory(List(unknown))
    assertEquals(unknown.cost, None)
    assert(unknown.promptLine.contains("cost=unknown"))
    assertEquals(
      inventory.selectRunner(List(unknown.runner)),
      Some(unknown.runner)
    )
