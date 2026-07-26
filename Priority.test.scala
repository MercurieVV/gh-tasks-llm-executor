class PrioritySuite extends munit.FunSuite:
  private def tool(
      id: String,
      agent: String,
      strengths: List[String],
      jobTypes: List[String] = Nil,
      input: Double = 1.0,
      output: Double = 1.0
  ): AgentTool =
    AgentTool(
      id = AgentToolId(id),
      agent = Agent(agent),
      model = Some("model"),
      effort = None,
      version = None,
      roles = List("implementor"),
      jobTypes = jobTypes,
      strengths = strengths,
      available = Available(true),
      inputUsdPerMTok = Some(input),
      outputUsdPerMTok = Some(output)
    )

  test("empty requirements reproduce the legacy marker-based tier ordering"):
    val cheapWeak = tool("cheap", "vendor-a", strengths = Nil, input = 0.1, output = 0.1)
    val pricierStrong = tool("strong", "vendor-b", strengths = List("complex-reasoning"), input = 10.0, output = 10.0)
    val weights = PriorityWeights.Default
    assert(
      Priority.score(pricierStrong, Map.empty, weights) < Priority.score(cheapWeak, Map.empty, weights),
      "high-tier strengths must outrank a cheaper tool with no strengths"
    )

  test("a tool missing a required ability is scored worse than one covering it, regardless of cost"):
    val cheapButMissing = tool("cheap", "vendor-a", strengths = Nil, input = 0.1, output = 0.1)
    val pricierButCovers = tool("covers", "vendor-b", strengths = List("scala"), input = 10.0, output = 10.0)
    val requirements = Map("scala" -> 1.0)
    val weights = PriorityWeights.Default
    assert(
      Priority.score(pricierButCovers, requirements, weights) <
        Priority.score(cheapButMissing, requirements, weights)
    )

  test("among tools that all cover required abilities, cheaper wins"):
    val cheap = tool("cheap", "vendor-a", strengths = List("scala"), input = 0.1, output = 0.1)
    val pricier = tool("pricier", "vendor-b", strengths = List("scala"), input = 10.0, output = 10.0)
    val requirements = Map("scala" -> 1.0)
    val weights = PriorityWeights.Default
    assert(Priority.score(cheap, requirements, weights) < Priority.score(pricier, requirements, weights))

  test("jobTypes also satisfy a required ability, not just strengths"):
    val viaJobType = tool("via-jobtype", "vendor-a", strengths = Nil, jobTypes = List("scala"))
    val viaStrength = tool("via-strength", "vendor-a", strengths = List("scala"), jobTypes = Nil)
    val requirements = Map("scala" -> 1.0)
    assertEquals(
      Priority.score(viaJobType, requirements, PriorityWeights.Default),
      Priority.score(viaStrength, requirements, PriorityWeights.Default)
    )

  test("a custom PriorityWeights can change the winner"):
    val fitsPoorly = tool("fits-poorly", "vendor-a", strengths = Nil, input = 0.1, output = 0.1)
    val fitsWell = tool("fits-well", "vendor-b", strengths = List("scala"), input = 100.0, output = 100.0)
    val requirements = Map("scala" -> 1.0)
    // Default weights: capability dominates, fitsWell wins despite huge cost gap.
    assert(
      Priority.score(fitsWell, requirements, PriorityWeights.Default) <
        Priority.score(fitsPoorly, requirements, PriorityWeights.Default)
    )

  test("AgentInventory.selectRunnerFor: an explicit pin overrides ability scoring"):
    val cheap = tool("cheap", "vendor-a", strengths = List("scala"))
    val pinned = tool("pinned", "vendor-b", strengths = Nil)
    val inventory = AgentInventory(List(cheap, pinned))
    assertEquals(
      inventory.selectRunnerFor(Map("scala" -> 1.0), List(pinned.runner)),
      Some(pinned.runner)
    )

  test("AgentInventory.selectRunnerFor: no pin, abilities pick the best-fit available tool"):
    val poorFit = tool("poor-fit", "vendor-a", strengths = Nil, input = 0.01, output = 0.01)
    val goodFit = tool("good-fit", "vendor-b", strengths = List("scala"), input = 5.0, output = 5.0)
    val inventory = AgentInventory(List(poorFit, goodFit))
    assertEquals(
      inventory.selectRunnerFor(Map("scala" -> 1.0), Nil),
      Some(goodFit.runner)
    )

  test("AgentInventory.selectRunnerFor: neither pin nor abilities falls back to defaultImplementor"):
    val only = tool("only", "vendor-a", strengths = Nil)
    val inventory = AgentInventory(List(only))
    assertEquals(inventory.selectRunnerFor(Map.empty, Nil), Some(only.runner))

  test("PriorityWeights.load falls back to Default when the config file is absent"):
    assertEquals(PriorityWeights.load(os.temp.dir()), PriorityWeights.Default)
