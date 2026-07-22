import cats.effect.IO
import munit.CatsEffectSuite

class AgentInventorySuite extends CatsEffectSuite:
  test("derives task costs from configured raw prices and effort") {
    AgentInventory.loadF[IO](os.pwd).map { inventory =>
      val costs = inventory.tools.map(tool => tool.id.value -> tool.cost).toMap

      assertEquals(costs("claude-opus"), Some(0.60))
      assertEquals(costs("claude-sonnet"), Some(0.12))
      assertEquals(costs("claude-haiku"), Some(0.04))
      assertEquals(costs("codex-gpt-5-high"), Some(0.105))
      assertEquals(costs("codex-gpt-5-medium"), Some(0.065))
      assertEquals(costs("codex-gpt-5-low"), Some(0.045))
      assertEquals(costs("aider-deepseek-deepseek-chat"), Some(0.010))
      assertEquals(costs("aider-deepseek-deepseek-reasoner"), Some(0.029))
    }
  }

  test("treats missing or zero raw price fields as cost unknown") {
    val unknown = AgentTool(
      id = Id("unknown"),
      agent = "unknown",
      model = Some("unknown"),
      effort = None,
      version = None,
      roles = Nil,
      jobTypes = Nil,
      strengths = Nil,
      available = true,
      priority = 1,
      inputUsdPerMTok = None,
      outputUsdPerMTok = Some(1.0)
    )

    assertEquals(unknown.cost, None)
    assert(unknown.promptLine.contains("cost=unknown"))
  }

  test("matches bare task-metadata version against full probe version string") {
    val codexTool = AgentTool(
      id = Id("codex-gpt-5-codex-medium"),
      agent = "codex",
      model = Some("gpt-5-codex"),
      effort = Some("medium"),
      version = Some("codex-cli 0.144.4"),
      roles = List("implementor"),
      jobTypes = Nil,
      strengths = Nil,
      available = true,
      priority = 111
    )
    val claudeTool = AgentTool(
      id = Id("claude-sonnet"),
      agent = "claude",
      model = Some("sonnet"),
      effort = None,
      version = Some("2.1.210 (Claude Code)"),
      roles = List("implementor"),
      jobTypes = Nil,
      strengths = Nil,
      available = true,
      priority = 30
    )

    assert(
      codexTool.matches(
        TaskRunner(
          agent = Agent("codex"),
          model = Some("gpt-5-codex"),
          effort = Some("medium"),
          version = Some("0.144.4")
        )
      )
    )
    assert(
      claudeTool.matches(
        TaskRunner(
          agent = Agent("claude"),
          model = Some("sonnet"),
          effort = None,
          version = Some("2.1.210")
        )
      )
    )
    assert(
      !codexTool.matches(
        TaskRunner(
          agent = Agent("codex"),
          model = Some("gpt-5-codex"),
          effort = Some("medium"),
          version = Some("0.144.3")
        )
      )
    )
  }
