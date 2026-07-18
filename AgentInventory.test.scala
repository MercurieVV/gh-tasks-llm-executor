import cats.effect.IO
import munit.CatsEffectSuite

class AgentInventorySuite extends CatsEffectSuite:
  test("derives task costs from configured raw prices and effort") {
    AgentInventory.load[IO](os.pwd).map { inventory =>
      val costs = inventory.tools.map(tool => tool.id -> tool.cost).toMap

      assertEquals(costs("claude-opus"), Some(0.60))
      assertEquals(costs("claude-sonnet"), Some(0.12))
      assertEquals(costs("claude-haiku"), Some(0.04))
      assertEquals(costs("codex-gpt-5-high"), Some(0.13))
      assertEquals(costs("codex-gpt-5-medium"), Some(0.065))
      assertEquals(costs("codex-gpt-5-low"), Some(0.0325))
      assertEquals(costs("aider-deepseek-deepseek-chat"), Some(0.0098))
      assertEquals(costs("aider-deepseek-deepseek-reasoner"), Some(0.01976))
    }
  }

  test("treats missing or zero raw price fields as cost unknown") {
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
      inputUsdPerMTok = None,
      outputUsdPerMTok = Some(1.0)
    )

    assertEquals(unknown.cost, None)
    assert(unknown.promptLine.contains("cost=unknown"))
  }
