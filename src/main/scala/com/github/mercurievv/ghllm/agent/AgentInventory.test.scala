package com.github.mercurievv.ghllm.agent

import com.github.mercurievv.ghllm.*
import com.github.mercurievv.ghllm.agent.*
import com.github.mercurievv.ghllm.arrow.*
import com.github.mercurievv.ghllm.cli.*
import com.github.mercurievv.ghllm.git.*
import com.github.mercurievv.ghllm.metrics.*

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
      assertEquals(costs("aider-deepseek-deepseek-chat"), Some(0.004))
      assertEquals(costs("aider-deepseek-deepseek-reasoner"), Some(0.016))
    }
  }

  test("treats missing or zero raw price fields as cost unknown") {
    val unknown = AgentTool(
      id = AgentToolId("unknown"),
      agent = Agent("unknown"),
      model = Some("unknown"),
      effort = None,
      version = None,
      roles = Nil,
      jobTypes = Nil,
      strengths = Nil,
      available = Available(true),
      inputUsdPerMTok = None,
      outputUsdPerMTok = Some(1.0)
    )

    assertEquals(unknown.cost, None)
    assert(unknown.promptLine.contains("cost=unknown"))
  }

  test("matches bare task-metadata version against full probe version string") {
    val codexTool = AgentTool(
      id = AgentToolId("codex-gpt-5-codex-medium"),
      agent = Agent("codex"),
      model = Some("gpt-5-codex"),
      effort = Some("medium"),
      version = Some("codex-cli 0.144.4"),
      roles = List("implementor"),
      jobTypes = Nil,
      strengths = Nil,
      available = Available(true)
    )
    val claudeTool = AgentTool(
      id = AgentToolId("claude-sonnet"),
      agent = Agent("claude"),
      model = Some("sonnet"),
      effort = None,
      version = Some("2.1.210 (Claude Code)"),
      roles = List("implementor"),
      jobTypes = Nil,
      strengths = Nil,
      available = Available(true)
    )

    assert(
      codexTool.matches(
        TaskRunner(
          agent = AgentBinary("codex"),
          model = Some("gpt-5-codex"),
          effort = Some("medium"),
          version = Some("0.144.4")
        )
      )
    )
    assert(
      claudeTool.matches(
        TaskRunner(
          agent = AgentBinary("claude"),
          model = Some("sonnet"),
          effort = None,
          version = Some("2.1.210")
        )
      )
    )
    assert(
      !codexTool.matches(
        TaskRunner(
          agent = AgentBinary("codex"),
          model = Some("gpt-5-codex"),
          effort = Some("medium"),
          version = Some("0.144.3")
        )
      )
    )
  }

  test("alternate implementor skips vendors marked exhausted by budget pressure") {
    val root = os.temp.dir()
    os.makeDir.all(root / ".gh-tasks-llm-executor")
    os.write.over(
      root / ".gh-tasks-llm-executor" / "vendor-budgets.json",
      ujson.write(
        ujson.Obj(
          "schemaVersion" -> 1,
          "generatedAtEpochMillis" -> System.currentTimeMillis().toString,
          "budgets" -> ujson.Arr(
            ujson.Obj(
              "vendor" -> "deepseek",
              "usedFraction" -> 0.50,
              "extra" -> ujson.Obj("currentBalanceEur" -> -0.12)
            ),
            ujson.Obj("vendor" -> "codex", "usedFraction" -> 0.10)
          )
        )
      )
    )
    os.write.over(
      root / ".gh-tasks-llm-executor" / "agent-runners.json",
      ujson.write(
        ujson.Obj(
          "tools" -> ujson.Arr(
            ujson.Obj(
              "id" -> "aider-deepseek",
              "agent" -> "aider",
              "model" -> "deepseek/deepseek-reasoner",
              "roles" -> ujson.Arr("implementor"),
              "jobTypes" -> ujson.Arr("implement"),
              "strengths" -> ujson.Arr("focused-fixes"),
              "available" -> true,
              "inputUsdPerMTok" -> 0.1,
              "outputUsdPerMTok" -> 1.0
            ),
            ujson.Obj(
              "id" -> "codex-low",
              "agent" -> "codex",
              "model" -> "gpt-5",
              "effort" -> "low",
              "roles" -> ujson.Arr("implementor"),
              "jobTypes" -> ujson.Arr("implement"),
              "strengths" -> ujson.Arr("focused-fixes"),
              "available" -> true,
              "inputUsdPerMTok" -> 1.25,
              "outputUsdPerMTok" -> 10.0
            )
          )
        )
      )
    )

    val inventory = AgentInventory.load(root)
    val failed = TaskRunner(AgentBinary("aider"), Some("deepseek/deepseek-reasoner"), None, None)

    assertEquals(inventory.defaultImplementor.map(_.agent.value), Some("codex"))
    assertEquals(inventory.alternateImplementor(failed).map(_.agent.value), Some("codex"))
  }
