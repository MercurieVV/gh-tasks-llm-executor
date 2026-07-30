package com.github.mercurievv.ghllm

import com.github.mercurievv.ghllm.agent.*
import com.github.mercurievv.ghllm.git.*

class CachePeersSuite extends munit.FunSuite:

  private def context(parallel: Boolean): RunContext =
    RunContext(
      root = os.pwd,
      agentInventory = AgentInventory(Nil),
      taskNumber = None,
      parallelExecution = ParallelExecution(parallel)
    )

  private def runner(agent: String, model: String): TaskRunner =
    TaskRunner(AgentBinary(agent), Some(model), None, None)

  private def candidate(number: Int, runner: TaskRunner, parallel: Boolean): TaskCandidate =
    TaskCandidate(
      context(parallel),
      Issue(TaskNumber(number), IssueTitle(s"Task $number"), IssueBody(""), State("open")),
      runner
    )

  private def marked(parallel: Boolean, runners: TaskRunner*): List[Boolean] =
    val candidates = runners.toList.zipWithIndex.map { case (r, i) => candidate(i + 1, r, parallel) }
    BusinessLogic
      .markCachePeers(TaskSelection(context(parallel), candidates))
      .map(_.runner.extendedCacheTtl)

  test("three peers on one runner earn the extended TTL"):
    val claude = runner("claude", "opus")
    assertEquals(marked(parallel = true, claude, claude, claude), List(true, true, true))

  test("two peers do not — the write premium never pays back"):
    val claude = runner("claude", "opus")
    assertEquals(marked(parallel = true, claude, claude), List(false, false))

  test("a single candidate does not"):
    assertEquals(marked(parallel = true, runner("claude", "opus")), List(false))

  test("peer groups are counted per (agent, model), not per batch"):
    // Three candidates, but no three of them share a prefix.
    val opus = runner("claude", "opus")
    val haiku = runner("claude", "haiku")
    val codex = runner("codex", "gpt-5")
    assertEquals(marked(parallel = true, opus, haiku, codex), List(false, false, false))

  test("a large group marks only its own members"):
    val opus = runner("claude", "opus")
    val codex = runner("codex", "gpt-5")
    assertEquals(marked(parallel = true, opus, opus, opus, codex), List(true, true, true, false))

  test("sequential runs never set it"):
    // Without --parallel a batch of unrelated roots has no shared prefix worth
    // paying a 2x write for.
    val claude = runner("claude", "opus")
    assertEquals(marked(parallel = false, claude, claude, claude), List(false, false, false))

class InvocationEnvironmentSuite extends munit.FunSuite:

  test("the claude CLI is told to use the 1h cache only when marked"):
    val base = TaskRunner(AgentBinary("claude"), Some("opus"), None, None)
    assertEquals(base.invocationEnvironment, Map.empty[String, String])
    assertEquals(
      base.copy(extendedCacheTtl = true).invocationEnvironment,
      Map("ENABLE_PROMPT_CACHING_1H" -> "1")
    )

  test("other vendors get no env var — the knob is claude-specific"):
    // codex/gemini/aider construct their own requests and expose no equivalent,
    // so marking them would be a silent no-op rather than a saving.
    val codex = TaskRunner(AgentBinary("codex"), Some("gpt-5"), None, None, extendedCacheTtl = true)
    assertEquals(codex.invocationEnvironment, Map.empty[String, String])
