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

  test("a sequential batch earns it too — the window outlives the run"):
    // Deliberately independent of --parallel: three same-runner roots share the
    // constant prompt segments whether they run side by side or one after
    // another, and an hour-long window is what lets the later ones read what
    // the first wrote.
    val claude = runner("claude", "opus")
    assertEquals(marked(parallel = false, claude, claude, claude), List(true, true, true))

  test("and a sequential batch of two still does not"):
    val claude = runner("claude", "opus")
    assertEquals(marked(parallel = false, claude, claude), List(false, false))

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

class CacheFlagSuite extends munit.FunSuite:
  // See ADR-0001-prompt-cache-knobs.md.

  private def commandFor(agent: String, model: String): Seq[String] =
    TaskRunner(AgentBinary(agent), Some(model), None, None)
      .command(AgentPrompt("do the thing"))

  test("claude is asked to keep per-machine context out of the system prompt"):
    // Each task runs in its own worktree, so cwd/git status differ per sibling.
    // Left in the system prompt they precede - and so invalidate - every
    // constant segment taskPrompt orders stable-first for T19.
    assert(commandFor("claude", "opus").contains("--exclude-dynamic-system-prompt-sections"))

  test("aider is asked to cache prompts at all — its own default is off"):
    assert(commandFor("aider", "deepseek/deepseek-chat").contains("--cache-prompts"))

  test("no keepalive pings: single-shot runs have no later reader to keep warm"):
    assert(!commandFor("aider", "deepseek/deepseek-chat").contains("--cache-keepalive-pings"))

  test("vendors with no client-side knob get no invented flag"):
    // codex/gemini/agy cache server-side or not at all; passing something here
    // would be a guess that fails hard on an unrecognised flag.
    List("codex", "gemini", "agy").foreach { agent =>
      val command = commandFor(agent, "some-model")
      assert(!command.exists(_.contains("cache")), s"$agent: $command")
    }
