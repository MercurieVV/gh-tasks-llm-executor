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

  private def dispatchOrder(runners: TaskRunner*): List[(Int, String)] =
    val candidates = runners.toList.zipWithIndex.map { case (r, i) => candidate(i + 1, r, parallel = false) }
    BusinessLogic
      .markCachePeers(TaskSelection(context(false), candidates))
      .map(c => (c.issue.number.value, c.runner.agent.value + "/" + c.runner.model.getOrElse("-")))

  test("candidates sharing a cache key are dispatched adjacently, not interleaved"):
    // A B A B re-sends A's prefix on its second run once B's turn outlives the
    // provider's default 5-minute window. Neither group here reaches MinPeers,
    // so no TTL is bought - adjacency is the whole saving.
    val opus = runner("claude", "opus")
    val codex = runner("codex", "gpt-5")
    assertEquals(
      dispatchOrder(opus, codex, opus, codex),
      List(1 -> "claude/opus", 3 -> "claude/opus", 2 -> "codex/gpt-5", 4 -> "codex/gpt-5")
    )

  test("grouping reorders the queue without reprioritising it"):
    // Both directions of stability: the first candidate still runs first, and
    // members keep their relative order inside a group. Without that this would
    // be a scheduler quietly overriding selection priority.
    val opus = runner("claude", "opus")
    val haiku = runner("claude", "haiku")
    val ordered = dispatchOrder(haiku, opus, haiku, opus, haiku)

    assertEquals(ordered.head, 1 -> "claude/haiku")
    assertEquals(ordered.map(_._1), List(1, 3, 5, 2, 4))

  test("a batch that already shares one key is left exactly as it was"):
    val claude = runner("claude", "opus")
    assertEquals(dispatchOrder(claude, claude, claude).map(_._1), List(1, 2, 3))

  test("the TTL marks follow the candidates through the reorder"):
    // The failure this pins: computing `qualifying` on the original order and
    // zipping it onto the grouped list would hand one group's marks to another.
    val opus = runner("claude", "opus")
    val codex = runner("codex", "gpt-5")
    val candidates =
      List(opus, codex, opus, codex, opus).zipWithIndex.map { case (r, i) =>
        candidate(i + 1, r, parallel = false)
      }
    val result = BusinessLogic.markCachePeers(TaskSelection(context(false), candidates))

    assertEquals(
      result.map(c => (c.runner.agent.value, c.runner.extendedCacheTtl)),
      List(
        ("claude", true),
        ("claude", true),
        ("claude", true),
        ("codex", false),
        ("codex", false)
      )
    )

class GroupAdjacentSuite extends munit.ScalaCheckSuite:
  import org.scalacheck.Gen
  import org.scalacheck.Prop.forAll

  private val items = Gen.listOf(Gen.zip(Gen.choose(0, 4), Gen.choose(0, 99)))

  property("grouping is a permutation - no run is dropped or duplicated"):
    forAll(items) { list =>
      val grouped = arrow.CachePeers.groupAdjacent(list)(_._1)
      assertEquals(grouped.sorted, list.sorted)
    }

  property("every key occupies exactly one contiguous block"):
    forAll(items) { list =>
      val keys = arrow.CachePeers.groupAdjacent(list)(_._1).map(_._1)
      // A key that appears in two separate blocks shows up twice in the
      // deduplicated run-length encoding of the key sequence.
      val blocks = keys.foldLeft(List.empty[Int]) {
        case (head :: tail, key) if head == key => head :: tail
        case (acc, key)                         => key :: acc
      }
      assertEquals(blocks.size, blocks.distinct.size)
    }

  property("order within a key is never disturbed"):
    forAll(items) { list =>
      val grouped = arrow.CachePeers.groupAdjacent(list)(_._1)
      list.map(_._1).distinct.foreach { key =>
        assertEquals(grouped.filter(_._1 == key), list.filter(_._1 == key))
      }
    }

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

  test("claude is never passed --system-prompt, which would silently void the flag"):
    // The failure mode ADR-0001 names and nothing guarded: claude documents
    // --exclude-dynamic-system-prompt-sections as ignored when --system-prompt
    // is present. Both flags would still be on the command line, every test
    // above would still pass, and the T19 ordering would quietly stop paying
    // off - with no error anywhere. Adding --system-prompt is a real design
    // option; doing it without noticing this is not.
    val command = commandFor("claude", "opus")
    assert(command.contains("--exclude-dynamic-system-prompt-sections"), command.toString)
    assert(
      !command.contains("--system-prompt"),
      "--system-prompt voids --exclude-dynamic-system-prompt-sections; see ADR-0001"
    )

  test("the opt-outs exist because an unrecognised flag is fatal, not a warning"):
    // scripts/remote-run.sh deploys to machines whose CLI versions this repo
    // does not control, so both knobs have to be switchable without a code
    // change. A default-on flag with no escape hatch is a deployment break.
    assertEquals(TaskRunner.claudeCacheFlags, Seq("--exclude-dynamic-system-prompt-sections"))
    assertEquals(TaskRunner.aiderCacheFlags, Seq("--cache-prompts"))

  test("no keepalive pings: single-shot runs have no later reader to keep warm"):
    assert(!commandFor("aider", "deepseek/deepseek-chat").contains("--cache-keepalive-pings"))

  test("vendors with no client-side knob get no invented flag"):
    // codex/gemini/agy cache server-side or not at all; passing something here
    // would be a guess that fails hard on an unrecognised flag.
    List("codex", "gemini", "agy").foreach { agent =>
      val command = commandFor(agent, "some-model")
      assert(!command.exists(_.contains("cache")), s"$agent: $command")
    }
