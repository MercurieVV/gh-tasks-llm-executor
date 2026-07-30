package com.github.mercurievv.ghllm.task

import com.github.mercurievv.ghllm.*
import com.github.mercurievv.ghllm.TaskTree.NodeRef
import com.github.mercurievv.ghllm.agent.*
import com.github.mercurievv.ghllm.cli.*
import com.github.mercurievv.ghllm.git.*
import com.github.mercurievv.ghllm.metrics.*
import com.github.mercurievv.ghllm.metrics.TokenMetrics.TokenMetricsEvent

import cats.effect.IO
import higherkindness.droste.data.Mu
import munit.CatsEffectSuite

class NodeProfilesSuite extends CatsEffectSuite:

  private def snapshot(input: Long, output: Long, cacheRead: Long = 0L, cacheWrite: Long = 0L) =
    TokenUsage.TokenSnapshot(input, output, cacheRead, cacheWrite, input + output)

  private def event(
      task: Option[Int],
      phase: Option[String],
      input: Long,
      output: Long,
      runner: Option[String] = Some("codex/gpt"),
      model: Option[String] = Some("m1")
  ) =
    TokenMetricsEvent(
      timestampMillis = 0L,
      vendor = TokenUsage.Vendor.Claude,
      usage = snapshot(input, output),
      taskNumber = task.map(TaskNumber(_)),
      model = model,
      scope = "task",
      phase = phase,
      runner = runner,
      turnCount = Some(3)
    )

  private val model =
    TaskTree.CostModel(inputUsdPerMillionTokens = 1.0, outputUsdPerMillionTokens = 10.0)

  test("a task's own measurements win over its phase's"):
    val events = List(
      event(Some(7), Some("implement"), input = 1000, output = 0),
      event(Some(7), Some("implement"), input = 3000, output = 0),
      event(Some(8), Some("implement"), input = 999999, output = 0)
    )
    val profile = NodeProfiles.fromEvents(events, os.pwd)(NodeRef.Task(7, Some("implement")))

    // Mean over task 7's two runs, not dragged toward task 8.
    assertEqualsDouble(profile.coefficients.inputTokens, 2000.0, 1e-9)
    assertEquals(profile.tier, "implement")

  test("a task that has never run is priced from its phase"):
    val events = List(
      event(Some(1), Some("test"), input = 100, output = 10),
      event(Some(2), Some("test"), input = 300, output = 30),
      event(Some(3), Some("plan"), input = 999999, output = 0)
    )
    val profile = NodeProfiles.fromEvents(events, os.pwd)(NodeRef.Task(42, Some("test")))

    assertEqualsDouble(profile.coefficients.inputTokens, 200.0, 1e-9)
    assertEqualsDouble(profile.coefficients.outputTokens, 20.0, 1e-9)
    assertEquals(profile.tier, "test")

  test("phase matching ignores case and surrounding whitespace"):
    val events = List(event(Some(1), Some("Implement"), input = 500, output = 0))
    val profile = NodeProfiles.fromEvents(events, os.pwd)(NodeRef.Task(9, Some("  implement ")))

    assertEqualsDouble(profile.coefficients.inputTokens, 500.0, 1e-9)

  test("an unseen phase falls back to the global mean rather than to zero"):
    val events = List(
      event(Some(1), Some("plan"), input = 100, output = 0),
      event(Some(2), Some("test"), input = 300, output = 0)
    )
    val profile = NodeProfiles.fromEvents(events, os.pwd)(NodeRef.Task(9, Some("source-of-truth")))

    assertEqualsDouble(profile.coefficients.inputTokens, 200.0, 1e-9)

  test("no history at all estimates zero rather than guessing"):
    // A zero estimate is visibly useless. An invented one reads as a forecast.
    val profile = NodeProfiles.fromEvents(Nil, os.pwd)(NodeRef.Task(1, Some("implement")))

    assertEqualsDouble(model.estimate(profile.coefficients), 0.0, 1e-12)
    assertEquals(profile.tier, "implement")

  test("nodes of the same phase share a tier, different phases do not"):
    // Same-phase nodes are the ones that actually share a cached prefix, and
    // `tier` is what carries that grouping. It used to be restated as a SHA-256
    // `stablePrefixHash` over ("system", "repo", tier); that hash was removed
    // because it was a second copy of this field and nothing cached on it.
    val events = List(
      event(Some(1), Some("plan"), input = 10, output = 0),
      event(Some(2), Some("test"), input = 10, output = 0)
    )
    val profileFor = NodeProfiles.fromEvents(events, os.pwd)
    val plan = profileFor(NodeRef.Task(1, Some("plan")))
    val alsoPlan = profileFor(NodeRef.Task(3, Some("plan")))
    val test = profileFor(NodeRef.Task(2, Some("test")))

    assertEquals(plan.tier, alsoPlan.tier)
    assertNotEquals(plan.tier, test.tier)

  test("the reported runner and model are the sample's most frequent"):
    val events = List(
      event(Some(1), Some("plan"), 10, 0, runner = Some("gemini/x"), model = Some("g")),
      event(Some(1), Some("plan"), 10, 0, runner = Some("codex/y"), model = Some("c")),
      event(Some(1), Some("plan"), 10, 0, runner = Some("codex/y"), model = Some("c"))
    )
    val profile = NodeProfiles.fromEvents(events, os.pwd)(NodeRef.Task(1, Some("plan")))

    assertEquals(profile.prefixKey.runner, "codex/y")
    assertEquals(profile.prefixKey.model, Some("c"))

  test("turnCount stays a diagnostic and is not multiplied into the cost"):
    // Token counts on an event are already cumulative over that run's turns.
    val events = List(event(Some(1), Some("plan"), input = 1000000, output = 0))
    val profile = NodeProfiles.fromEvents(events, os.pwd)(NodeRef.Task(1, Some("plan")))

    assertEqualsDouble(profile.coefficients.turnCount, 3.0, 1e-9)
    assertEqualsDouble(model.estimate(profile.coefficients), 1.0, 1e-9)

class TaskGraphCostBridgeSuite extends CatsEffectSuite:

  private val context = RunContext(os.pwd, AgentInventory(Nil), None)

  private def issue(number: Int, body: String = "") =
    Issue(TaskNumber(number), IssueTitle(s"Task $number"), IssueBody(body), State("open"))

  private def node(number: Int, body: String = "") = TaskNode(context, issue(number, body))

  private def pendingFrom(edges: Map[Int, (String, List[Int])]): TaskNode => IO[List[TaskNode]] =
    task =>
      IO.pure(
        edges
          .get(task.issue.number.value)
          .map(_._2)
          .getOrElse(Nil)
          .map(child => node(child, edges.get(child).map(_._1).getOrElse("")))
      )

  private def refsOf(tree: TaskTree.Tree): List[TaskTree.NodeRef] =
    higherkindness.droste.scheme
      .cata[TaskTree.Node, TaskTree.Tree, List[TaskTree.NodeRef]](
        TaskTree.Algebra(n => TaskF.payloadOf(n) :: TaskF.childrenOf(n).flatten)
      )
      .apply(tree)

  test("a live dependency tree retags into cost identities, phase included"):
    val edges = Map(
      1 -> ("Task metadata:\nPhase: plan", List(2, 3)),
      2 -> ("Task metadata:\nPhase: implement", Nil),
      3 -> ("Task metadata:\nPhase: test", Nil)
    )
    TaskGraph
      .unfold[IO](pendingFrom(edges))(TaskGraph.seed(node(1, edges(1)._1)))
      .map { graph =>
        assertEquals(
          refsOf(TaskTree.ofTaskGraph(graph)),
          List(
            NodeRef.Task(1, Some("plan")),
            NodeRef.Task(2, Some("implement")),
            NodeRef.Task(3, Some("test"))
          )
        )
      }

  test("a task with no Phase metadata retags with no phase"):
    TaskGraph
      .unfold[IO](pendingFrom(Map.empty))(TaskGraph.seed(node(5)))
      .map(graph => assertEquals(refsOf(TaskTree.ofTaskGraph(graph)), List(NodeRef.Task(5, None))))

  test("the identity does not depend on whether the node came out a branch or a leaf"):
    // A task is a branch while dependencies are open and a leaf once they
    // close. Keying the profile on shape would reprice it as work lands.
    val body = "Task metadata:\nPhase: implement"
    for
      withChild <- TaskGraph.unfold[IO](pendingFrom(Map(1 -> (body, List(2)))))(
        TaskGraph.seed(node(1, body))
      )
      alone <- TaskGraph.unfold[IO](pendingFrom(Map.empty))(TaskGraph.seed(node(1, body)))
    yield assertEquals(refsOf(TaskTree.ofTaskGraph(withChild)).head, refsOf(TaskTree.ofTaskGraph(alone)).head)

  test("retagging preserves shape, so a diamond is still counted twice"):
    val edges = Map(
      1 -> ("", List(2, 3)),
      2 -> ("", List(4)),
      3 -> ("", List(4)),
      4 -> ("", Nil)
    )
    TaskGraph
      .unfold[IO](pendingFrom(edges))(TaskGraph.seed(node(1)))
      .map { graph =>
        val refs = refsOf(TaskTree.ofTaskGraph(graph))
        assertEquals(refs.count(_ == NodeRef.Task(4, None)), 2)
        assertEquals(refs.size, 5)
      }

  test("a cost estimate over a real unfolded plan uses recorded coefficients"):
    // End to end: GitHub-shaped discovery -> tree -> retag -> measured
    // profiles -> USD. Every link in that chain existed except the ones added
    // here, which is why the cost model had never priced a real plan.
    val edges = Map(
      1 -> ("Task metadata:\nPhase: plan", List(2)),
      2 -> ("Task metadata:\nPhase: implement", Nil)
    )
    val events = List(
      TokenMetrics.TokenMetricsEvent(
        timestampMillis = 0L,
        vendor = TokenUsage.Vendor.Claude,
        usage = TokenUsage.TokenSnapshot(1000000L, 0L, 0L, 0L, 1000000L),
        taskNumber = Some(TaskNumber(1)),
        model = Some("m"),
        scope = "task",
        phase = Some("plan"),
        runner = Some("claude/opus")
      ),
      TokenMetrics.TokenMetricsEvent(
        timestampMillis = 0L,
        vendor = TokenUsage.Vendor.Claude,
        usage = TokenUsage.TokenSnapshot(0L, 100000L, 0L, 0L, 100000L),
        taskNumber = Some(TaskNumber(2)),
        model = Some("m"),
        scope = "task",
        phase = Some("implement"),
        runner = Some("codex/mini")
      )
    )
    val costModel =
      TaskTree.CostModel(inputUsdPerMillionTokens = 1.0, outputUsdPerMillionTokens = 10.0)

    TaskGraph
      .unfold[IO](pendingFrom(edges))(TaskGraph.seed(node(1, edges(1)._1)))
      .map { graph =>
        val cost =
          TaskTree.estimate(
            TaskTree.ofTaskGraph(graph),
            costModel,
            NodeProfiles.fromEvents(events, os.pwd)
          )
        assertEqualsDouble(cost.ownUsd, 1.0, 1e-9) // plan: 1M input @ $1/M
        assertEqualsDouble(cost.subtreeUsd, 2.0, 1e-9) // + implement: 100k output @ $10/M
        assertEquals(cost.nodeCount, 2)
      }
