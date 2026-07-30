package com.github.mercurievv.ghllm.arrow

import com.github.mercurievv.ghllm.*
import com.github.mercurievv.ghllm.agent.*
import com.github.mercurievv.ghllm.arrow.*
import com.github.mercurievv.ghllm.cli.*
import com.github.mercurievv.ghllm.git.*
import com.github.mercurievv.ghllm.metrics.*

import cats.effect.IO
import cats.effect.Ref
import munit.CatsEffectSuite

class ScalaSemanticMandateSuite extends munit.FunSuite:
  private val runner = TaskRunner(AgentBinary("claude"), Some("opus"), None, None)

  private def promptFor(title: String, body: String): String =
    Impl
      .taskPrompt(
        Issue(TaskNumber(1), IssueTitle(title), IssueBody(body), State("open")),
        runner,
        None,
        None
      )
      .value

  test("a Scala task's prompt carries the ScalaSemantic mandate"):
    val prompt = promptFor("Fix the router", "Change routeRunnerFallback in BusinessLogicRetry.scala")
    assert(prompt.contains(Impl.ScalaSemanticMandate))

  test("a non-Scala task's prompt does not"):
    val prompt = promptFor("Update the README", "Document the new config flag in README.md")
    assert(!prompt.contains(Impl.ScalaSemanticMandate))

  test("the mandate triggers on the title too, not only the body"):
    // The gate is taskTouchesScala, shared with the SemanticDB refresh, so the
    // two Stage 3 features cannot disagree about what a Scala task is.
    val prompt = promptFor("Rename the enum in VerificationResult.scala", "See the linked discussion.")
    assert(prompt.contains(Impl.ScalaSemanticMandate))

  test("the mandate is added, not substituted for the task body"):
    val body = "Change routeRunnerFallback in BusinessLogicRetry.scala"
    val prompt = promptFor("Fix the router", body)
    assert(prompt.contains(body))
    // The mandate now PRECEDES the body: it is constant, and stable-first
    // ordering is what makes it cacheable (T19). See PromptSegmentOrderSuite.
    assert(prompt.indexOf(Impl.ScalaSemanticMandate) < prompt.indexOf(body))

class PromptSegmentOrderSuite extends munit.FunSuite:
  private val runner = TaskRunner(AgentBinary("claude"), Some("opus"), None, None)

  private def promptFor(number: Int, title: String, body: String): String =
    Impl
      .taskPrompt(
        Issue(TaskNumber(number), IssueTitle(title), IssueBody(body), State("open")),
        runner,
        dependencyConclusion = Some("parent decided X"),
        replayContext = Some("previous run failed CI")
      )
      .value

  test("segments run most-stable-first"):
    val prompt = promptFor(1, "Fix the router", "Change BusinessLogicRetry.scala")
    val expected = List(
      "Agent boundary:", // fully constant
      "Final answer contract:", // fully constant
      "Workflow:", // fully constant since the split instructions were removed
      "SCALA SEMANTIC NAVIGATION RULE", // constant, conditional on the task
      "Dependency Task Conclusion Comment:", // parent artifact
      "Replay / repair context:", // prior-run artifact
      "Task ID: #", // task instruction
      "Task Description:"
    )
    val positions = expected.map(marker => marker -> prompt.indexOf(marker))
    positions.foreach { case (marker, at) => assert(at >= 0, s"missing segment: $marker") }
    assertEquals(positions.sortBy(_._2).map(_._1), expected)

  test("two tasks on one runner share a prefix covering the constant segments"):
    // The point of the ordering, stated as a property: caching is prefix-only,
    // so what matters is not the index order but how many leading bytes two
    // sibling leaves have in common.
    val left = promptFor(1, "Fix the router", "Change BusinessLogicRetry.scala")
    val right = promptFor(2, "Rename a field", "Rename escalationDepth in Models.scala")
    val shared = left.zip(right).takeWhile((l, r) => l == r).map(_._1).mkString

    assert(shared.contains("Agent boundary:"))
    assert(shared.contains("Final answer contract:"))
    assert(shared.contains("SCALA SEMANTIC NAVIGATION RULE"))
    // Divergence begins no earlier than the first task-specific byte.
    assert(!shared.contains("Task Description:"))
    assert(!shared.contains("Fix the router"))

  test("a non-Scala task still leads with the unconditional constants"):
    // The mandate is conditional, so it must not sit ahead of the segments that
    // every task shares, or Scala and non-Scala tasks would diverge at byte one.
    val prompt = promptFor(3, "Update the README", "Document the flag in README.md")
    assert(!prompt.contains(Impl.ScalaSemanticMandate))
    assert(prompt.indexOf("Agent boundary:") < prompt.indexOf("Workflow:"))
    assert(prompt.startsWith("Agent boundary:"))

  test("the workflow block is shared by a Scala and a non-Scala sibling"):
    // Only possible because Workflow no longer interpolates the task number.
    val scalaTask = promptFor(1, "Fix the router", "Change BusinessLogicRetry.scala")
    val docsTask = promptFor(2, "Update the README", "Document the flag in README.md")
    val shared = scalaTask.zip(docsTask).takeWhile((l, r) => l == r).map(_._1).mkString
    assert(shared.contains("Workflow:"))

class ImplementerScopeSuite extends munit.FunSuite:
  private val runner = TaskRunner(AgentBinary("claude"), Some("opus"), None, None)

  private def prompt: String =
    Impl
      .taskPrompt(
        Issue(TaskNumber(42), IssueTitle("Fix the router"), IssueBody("Change the fallback"), State("open")),
        runner,
        None,
        None
      )
      .value

  test("the implementer is not asked to re-decide the split"):
    // The evaluator arrow already routed this task as Execution.Implement
    // (BusinessLogic.executeClaimedTask), so split instructions here are both
    // wasted prompt and an invitation to burn a run producing issue noise
    // instead of code.
    assert(!prompt.contains("Required abilities/importance:"))
    assert(!prompt.contains("When splitting, create GitHub subtasks"))
    assert(!prompt.contains("Prefer splitting"))
    assert(!prompt.contains("preferred llms/models/efforts/versions"))

  test("the implementer is told to implement, and how to refuse"):
    assert(prompt.contains("Do not split the task"))
    assert(prompt.contains("make NO file changes"))

  test("the workflow no longer interpolates the task number"):
    val workflow = prompt.substring(prompt.indexOf("Workflow:"), prompt.indexOf("Task ID: #"))
    assert(!workflow.contains("#42"), workflow)

class SemanticDbMemoizationSuite extends CatsEffectSuite:
  test("refresh runs once across two dispatches with unchanged source hash"):
    for
      issues <- Ref[IO].of(Map.empty[TaskNumber, Issue])
      env <- RunEnv.create[IO](issues)
      count <- Ref[IO].of(0)
      one = SemanticDbSource(os.pwd, "one")
      two = SemanticDbSource(os.pwd, "two")
      _ <- Impl.refreshSemanticDbIfNeeded(env, one)(count.update(_ + 1))
      _ <- Impl.refreshSemanticDbIfNeeded(env, one)(count.update(_ + 1))
      unchanged <- count.get
    yield assert(unchanged == 1, s"expected 1 refresh, got $unchanged")

  test("refresh runs twice when source hash changes"):
    for
      issues <- Ref[IO].of(Map.empty[TaskNumber, Issue])
      env <- RunEnv.create[IO](issues)
      count <- Ref[IO].of(0)
      one = SemanticDbSource(os.pwd, "one")
      two = SemanticDbSource(os.pwd, "two")
      _ <- Impl.refreshSemanticDbIfNeeded(env, one)(count.update(_ + 1))
      _ <- Impl.refreshSemanticDbIfNeeded(env, one)(count.update(_ + 1))
      unchanged <- count.get
      _ <- Impl.refreshSemanticDbIfNeeded(env, two)(count.update(_ + 1))
      changed <- count.get
    yield assert(changed == 2, s"expected 2 refreshes, got $changed")

class FanOutCachePeersSuite extends CatsEffectSuite:

  private def issue(number: Int, body: String = ""): Issue =
    Issue(TaskNumber(number), IssueTitle(s"Task $number"), IssueBody(body), State("open"))

  private def context: RunContext =
    RunContext(os.pwd, AgentInventory(Nil), taskNumber = None)

  private def pendingTtl(dependencyCount: Int): IO[List[Boolean]] =
    val children = (2 to dependencyCount + 1).toList
    val parent = issue(1, s"Depends on ${children.map(n => s"#$n").mkString(", ")}")
    for
      issues <- Ref[IO].of((parent :: children.map(issue(_))).map(i => i.number -> i).toMap)
      env <- RunEnv.create[IO](issues)
      plan <- Impl.collectPendingDependencies[IO].run(TaskNode(context, parent)).run(env)
    yield plan.pending.map(_.extendedCacheTtl)

  test("three siblings routing to one runner earn the extended TTL"):
    pendingTtl(3).map(ttl => assertEquals(ttl, List(true, true, true)))

  test("two siblings do not"):
    pendingTtl(2).map(ttl => assertEquals(ttl, List(false, false)))

  test("a lone dependency does not"):
    pendingTtl(1).map(ttl => assertEquals(ttl, List(false)))

  test("the parent node itself is not marked by its children"):
    // extendedCacheTtl travels with the sibling set, not with the task that
    // owns it: the parent has no peers here.
    val parent = issue(1, "Depends on #2, #3, #4")
    for
      issues <- Ref[IO].of(
        (parent :: List(2, 3, 4).map(issue(_))).map(i => i.number -> i).toMap
      )
      env <- RunEnv.create[IO](issues)
      plan <- Impl.collectPendingDependencies[IO].run(TaskNode(context, parent)).run(env)
    yield assert(!plan.node.extendedCacheTtl)
