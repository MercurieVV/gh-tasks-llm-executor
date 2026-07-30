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

  test("the mandate is appended, not substituted for the task body"):
    val body = "Change routeRunnerFallback in BusinessLogicRetry.scala"
    val prompt = promptFor("Fix the router", body)
    assert(prompt.contains(body))
    assert(prompt.indexOf(Impl.ScalaSemanticMandate) > prompt.indexOf(body))

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
