package com.github.mercurievv.ghllm.task

import com.github.mercurievv.ghllm.*
import com.github.mercurievv.ghllm.TaskTree.NodeRef
import com.github.mercurievv.ghllm.agent.*
import com.github.mercurievv.ghllm.cli.*
import com.github.mercurievv.ghllm.git.*
import com.github.mercurievv.ghllm.metrics.*

import cats.effect.IO
import cats.effect.Ref
import munit.CatsEffectSuite

class PlanEstimateSuite extends CatsEffectSuite:

  private val context = RunContext(os.pwd, AgentInventory(Nil), None)

  private val costModel =
    TaskTree.CostModel(inputUsdPerMillionTokens = 1.0, outputUsdPerMillionTokens = 10.0)

  private def issue(number: Int, body: String) =
    Issue(TaskNumber(number), IssueTitle(s"Task $number"), IssueBody(body), State("open"))

  private def event(task: Int, phase: String, input: Long, output: Long) =
    TokenMetrics.TokenMetricsEvent(
      timestampMillis = 0L,
      vendor = TokenUsage.Vendor.Claude,
      usage = TokenUsage.TokenSnapshot(input, output, 0L, 0L, input + output),
      taskNumber = Some(TaskNumber(task)),
      model = Some("m"),
      scope = "task",
      phase = Some(phase),
      runner = Some("codex/mini")
    )

  test("a plan is priced end to end, from open issues to a rendered report"):
    // Root #1 depends on #2, which depends on #3. Only the executor's own
    // discovery decides that; the estimate reads the same definition.
    val root = issue(1, "Task metadata:\nPhase: plan\nDepends on #2")
    val middle = issue(2, "Task metadata:\nPhase: implement\nDepends on #3")
    val leaf = issue(3, "Task metadata:\nPhase: test")
    val events = List(
      event(1, "plan", input = 1000000L, output = 0L), // $1.00
      event(2, "implement", input = 0L, output = 100000L), // $1.00
      event(3, "test", input = 500000L, output = 0L) // $0.50
    )

    for
      openIssues <- Ref[IO].of(
        Map(TaskNumber(1) -> root, TaskNumber(2) -> middle, TaskNumber(3) -> leaf)
      )
      env <- IO.pure(RunEnv(openIssues))
      annotated <- PlanEstimate
        .annotate[IO](
          TaskNode(context, root),
          costModel,
          NodeProfiles.fromEvents(events, os.pwd)
        )
        .run(env)
    yield
      assertEqualsDouble(annotated.head.cost.ownUsd, 1.0, 1e-9)
      assertEqualsDouble(annotated.head.cost.subtreeUsd, 2.5, 1e-9)
      assertEquals(annotated.head.cost.nodeCount, 3)
      assertEquals(annotated.head.tier, "plan")

      val report = PlanEstimate.render(annotated, costModel)
      assert(report.contains("3 nodes"), report)
      assert(report.contains("#1 [plan]"), report)
      assert(report.contains("#2 [implement]"), report)
      assert(report.contains("#3 [test]"), report)
      // Prices are echoed, so a figure can never be read without its inputs.
      assert(report.contains("$1.00/Mtok in"), report)
      assert(report.contains("$10.00/Mtok out"), report)

  test("nesting in the report follows the dependency depth"):
    val root = issue(1, "Depends on #2")
    val child = issue(2, "")
    for
      openIssues <- Ref[IO].of(Map(TaskNumber(1) -> root, TaskNumber(2) -> child))
      annotated <- PlanEstimate
        .annotate[IO](TaskNode(context, root), costModel, NodeProfiles.fromEvents(Nil, os.pwd))
        .run(RunEnv(openIssues))
    yield
      val body = PlanEstimate.render(annotated, costModel).linesIterator.toList
      assert(body.exists(line => line.startsWith("#1 ")), body.mkString("\n"))
      assert(body.exists(line => line.startsWith("  #2 ")), body.mkString("\n"))

class EstimateCommandSuite extends munit.FunSuite:

  test("estimate parses a task and defaults its prices"):
    val command = Cli.parseEstimateCommand(List("estimate", "--task=42"), os.pwd)

    assertEquals(command.map(_.task), Some(TaskNumber(42)))
    assertEquals(
      command.map(_.costModel.inputUsdPerMillionTokens),
      Some(Cli.DefaultInputUsdPerMillionTokens)
    )

  test("prices are overridable"):
    val command = Cli.parseEstimateCommand(
      List("estimate", "--task=1", "--input-usd-per-mtok=0.5", "--output-usd-per-mtok", "7"),
      os.pwd
    )

    assertEquals(command.map(_.costModel.inputUsdPerMillionTokens), Some(0.5))
    assertEquals(command.map(_.costModel.outputUsdPerMillionTokens), Some(7.0))

  test("a nonsensical price is refused rather than silently used"):
    val command =
      Cli.parseEstimateCommand(List("estimate", "--task=1", "--input-usd-per-mtok=abc"), os.pwd)

    assertEquals(
      command.map(_.costModel.inputUsdPerMillionTokens),
      Some(Cli.DefaultInputUsdPerMillionTokens)
    )

  test("estimate without a task is not an estimate command"):
    assertEquals(Cli.parseEstimateCommand(List("estimate"), os.pwd), None)

  test("other invocations are untouched"):
    assertEquals(Cli.parseEstimateCommand(List("--task=1"), os.pwd), None)
    assertEquals(Cli.parseEstimateCommand(List("metrics", "--task=1"), os.pwd), None)

  test("estimate args never leak into the agent's argv"):
    assertEquals(Cli.removeScriptArgs(List("estimate", "--task=1", "--input-usd-per-mtok=2")), Nil)
