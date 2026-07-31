package com.github.mercurievv.ghllm.arrow

import com.github.mercurievv.ghllm.*
import com.github.mercurievv.ghllm.agent.*
import com.github.mercurievv.ghllm.arrow.*
import com.github.mercurievv.ghllm.cli.*
import com.github.mercurievv.ghllm.git.*
import com.github.mercurievv.ghllm.metrics.*

import cats.data.Kleisli
import cats.effect.IO
import munit.CatsEffectSuite

class EvaluationArrowsSuite extends CatsEffectSuite:

  private val runner = TaskRunner(
    agent = AgentBinary("claude"),
    model = Some("haiku"),
    effort = None,
    version = None
  )

  private def issue(body: String, number: Int = 1): Issue =
    Issue(
      number = TaskNumber(number),
      title = IssueTitle("t"),
      body = IssueBody(body),
      state = State("open")
    )

  // Deliberately NOT a GitHub checkout: these tests only exercise routing on
  // metadata that is already up to date, so no `gh` call may happen. If one
  // ever does, it fails here instead of writing to a real repository.
  private val sandboxRoot = os.temp.dir()

  private def preparedTask(task: Issue): PreparedTask =
    PreparedTask(
      claimedTask = ClaimedTask(
        context = RunContext(
          root = sandboxRoot,
          agentInventory = AgentInventory(Nil),
          taskNumber = None
        ),
        task = task,
        runner = runner,
        worktreePath = os.pwd,
        branchName = BranchName(s"task-${task.number}"),
        baseBranch = None
      ),
      parentConclusion = None,
      replayContext = None
    )

  // Routes through the real persist arrow. Bodies are chosen so the rendered
  // metadata is unchanged, which keeps the arrow from touching `gh`.
  private def route(
      evaluated: EvaluationArrows.EvaluatedTask,
      verified: EvaluationArrows.VerifiedSplit
  ): IO[(EvaluationArrows.Result, Option[String])] =
    for
      asked <- IO.ref(Option.empty[String])
      arrows = EvaluationArrows[IO](
        progress = _ => IO.unit,
        evaluatorRunner = runner,
        waitForUserInput = Kleisli { case (task, questions) =>
          asked.set(Some(questions)).as(Left(NeedsUserInput(task.claimedTask, Questions(questions))))
        }
      )
      result <- arrows.persistAndRouteEvaluation.run((evaluated, verified))
      questions <- asked.get
    yield (result, questions)

  private val readyBody =
    TaskMetadata
      .render(TaskMetadata(evaluation = Some("ready"), execution = Some("implement")))
      .value

  private val blockedBody =
    TaskMetadata
      .render(TaskMetadata(evaluation = Some("ready"), execution = Some("needs-input")))
      .value

  test("a consumed answer no longer suppresses the cached evaluation"):
    val answer = EvaluationArrows.UserInput("please use the v2 endpoint")
    val digest = EvaluationArrows.answerDigest(answer)
    val consumed = issue(
      TaskMetadata
        .render(
          TaskMetadata(
            evaluation = Some("ready"),
            execution = Some("implement"),
            answerConsumed = Some(digest)
          )
        )
        .value
    )
    assert(EvaluationArrows.answerAlreadyApplied(consumed, Some(answer)))
    // No answer at all is trivially "already applied".
    assert(EvaluationArrows.answerAlreadyApplied(consumed, None))

  test("a pending or different answer still forces a fresh evaluation"):
    val answer = EvaluationArrows.UserInput("please use the v2 endpoint")
    val unmarked = issue(readyBody)
    assert(!EvaluationArrows.answerAlreadyApplied(unmarked, Some(answer)))

    val staleMark = issue(
      TaskMetadata
        .render(
          TaskMetadata(
            evaluation = Some("ready"),
            execution = Some("implement"),
            answerConsumed = Some(EvaluationArrows.answerDigest(EvaluationArrows.UserInput("older answer")))
          )
        )
        .value
    )
    assert(!EvaluationArrows.answerAlreadyApplied(staleMark, Some(answer)))

  test("answerDigest ignores whitespace-only differences"):
    assertEquals(
      EvaluationArrows.answerDigest(EvaluationArrows.UserInput(" use   v2\nendpoint ")),
      EvaluationArrows.answerDigest(EvaluationArrows.UserInput("use v2 endpoint"))
    )

  test("re-routing an already-consumed answer is idempotent: implement, no rewrite"):
    val answer = EvaluationArrows.UserInput("use the v2 endpoint")
    val markedBody = TaskMetadata
      .render(
        TaskMetadata
          .parse(readyBody)
          .copy(answerConsumed = Some(EvaluationArrows.answerDigest(answer)))
      )
      .value
    val task = preparedTask(issue(markedBody))
    val evaluation = TaskEvaluation(IssueBody(markedBody), None, Execution.Implement)
    route(
      EvaluationArrows.EvaluatedTask(task, evaluation, Some(answer), replayedSplit = false),
      EvaluationArrows.VerifiedSplit(evaluation, repaired = false)
    ).map { case (result, _) =>
      // Routes to implementation, and persists nothing new (a metadata write
      // would hit `gh` in the sandbox root and fail this test).
      assert(result.toOption.flatMap(_.toOption).isDefined)
      assert(EvaluationArrows.answerAlreadyApplied(issue(markedBody), Some(answer)))
    }

  test("needs-input without questions blocks instead of routing to implement"):
    val needsInputBody = TaskMetadata
      .render(TaskMetadata(evaluation = Some("needs-input"), execution = Some("needs-input")))
      .value
    val task = preparedTask(issue(needsInputBody))
    val evaluation = TaskEvaluation(IssueBody(needsInputBody), None, Execution.NeedsInput)
    route(
      EvaluationArrows.EvaluatedTask(task, evaluation, None, replayedSplit = false),
      EvaluationArrows.VerifiedSplit(evaluation, repaired = false)
    ).map { case (result, questions) =>
      assert(clue(result).isLeft, "an evaluator that asked nothing must still block on the user")
      assert(questions.exists(_.nonEmpty), "a concrete question must be synthesized and posted")
    }

  test("contradictory ready/needs-input metadata replays as a real evaluation"):
    assertEquals(
      EvaluationArrows.completedEvaluation(issue(blockedBody), hasRealQuestion = false),
      None
    )
    // A genuine, question-backed block still replays as needs-input.
    assertEquals(
      EvaluationArrows
        .completedEvaluation(issue(blockedBody), hasRealQuestion = true)
        .map(_.execution),
      Some(Execution.NeedsInput)
    )
    // Actionable verdicts still replay unchanged.
    assertEquals(
      EvaluationArrows
        .completedEvaluation(issue(readyBody), hasRealQuestion = false)
        .map(_.execution),
      Some(Execution.Implement)
    )

  test("a repaired split is not treated as a pure replay"):
    val splitBody =
      TaskMetadata
        .render(TaskMetadata(evaluation = Some("ready"), execution = Some("split")))
        .value
    val task = preparedTask(issue(splitBody))
    val evaluation = TaskEvaluation(IssueBody(splitBody), None, Execution.Split)
    val evaluated = EvaluationArrows.EvaluatedTask(task, evaluation, None, replayedSplit = true)

    for
      repaired <- route(evaluated, EvaluationArrows.VerifiedSplit(evaluation, repaired = true))
      pure <- route(evaluated, EvaluationArrows.VerifiedSplit(evaluation, repaired = false))
    yield
      // Child issues were created in THIS run, so the split still needs its
      // split-evaluation comment.
      assertEquals(repaired._1.toOption.flatMap(_.swap.toOption).map(_.replayed), Some(false))
      assertEquals(pure._1.toOption.flatMap(_.swap.toOption).map(_.replayed), Some(true))

class EvaluatorOutputScopeSuite extends munit.FunSuite:

  private def prompt: String =
    EvaluationArrows
      .evaluateTaskPrompt(
        Issue(TaskNumber(7), IssueTitle("Fix the router"), IssueBody("Change the fallback"), State("open")),
        dependencyConclusion = None,
        agentInventory = AgentInventory(Nil),
        userAnswer = None
      )
      .value

  test("the evaluator is not asked to write the verdict metadata twice"):
    // persistAndRouteEvaluation always takes Evaluation/Execution from the JSON
    // status and ignores whatever the body says, so asking for those keys buys
    // output tokens that are then discarded.
    assert(!prompt.contains("write this metadata into the body"))
    assert(!prompt.contains("Execution: needs-input"))
    assert(prompt.contains("\"status\" field above, and that field alone"))

  test("but subtask bodies still carry their own verdict metadata"):
    // A subtask is a new issue with no metadata comments behind it, and
    // completedEvaluation reads Evaluation/Execution straight off that body to
    // skip re-evaluating a child the evaluator already scoped.
    assert(prompt.contains("Evaluation: ready"))
    assert(prompt.contains("Execution: implement"))

  test("the evaluator is told the executor preserves what it omits"):
    // TaskMetadata's Monoid falls back to the older layer per field, so
    // restating parent/dependency/runner lines is redundant - and, since the
    // newer layer wins when present, a mistranscription silently replaces them.
    assert(prompt.contains("Do NOT restate metadata you are not changing"))
    assert(!prompt.contains("Preserve existing parent/dependency references"))

  test("subtask metadata instructions are untouched"):
    // Subtasks are new issues the evaluator writes directly, so their Phase and
    // required-abilities blocks have no older layer to fall back on.
    assert(prompt.contains("Required abilities/importance:"))
    assert(prompt.contains("Phase: implement"))

  // The four defects POSTMORTEM-2026-07-31 documents. Two of them are checked
  // mechanically after the fact (AcceptanceCoverage); these instructions are
  // what lets the evaluator pass that check on the first attempt instead of
  // paying for a re-run, and they are the only handle on the two that cannot
  // be checked at all.
  test("the evaluator is told scope and acceptance are 1:1"):
    assert(prompt.contains("Scope and Acceptance Criteria are 1:1"))
    assert(prompt.contains("EVERY scope item gets its own acceptance criterion"))

  test("the evaluator is told to constrain values, not shape"):
    assert(prompt.contains("over the VALUES it accepts"))
    assert(prompt.contains("{plan, source-of-truth, implement, test}"))

  test("the evaluator is told to cross a serialisation boundary with a third subtask"):
    assert(prompt.contains("emit a THIRD subtask that crosses it"))

  test("the evaluator is told not to write a Files allowlist"):
    // It was guessed, it was wrong, and succeeding required ignoring it.
    assert(prompt.contains("Do not add a \"Files\" section"))

  test("a coverage repair is only in the prompt when there was a shortfall"):
    assert(!prompt.contains("Your previous attempt at this task was rejected"))
    val repaired = EvaluationArrows
      .evaluateTaskPrompt(
        Issue(TaskNumber(7), IssueTitle("Fix the router"), IssueBody("Change the fallback"), State("open")),
        dependencyConclusion = None,
        agentInventory = AgentInventory(Nil),
        userAnswer = None,
        coverageRepair = Some("3 scope item(s) but only 2 acceptance criterion(a)")
      )
      .value
    assert(repaired.contains("Your previous attempt at this task was rejected"))
    assert(repaired.contains("3 scope item(s) but only 2"))

class EvaluatorInventoryScopeSuite extends munit.FunSuite:

  private val inventory = AgentInventory(
    List(
      tool("cheap", "cheap-agent", 0.5, List("focused-fixes", "scala"), List("implement", "test")),
      tool("strong", "strong-agent", 30.0, List("complex-reasoning", "scala"), List("plan"))
    )
  )

  private val task =
    Issue(TaskNumber(7), IssueTitle("Fix the router"), IssueBody("Change the fallback"), State("open"))

  private def evaluate =
    EvaluationArrows.evaluateTaskPrompt(task, None, inventory, userAnswer = None).value

  private def split =
    EvaluationArrows.splitTaskPrompt(task, None, inventory).value

  test("both evaluation prompts carry the ability vocabulary"):
    // The vocabulary is the one thing the rules actually reference: an ability
    // name that matches nothing advertised never fires at selection time.
    for prompt <- List(evaluate, split) do
      assert(prompt.contains("complex-reasoning"), prompt)
      assert(prompt.contains("focused-fixes"), prompt)
      assert(prompt.contains("implement"), prompt)

  test("neither prompt ships per-tool prices or runner ids"):
    // Prices invite the evaluator to pick a runner, and a pin written during
    // evaluation beats run-time measured selection outright. Selection reads
    // cost and success rate itself; the evaluator does not need either.
    for prompt <- List(evaluate, split) do
      assert(!prompt.contains("cheap-agent"), prompt)
      assert(!prompt.contains("strong-agent"), prompt)
      assert(!prompt.contains("/task"), prompt)
      assert(!prompt.contains("budget="), prompt)

  test("the vocabulary is deduplicated and ordered"):
    // "scala" is advertised by both tools; repeating it is paid-for noise, and
    // a stable order keeps the prompt prefix cacheable across evaluations.
    assertEquals(
      inventory.abilityVocabulary,
      "complex-reasoning, focused-fixes, implement, plan, scala, test"
    )

  test("an empty inventory still names usable abilities"):
    val empty = AgentInventory(Nil).abilityVocabulary
    assert(empty.contains("complex-reasoning"), empty)

  private def tool(
      id: String,
      agent: String,
      price: Double,
      strengths: List[String],
      jobTypes: List[String]
  ): AgentTool =
    AgentTool(
      id = AgentToolId(id),
      agent = Agent(agent),
      model = Some(id),
      effort = None,
      version = None,
      roles = List("implementor"),
      jobTypes = jobTypes,
      strengths = strengths,
      available = Available(true),
      inputUsdPerMTok = Some(price),
      outputUsdPerMTok = Some(price)
    )
