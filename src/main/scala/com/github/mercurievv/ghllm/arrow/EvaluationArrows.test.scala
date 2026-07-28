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
