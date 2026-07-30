package com.github.mercurievv.ghllm.arrow

import com.github.mercurievv.ghllm.*
import com.github.mercurievv.ghllm.agent.*
import com.github.mercurievv.ghllm.arrow.*
import com.github.mercurievv.ghllm.cli.*
import com.github.mercurievv.ghllm.git.*
import com.github.mercurievv.ghllm.metrics.*

import cats.arrow.Arrow
import cats.Monoid
import cats.data.Kleisli
import cats.data.OptionT
import cats.effect.kernel.Sync
import cats.syntax.all.*
import cats.syntax.arrow.*

import scala.util.Try

final case class EvaluationArrows[-->[_, _]](
    fetchMaybeUserAnswer: PreparedTask --> Option[EvaluationArrows.UserInput],
    evaluateOrGetCached: EvaluationArrows.EvaluationInput --> EvaluationArrows.EvaluatedTask,
    verifySplitExists: EvaluationArrows.EvaluatedTask --> EvaluationArrows.VerifiedSplit,
    persistAndRouteEvaluation: EvaluationArrows.VerifiedEvaluation --> EvaluationArrows.Result
):
  def evaluateTask(using Arrow[-->]): PreparedTask --> EvaluationArrows.Result =
    (Arrow[-->].id &&& fetchMaybeUserAnswer) >>>
      evaluateOrGetCached >>>
      (Arrow[-->].id &&& verifySplitExists) >>>
      persistAndRouteEvaluation

final case class CachedEvaluationArrows[-->[_, _]](
    evaluate: EvaluationArrows.EvaluationInput --> EvaluationArrows.EvaluatedTask
)

object CachedEvaluationArrows:
  def apply[F[_]: Sync](
      progress: String => F[Unit]
  ): CachedEvaluationArrows[EvaluationArrows.OptionalArrow[F]] =
    CachedEvaluationArrows(
      evaluate = Kleisli[[X] =>> OptionT[F, X], EvaluationArrows.EvaluationInput, EvaluationArrows.EvaluatedTask] {
        case (task, userAnswer) =>
          val claimedTask = task.claimedTask
          OptionT(
            for
              hasRealQuestion <- GitHub.hasQuestionComment(
                claimedTask.context.root,
                claimedTask.task
              )
              replayedEvaluation = EvaluationArrows
                .completedEvaluation(claimedTask.task, hasRealQuestion)
                // A pending (not yet folded in) user answer must force a fresh
                // evaluation. An answer already marked consumed must NOT: it
                // stays the latest comment forever, and re-evaluating on it
                // would re-run the evaluator LLM on every later replay.
                .filter(_ => EvaluationArrows.answerAlreadyApplied(claimedTask.task, userAnswer))
              result <- replayedEvaluation.traverse { cachedEvaluation =>
                progress(
                  s"Reusing existing evaluation for #${claimedTask.task.number}."
                ).as(
                  EvaluationArrows.EvaluatedTask(
                    task = task,
                    evaluation = cachedEvaluation,
                    userAnswer = userAnswer,
                    replayedSplit = cachedEvaluation.execution == Execution.Split
                  )
                )
              }
            yield result
          )
      }
    )

final case class AgenticEvaluationArrows[EvalArrow[_, _], VerifyArrow[_, _]](
    evaluate: EvalArrow[EvaluationArrows.EvaluationInput, EvaluationArrows.EvaluatedTask],
    verifySplitExists: VerifyArrow[EvaluationArrows.EvaluatedTask, EvaluationArrows.VerifiedSplit]
)

object AgenticEvaluationArrows:
  def apply[F[_]: Sync](
      progress: String => F[Unit],
      evaluatorRunner: TaskRunner
  ): AgenticEvaluationArrows[EvaluationArrows.OptionalArrow[F], [A, B] =>> Kleisli[F, A, B]] =
    def runEvaluator(task: PreparedTask, userAnswer: Option[String]): F[TaskEvaluation] =
      val claimedTask = task.claimedTask
      progress(
        s"Evaluating task #${claimedTask.task.number} with ${evaluatorRunner.display}..."
      ) >> AgentExecutor[F]
        .run(
          evaluatorRunner,
          EvaluationArrows.evaluateTaskPrompt(
            claimedTask.task,
            task.parentConclusion,
            claimedTask.context.agentInventory,
            userAnswer
          ),
          claimedTask.context.root,
          EvaluationArrows.EvaluatorAllowedTools,
          Some(EvaluationArrows.EvaluationJsonSchema)
        )
        .map(EvaluationArrows.parseTaskEvaluation(_, claimedTask.task.body))

    def createMissingSplitSubtasks(
        task: PreparedTask,
        evaluation: TaskEvaluation
    ): F[Unit] =
      val claimedTask = task.claimedTask
      progress(
        s"Creating missing split subtasks for already-evaluated task #${claimedTask.task.number} with ${evaluatorRunner.display}..."
      ) >>
        AgentExecutor[F]
          .run(
            evaluatorRunner,
            EvaluationArrows.splitTaskPrompt(
              claimedTask.task.copy(body = evaluation.body),
              task.parentConclusion,
              claimedTask.context.agentInventory
            ),
            claimedTask.context.root,
            EvaluationArrows.EvaluatorAllowedTools
          )
          .void

    val evaluate: Kleisli[[X] =>> OptionT[F, X], EvaluationArrows.EvaluationInput, EvaluationArrows.EvaluatedTask] =
      Kleisli { case (task, userAnswer) =>
        OptionT.liftF(
          runEvaluator(task, userAnswer.map(_.value)).map(evaluation =>
            EvaluationArrows.EvaluatedTask(
              task = task,
              evaluation = evaluation,
              userAnswer = userAnswer,
              replayedSplit = false
            )
          )
        )
      }

    val verifySplitExists
        : Kleisli[[X] =>> OptionT[F, X], EvaluationArrows.EvaluatedTask, EvaluationArrows.VerifiedSplit] =
      Kleisli { evaluated =>
        val task = evaluated.task
        val evaluation = evaluated.evaluation
        OptionT.liftF {
          val claimedTask = task.claimedTask
          val unrepaired = EvaluationArrows.VerifiedSplit(evaluation, repaired = false)
          if evaluation.execution == Execution.Split then
            GitHub.fetchIssues(claimedTask.context.root).flatMap { allIssues =>
              if GitHub.hasOpenChildren(claimedTask.task, allIssues) then unrepaired.pure[F]
              else
                // The task has already been evaluated. Do not call the
                // evaluator again; complete the side effect the split verdict
                // required, then verify it produced linked child issues.
                GitHub
                  .commentSplitMissingWarning(progress)(
                    claimedTask.context.root,
                    claimedTask.task
                  ) *>
                  progress(
                    s"Split expected for task #${claimedTask.task.number}, but no child issues exist; creating missing split subtasks without re-evaluating..."
                  ) *>
                  createMissingSplitSubtasks(task, evaluation) *>
                  GitHub.fetchIssues(claimedTask.context.root).flatMap { updatedIssues =>
                    if GitHub.hasOpenChildren(claimedTask.task, updatedIssues) then
                      EvaluationArrows.VerifiedSplit(evaluation, repaired = true).pure[F]
                    else
                      Sync[F].raiseError(
                        RuntimeException(
                          s"Task #${claimedTask.task.number} is already evaluated as split, but missing split-subtask creation produced no linked child issues."
                        )
                      )
                  }
            }
          else unrepaired.pure[F]
        }
      }

    AgenticEvaluationArrows(
      evaluate = evaluate,
      verifySplitExists = Kleisli[F, EvaluationArrows.EvaluatedTask, EvaluationArrows.VerifiedSplit] { evaluatedTask =>
        verifySplitExists
          .run(evaluatedTask)
          .getOrElseF(
            Sync[F].raiseError(
              RuntimeException("Agentic split verification did not produce a result.")
            )
          )
      }
    )

object EvaluationArrows:
  opaque type UserInput = String
  object UserInput:
    def apply(value: String): UserInput = value
    extension (self: UserInput) def value: String = self

  type EvaluationInput = (PreparedTask, Option[UserInput])

  /** Outcome of the cached-or-agentic evaluation step.
    *
    * `userAnswer` is threaded through so the persist step can durably mark it consumed; `replayedSplit` records that
    * the split verdict came from cached metadata rather than a fresh evaluator run.
    */
  final case class EvaluatedTask(
      task: PreparedTask,
      evaluation: TaskEvaluation,
      userAnswer: Option[UserInput],
      replayedSplit: Boolean
  )

  /** A split verdict after its child issues were confirmed to exist. `repaired` means this run had to create the
    * missing child issues, so the split is not a pure replay even when the verdict itself was cached.
    */
  final case class VerifiedSplit(evaluation: TaskEvaluation, repaired: Boolean)

  type VerifiedEvaluation = (EvaluatedTask, VerifiedSplit)
  type Result = Either[NeedsUserInput, Either[SplitTask, PreparedTask]]
  type OptionalArrow[F[_]] = [A, B] =>> Kleisli[[X] =>> OptionT[F, X], A, B]
  // Scoped tool permission for the evaluator run only: "split" verdicts
  // require it to actually create child issues, which a non-interactive
  // `claude -p` run otherwise can't do -- there's no TTY to approve the Bash
  // call, so it stalls out asking for confirmation instead of producing the
  // required JSON. `gh issue edit` is deliberately NOT granted: that would
  // let the evaluator rewrite an issue body directly, bypassing the
  // comment-only TaskMetadata architecture (see taskMetadata.scala) the same
  // way the old updateIssueBody call used to. Linking a new subtask back to
  // its parent should go through `gh issue comment` instead.
  val EvaluatorAllowedTools = Seq(
    "Bash(gh issue create:*)",
    "Bash(gh issue comment:*)"
  )

  // Forces the claude CLI's final response to conform to this shape (via
  // --json-schema) instead of relying on the prompt's "Return only JSON"
  // instruction and hoping the model complies. This is what parseTaskEvaluation
  // expects; matching it here should turn most JSON-parse failures into
  // guaranteed-valid output instead. Only "claude" consumes jsonSchema
  // (TaskRunner.command) -- a no-op for other evaluator runners.
  val EvaluationJsonSchema =
    """{"type":"object","properties":{"status":{"type":"string","enum":["ready","split","questions"]},"body":{"type":"string"},"questions":{"type":"string"}},"required":["status","body"]}"""

  def apply[F[_]: Sync](
      progress: String => F[Unit],
      evaluatorRunner: TaskRunner,
      waitForUserInput: Kleisli[F, (PreparedTask, String), Result]
  ): EvaluationArrows[[A, B] =>> Kleisli[F, A, B]] =
    val fetchMaybeUserAnswer: Kleisli[F, PreparedTask, Option[UserInput]] =
      Kleisli { task =>
        val claimedTask = task.claimedTask
        GitHub.userAnswer(progress)(claimedTask.context.root, claimedTask.task)
      }

    val agenticEvaluation = AgenticEvaluationArrows[F](
      progress = progress,
      evaluatorRunner = evaluatorRunner
    )
    val evaluateTask: Kleisli[F, EvaluationInput, EvaluatedTask] =
      Kleisli { input =>
        agenticEvaluation.evaluate
          .run(input)
          .getOrElseF(
            Sync[F].raiseError(
              RuntimeException("Agentic task evaluation did not produce a result.")
            )
          )
      }

    EvaluationArrows(
      fetchMaybeUserAnswer = fetchMaybeUserAnswer,
      evaluateOrGetCached = evaluateTask,
      verifySplitExists = agenticEvaluation.verifySplitExists,
      persistAndRouteEvaluation = persistAndRouteEvaluation(progress, waitForUserInput)
    )

  def replay[F[_]: Sync](
      progress: String => F[Unit],
      evaluatorRunner: TaskRunner,
      waitForUserInput: Kleisli[F, (PreparedTask, String), Result]
  ): EvaluationArrows[OptionalArrow[F]] =
    val fetchMaybeUserAnswer: Kleisli[[X] =>> OptionT[F, X], PreparedTask, Option[UserInput]] =
      Kleisli { task =>
        val claimedTask = task.claimedTask
        OptionT.liftF(GitHub.userAnswer(progress)(claimedTask.context.root, claimedTask.task))
      }

    val cachedEvaluation = CachedEvaluationArrows[F](progress)
    val agenticEvaluation = AgenticEvaluationArrows[F](
      progress = progress,
      evaluatorRunner = evaluatorRunner
    )

    EvaluationArrows(
      fetchMaybeUserAnswer = fetchMaybeUserAnswer,
      evaluateOrGetCached = cachedEvaluation.evaluate,
      verifySplitExists = Kleisli { evaluated =>
        OptionT.liftF(agenticEvaluation.verifySplitExists.run(evaluated))
      },
      persistAndRouteEvaluation = Kleisli { verified =>
        OptionT.liftF(persistAndRouteEvaluation(progress, waitForUserInput).run(verified))
      }
    )

  private def persistAndRouteEvaluation[F[_]: Sync](
      progress: String => F[Unit],
      waitForUserInput: Kleisli[F, (PreparedTask, String), Result]
  ): Kleisli[F, VerifiedEvaluation, Result] =
    Kleisli { case (evaluated, VerifiedSplit(verifiedEvaluation, repairedSplit)) =>
      val task = evaluated.task
      val claimedTask = task.claimedTask
      val cleanBody = stripMarkdownFence(verifiedEvaluation.body.value).trim
      // Questions win over the verdict: a blocked task must never be routed
      // to implement. Execution.NeedsInput without any questions is an
      // evaluator defect, so synthesize a real question instead of silently
      // falling through to implementation.
      val questions = verifiedEvaluation.questions
        .map(_.trim)
        .filter(_.nonEmpty)
        .orElse(
          Option.when(verifiedEvaluation.execution == Execution.NeedsInput)(
            s"""Evaluation of task #${claimedTask.task.number} ended as "needs-input" but produced no concrete questions.
               |
               |Clarify what this task should do (scope, acceptance criteria), or reply here to have it re-evaluated.""".stripMargin
          )
        )
      // Evaluation/Execution are always taken from verifiedEvaluation, not
      // from whatever text happens to be embedded in cleanBody: for the
      // split-verification fallback above, cleanBody is the task's old
      // (pre-fallback) body, and trusting its stale "Execution: split"
      // would silently re-persist the wrong status.
      val priorMetadata = TaskMetadata.parse(claimedTask.task.body.value)
      val newMetadata = TaskMetadata
        .parse(cleanBody)
        .copy(
          evaluation = Some(if questions.isDefined then "needs-input" else "ready"),
          // Persist the blocked verdict too, so a replay can't read back
          // "Evaluation: needs-input, Execution: implement" and implement a
          // task that is still waiting on the user.
          execution = Some(
            if questions.isDefined then Execution.NeedsInput.wireValue
            else verifiedEvaluation.execution.wireValue
          ),
          answerConsumed = evaluated.userAnswer.map(answerDigest)
        )
      // Comments only need to carry prose when the evaluator actually
      // rewrote it. Re-posting an unchanged echo of the existing
      // description duplicates the issue body for no reason; read() still
      // recovers the description via the older-layer fallback either way
      // (TaskMetadata's Monoid, enrichedDescription = newer.orElse(older)).
      val commentMetadata = newMetadata.copy(
        enrichedDescription = newMetadata.enrichedDescription.filterNot(
          _.trim === priorMetadata.enrichedDescription.getOrElse("").trim
        )
      )
      val finalMetadata = Monoid[TaskMetadata].combine(priorMetadata, newMetadata)
      val updatedTask = claimedTask.task.copy(body = TaskMetadata.render(finalMetadata))
      val updatedRunner = claimedTask.context.agentInventory
        .selectRunner(GitHub.taskRunners(updatedTask))
        .getOrElse(claimedTask.runner)
      val updatedRun = claimedTask.copy(task = updatedTask, runner = updatedRunner)
      // Ignore `enrichedDescription` here: the evaluator LLM's prose is
      // non-deterministic between runs, so a raw text diff on the rendered
      // body would keep tripping on prose churn alone and re-post an
      // otherwise-unchanged "Task metadata:" comment every replay (only the
      // structured fields below are meaningful state to persist).
      val structuredMetadataChanged =
        finalMetadata.copy(enrichedDescription = None) !=
          priorMetadata.copy(enrichedDescription = None)
      for
        // Never rewrite the issue body: persist the evaluator's decision as a
        // new "Task metadata:" comment instead, folded back in on the next
        // read by TaskMetadataStore (see effectiveIssue).
        _ <-
          if structuredMetadataChanged then
            TaskMetadataStore
              .commentBased[F](progress)
              .write(
                claimedTask.context.root,
                claimedTask.task.number,
                commentMetadata
              )
          else Sync[F].unit
        result <- (questions, verifiedEvaluation.execution) match
          case (Some(pending), _) =>
            waitForUserInput((task.copy(claimedTask = updatedRun), pending))
          case (None, Execution.Split) =>
            Right(
              Left(
                SplitTask(
                  updatedRun,
                  // Repairing a cached split creates child issues in THIS
                  // run, so it still deserves the split-evaluation comment.
                  replayed = evaluated.replayedSplit && !repairedSplit
                )
              )
            ).pure[F]
          case (None, Execution.Implement) =>
            Right(Right(task.copy(claimedTask = updatedRun))).pure[F]
          case (None, Execution.NeedsInput) =>
            // Unreachable: `questions` is always defined for NeedsInput.
            Sync[F].raiseError(
              RuntimeException(
                s"Task #${claimedTask.task.number} needs user input but no questions were produced."
              )
            )
      yield result
    }

  def evaluateTaskPrompt(
      task: Issue,
      dependencyConclusion: Option[String],
      agentInventory: AgentInventory,
      userAnswer: Option[String]
  ): AgentPrompt =
    val dependencyConclusionStr = dependencyConclusion
      .map(comment => s"\nDependency Task Conclusion Comment:\n$comment\n")
      .getOrElse("")
    val userAnswerStr = userAnswer
      .map(answer =>
        s"\nUser's answer to previous clarifying questions:\n$answer\n\nIncorporate this answer. Only set status to \"questions\" again if the answer still leaves essential information missing.\n"
      )
      .getOrElse("")
    val descriptionState =
      if task.body.value.trim.isEmpty then "missing"
      else if strongDescription(task.body) then "strong"
      else "weak"

    val scalaMandate = if Impl.taskTouchesScala(task) then s"\n\n${Impl.ScalaSemanticMandate}" else ""

    AgentPrompt(s"""Evaluate and prepare this GitHub task before implementation.

Return only JSON, with this shape:
{
  "status": "ready" | "split" | "questions",
  "body": "updated GitHub issue body",
  "questions": "questions for the user, only when status is questions"
}

Rules:
- If the task is simple enough for implementation, write this metadata into the body:
  Task metadata:
  Evaluation: ready
  Execution: implement
- If the task should be split, create subtasks and write this metadata into the parent body:
  Task metadata:
  Evaluation: ready
  Execution: split
- If questions are needed, write this metadata into the body:
  Task metadata:
  Evaluation: needs-input
  Execution: needs-input
- Use Claude/Opus-level judgment to evaluate task clarity, scope, and split-readiness.
- If the description is missing or weak, create a clear, structured, detailed GitHub issue body.
- Keep the task narrow and implementation-oriented.
- Preserve existing parent/dependency references.
- Preserve existing preferred runner metadata, or existing required-abilities metadata, unchanged.
- Do NOT pin a concrete runner (agent/model/effort/version). Instead, express what the task needs as abstract abilities with importance coefficients (0..1) - the concrete runner is chosen at RUN time (not now), from live cost/availability/budget data, by matching these abilities against each tool's strengths/jobTypes. This lets the operator retune runner selection later without re-evaluating the task.
  - Write this metadata key exactly:
    Required abilities/importance:
    - complex-reasoning: 1.0
    - scala: 0.6
  - Name abilities that match the vocabulary already used in tool strengths/jobTypes above (e.g. complex-reasoning, source-of-truth, refactoring, focused-fixes, scala, planning, debugging, docs) so they actually match a tool's advertised strengths/jobTypes at selection time.
  - Coefficient = importance, not confidence: 1.0 for an ability the task cannot succeed without, lower for nice-to-have.
  - EXCEPTION - pinning a concrete runner is still allowed, and takes priority over required abilities, ONLY when the task genuinely needs one specific tool/version (e.g. reproducing a bug tied to one exact model). In that rare case, keep using "preferred llms/models/efforts/versions" as a ranked list of exact agent/model/effort/version entries (leave effort or version empty when unused, e.g. claude/sonnet//1.0.0).
  - Match the abilities to the job type, scope, and risk: small mechanical tasks need few/low-importance abilities, while broad Scala refactors, failing CI repair, and ambiguous debugging need high-importance capability abilities.
- Include Context, Goal, Scope, Acceptance Criteria, and Notes/Risks when useful.
- Evaluate whether the task should be split before implementation. Record the evaluation in the body.
- If splitting is needed, create GitHub subtasks before returning. Each subtask must have parent, dependencies if needed, acceptance criteria, narrow scope, and a "Required abilities/importance:" block.
- Prefer a target scope that a weaker model such as Haiku could implement without another split.
- If the task's existing preferred runner metadata pins claude/opus (or any top-tier model) for the whole task, treat that as a signal to split: opus-level judgment is only needed for plan/source-of-truth-shaped work, so carve those parts out and route implement/test leaves to cheaper runners instead of leaving the whole task on opus.
- If important information is missing and cannot be inferred, set status to "questions" and put concrete user questions in "questions".
- If questions are needed, still provide the best improved body possible in "body".
- Do not implement code changes.

Phase-typed decomposition:
- After deciding a task should split, also decide which PHASES it needs from {plan, source-of-truth, implement, test}. Only emit a phase when it adds value. Phase decomposition COMPOSES WITH scope split: a scope-piece may itself carry a "Phase:" tag, and a phase subtask may be scoped narrower.
- Bias toward FEWER phases. A trivial, fully-specified task -> a single "implement" subtask (or no split at all, status "ready"). Never force 4-phase overhead; over-splitting tiny tasks wastes evaluation tokens and adds issue noise.
- When you do phase-split, emit one subtask per chosen phase, dependency-ordered plan -> source-of-truth -> implement -> test. Give each subtask a "Phase:" tag inside its Task metadata block, e.g.:
  Task metadata:
  Evaluation: ready
  Execution: implement
  Phase: implement
- For "source-of-truth": pick, per concrete task, what the authoritative reference is (a spec / definition-of-done, a pinned existing artifact + versions, or the test oracle) and state it EXPLICITLY in that subtask's acceptance criteria so later phases conform to it.
- Actively narrow "implement" and "test" leaves toward the cheapest tier: split further while a leaf is too big or ambiguous for a weak model such as Haiku. Do NOT try to predict the tier a leaf needs and defend it with high coefficients: runner choice is MEASURED at run time from recorded success rates per (phase, runner), and a leaf that a cheap runner fails is re-run on a stronger one from a clean worktree. A wasted cheap attempt is therefore a bounded cost, not a failure - under-powering a leaf is NOT the primary failure mode. List only the abilities the leaf genuinely exercises, at honest importance; padding the list or inflating coefficients "to be safe" removes cheap runners from consideration and is the real cost here.
- The failure mode that IS unrecoverable is an ambiguous or under-specified leaf, because escalation replays the same ambiguity on a pricier runner. Spend your effort on acceptance criteria sharp enough to be judged mechanically, not on capability coefficients.
- A leaf that changes existing test files must carry "Phase: test"; implement-phase runs are refused if they modify or delete an existing test (adding one is fine). If a scope-piece needs both, split it.
- Token savings depend on plan/source-of-truth producing acceptance criteria tight enough for the cheaper implement/test model to succeed. Cross-phase context handoff rides through the issue bodies + dependency conclusion, so plan/source-of-truth subtasks must write their conclusions back into the issue body explicitly.

Phase -> required-abilities guidance. These are a STARTING PRIOR for a phase with no measurements yet, not a floor to defend (write them as "Required abilities/importance:" coefficients per subtask; the concrete runner across ALL vendors is picked at run time from live cost/fit data plus recorded success rates, never hardcode a vendor here):
- plan            -> strong reasoning / decomposition        -> high importance (e.g. complex-reasoning: 1.0)
- source-of-truth -> judgment on authority / spec            -> high-medium importance (e.g. source-of-truth: 0.8)
- implement       -> narrow, well-specified code change      -> medium importance (task-dependent); go low freely once the leaf is narrow and fully specified - a failed cheap attempt escalates
- test            -> narrow verification                     -> low-medium importance; keep the ability list short so a cheap runner is picked
Name abilities that match the vocabulary already used in tool strengths/jobTypes above so run-time matching actually fires.
$scalaMandate

Available local implementor tools:
${agentInventory.promptBlock}

Task ID: #${task.number}
Title: ${task.title}
Description state: $descriptionState

Current Task Description:
${task.body}
$dependencyConclusionStr$userAnswerStr""")

  def splitTaskPrompt(
      task: Issue,
      dependencyConclusion: Option[String],
      agentInventory: AgentInventory
  ): AgentPrompt =
    val dependencyConclusionStr = dependencyConclusion
      .map(comment => s"\nDependency Task Conclusion Comment:\n$comment\n")
      .getOrElse("")

    val scalaMandate = if Impl.taskTouchesScala(task) then s"\n\n${Impl.ScalaSemanticMandate}" else ""

    AgentPrompt(
      s"""Create missing GitHub subtasks for an already-evaluated split task.
$scalaMandate

Available local implementor tools:
${agentInventory.promptBlock}

Rules:
- Do not evaluate this task again.
- Do not decide whether it should be implemented directly.
- The task is already classified as:
  Task metadata:
  Evaluation: ready
  Execution: split
- Create the missing child issues now.
- Each child issue body must include this exact parent link line:
  Parent: #${task.number}
- Each child issue must include dependencies if needed, acceptance criteria, narrow scope, and a "Required abilities/importance:" block (ability -> importance coefficient 0..1). Only pin a concrete "preferred llms/models/efforts/versions" runner when the child genuinely needs one specific tool/version.
- Preserve the phase-typed split design when the evaluated task body describes one.
- Prefer a target scope that a weaker model such as Haiku could implement without another split.
- Use only GitHub issue creation/comment commands for the split side effects.
- Do not implement code changes.
- Return a short plain-text summary listing the child issue numbers you created.

Task ID: #${task.number}
Title: ${task.title}

Already-evaluated task body:
${task.body}
$dependencyConclusionStr
"""
    )

  def parseTaskEvaluation(
      output: AgentOutput,
      fallbackBody: IssueBody
  ): TaskEvaluation =
    val stripped =
      extractJsonObject(output.value)
        .getOrElse(stripMarkdownFence(output.value))
        .trim
    Try {
      val json = ujson.read(stripped)
      TaskEvaluation(
        body = IssueBody(
          json.obj
            .get("body")
            .collect { case ujson.Str(value) => value }
            .filter(_.trim.nonEmpty)
            .getOrElse(fallbackBody.value)
        ),
        questions = json.obj
          .get("questions")
          .collect { case ujson.Str(value) => value }
          .filter(_.trim.nonEmpty),
        execution = json.obj
          .get("status")
          .collect { case ujson.Str(value) => Execution.fromString(value) }
          .getOrElse(Execution.NeedsInput)
      )
    }.getOrElse {
      // Evaluator output wasn't valid JSON: don't corrupt the issue body with
      // raw output, and don't silently fall through to "ready" either. Post
      // an explicit, real reason so this is a genuine, explainable block.
      val preview = stripped.take(500)
      TaskEvaluation(
        body = fallbackBody,
        questions = Some(
          s"""Evaluator output could not be parsed as JSON, so task evaluation failed.
             |
             |Raw output (truncated):
             |$preview
             |
             |Reply here once this can be re-run, or fix the underlying evaluator issue.""".stripMargin
        ),
        execution = Execution.NeedsInput
      )
    }

  private def extractJsonObject(value: String): Option[String] =
    val jsonFence = "(?s)```json\\s*(\\{.*?\\})\\s*```".r
    val anyFence = "(?s)```\\s*(\\{.*?\\})\\s*```".r
    jsonFence
      .findFirstMatchIn(value)
      .orElse(anyFence.findFirstMatchIn(value))
      .map(_.group(1))
      .orElse {
        val start = value.indexOf('{')
        val end = value.lastIndexOf('}')
        Option.when(start >= 0 && end > start)(value.substring(start, end + 1))
      }

  /** Stable short digest of a user answer, persisted as the "Answer-consumed:" metadata mark. Whitespace-normalized so
    * trivial re-rendering of the same comment text doesn't look like a new answer.
    */
  def answerDigest(answer: UserInput): String =
    // Inside the opaque type's scope UserInput is String; ascribe rather than
    // call the `value` extension, which is shadowed by java.lang.String here.
    val normalized = (answer: String).trim.replaceAll("\\s+", " ")
    java.security.MessageDigest
      .getInstance("SHA-256")
      .digest(normalized.getBytes("UTF-8"))
      .take(8)
      .map(byte => f"${byte & 0xff}%02x")
      .mkString

  /** True when there is no answer, or the answer was already folded into a persisted evaluation — i.e. when the cached
    * evaluation may be reused.
    */
  def answerAlreadyApplied(task: Issue, answer: Option[UserInput]): Boolean =
    answer.forall(value => metadataValue(task.body, "answer-consumed").contains(answerDigest(value)))

  def completedEvaluation(
      task: Issue,
      hasRealQuestion: Boolean
  ): Option[TaskEvaluation] =
    (evaluationStatus(task.body), executionStatus(task.body)) match
      // Only actionable verdicts replay as "ready". A persisted
      // (ready, needs-input) pair is contradictory legacy state: fall through
      // to a real evaluation rather than implementing a blocked task.
      case (Some("ready"), Some(execution @ (Execution.Implement | Execution.Split))) =>
        TaskEvaluation(task.body, None, execution = execution).some
      case (Some("needs-input"), _) | (_, Some(Execution.NeedsInput)) if hasRealQuestion =>
        TaskEvaluation(
          task.body,
          Some(
            "Task is already marked as needing user input. See the \"Questions before execution:\" comment on this issue for details."
          ),
          execution = Execution.NeedsInput
        ).some
      // needs-input metadata but no question was ever actually posted:
      // not a genuine block, fall through to a real evaluation.
      case _ => None

  private def evaluationStatus(body: IssueBody): Option[String] =
    metadataValue(body, "evaluation")

  private def executionStatus(body: IssueBody): Option[Execution] =
    metadataValue(body, "execution").map(Execution.fromString)

  private def metadataValue(body: IssueBody, key: String): Option[String] =
    val prefix = s"$key:"
    body.value.linesIterator
      .map(_.trim.toLowerCase)
      .collectFirst {
        case line if line.startsWith(prefix) =>
          line.stripPrefix(prefix).trim
      }

  private def strongDescription(body: IssueBody): Boolean =
    val lower = body.value.toLowerCase
    val hasStructure =
      List("context", "goal", "scope", "acceptance").count(lower.contains) >= 2
    val hasEnoughDetail = body.value.trim.length >= 500
    hasStructure && hasEnoughDetail && hasRunnerMetadata(body)

  private def hasRunnerMetadata(body: IssueBody): Boolean =
    val lower = body.value.toLowerCase
    lower.contains("preferred llms/models/versions") ||
    lower.contains("preferred llms/models/efforts/versions") ||
    lower.contains("agent/model/version") ||
    lower.contains("agent/model/effort/version")

  private def stripMarkdownFence(value: String): String =
    val trimmed = value.trim
    if trimmed.startsWith("```") && trimmed.endsWith("```") then
      trimmed.linesIterator.toList.drop(1).dropRight(1).mkString("\n")
    else trimmed
