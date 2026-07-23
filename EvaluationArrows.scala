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
    verifySplitExists: EvaluationArrows.EvaluatedTask --> TaskEvaluation,
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
                .filter(_ => userAnswer.isEmpty)
              result <- replayedEvaluation.traverse { cachedEvaluation =>
                progress(
                  s"Reusing existing evaluation for #${claimedTask.task.number}."
                ).as((task, cachedEvaluation, cachedEvaluation.execution.value === "split"))
              }
            yield result
          )
      }
    )

final case class AgenticEvaluationArrows[EvalArrow[_, _], VerifyArrow[_, _]](
    evaluate: EvalArrow[EvaluationArrows.EvaluationInput, EvaluationArrows.EvaluatedTask],
    verifySplitExists: VerifyArrow[EvaluationArrows.EvaluatedTask, TaskEvaluation]
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
        OptionT.liftF(runEvaluator(task, userAnswer.map(_.value)).map(evaluation => (task, evaluation, false)))
      }

    val verifySplitExists: Kleisli[[X] =>> OptionT[F, X], EvaluationArrows.EvaluatedTask, TaskEvaluation] =
      Kleisli { case (task, evaluation, _) =>
        OptionT.liftF {
          val claimedTask = task.claimedTask
          if evaluation.execution.value === "split" then
            GitHub.fetchIssues(claimedTask.context.root).flatMap { allIssues =>
              if GitHub.hasOpenChildren(claimedTask.task, allIssues) then evaluation.pure[F]
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
                    if GitHub.hasOpenChildren(claimedTask.task, updatedIssues) then evaluation.pure[F]
                    else
                      Sync[F].raiseError(
                        RuntimeException(
                          s"Task #${claimedTask.task.number} is already evaluated as split, but missing split-subtask creation produced no linked child issues."
                        )
                      )
                  }
            }
          else evaluation.pure[F]
        }
      }

    AgenticEvaluationArrows(
      evaluate = evaluate,
      verifySplitExists = Kleisli[F, EvaluationArrows.EvaluatedTask, TaskEvaluation] { evaluatedTask =>
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
    extension (self: UserInput) def value: String = self

  type EvaluationInput = (PreparedTask, Option[UserInput])
  type EvaluatedTask = (PreparedTask, TaskEvaluation, Boolean)
  type VerifiedEvaluation = (EvaluatedTask, TaskEvaluation)
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

    val cachedEvaluation = CachedEvaluationArrows[F](progress)
    val agenticEvaluation = AgenticEvaluationArrows[F](
      progress = progress,
      evaluatorRunner = evaluatorRunner
    )
    val evaluateOrGetCached: Kleisli[F, EvaluationInput, EvaluatedTask] =
      val evaluate =
        cachedEvaluation.evaluate <+> agenticEvaluation.evaluate
      Kleisli { input =>
        evaluate
          .run(input)
          .getOrElseF(
            Sync[F].raiseError(
              RuntimeException("Cached and agentic task evaluation both missed.")
            )
          )
      }

    val persistAndRouteEvaluation: Kleisli[F, VerifiedEvaluation, Result] =
      Kleisli { case ((task, _, replayedSplit), verifiedEvaluation) =>
        val claimedTask = task.claimedTask
        val cleanBody = stripMarkdownFence(verifiedEvaluation.body.value).trim
        // Evaluation/Execution are always taken from verifiedEvaluation, not
        // from whatever text happens to be embedded in cleanBody: for the
        // split-verification fallback above, cleanBody is the task's old
        // (pre-fallback) body, and trusting its stale "Execution: split"
        // would silently re-persist the wrong status.
        val priorMetadata = TaskMetadata.parse(claimedTask.task.body.value)
        val newMetadata = TaskMetadata
          .parse(cleanBody)
          .copy(
            evaluation = Some(
              if verifiedEvaluation.questions.exists(_.trim.nonEmpty) then "needs-input"
              else "ready"
            ),
            execution = Some(normalizeExecution(verifiedEvaluation.execution.value))
          )
        val finalMetadata = Monoid[TaskMetadata].combine(priorMetadata, newMetadata)
        val updatedTask = claimedTask.task.copy(body = TaskMetadata.render(finalMetadata))
        val updatedRunner = claimedTask.context.agentInventory
          .selectRunner(GitHub.taskRunners(updatedTask))
          .getOrElse(claimedTask.runner)
        val updatedRun = claimedTask.copy(task = updatedTask, runner = updatedRunner)
        for
          // Never rewrite the issue body: persist the evaluator's decision as a
          // new "Task metadata:" comment instead, folded back in on the next
          // read by TaskMetadataStore (see effectiveIssue).
          _ <-
            if updatedTask.body.value.trim =!= claimedTask.task.body.value.trim then
              TaskMetadataStore
                .commentBased[F](progress)
                .write(
                  claimedTask.context.root,
                  claimedTask.task.number,
                  newMetadata
                )
            else Sync[F].unit
          result <- {
            verifiedEvaluation.questions.filter(_.trim.nonEmpty) match
              case Some(questions) =>
                waitForUserInput((task.copy(claimedTask = updatedRun), questions.trim))
              case None if verifiedEvaluation.execution.value === "split" =>
                Right(
                  Left(
                    SplitTask(
                      updatedRun,
                      replayed = replayedSplit
                    )
                  )
                ).pure[F]
              case None =>
                Right(Right(task.copy(claimedTask = updatedRun))).pure[F]
          }
        yield result
      }

    EvaluationArrows(
      fetchMaybeUserAnswer = fetchMaybeUserAnswer,
      evaluateOrGetCached = evaluateOrGetCached,
      verifySplitExists = agenticEvaluation.verifySplitExists,
      persistAndRouteEvaluation = persistAndRouteEvaluation
    )

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

    AgentPrompt(s"""Evaluate and prepare this GitHub task before implementation.

Task ID: #${task.number}
Title: ${task.title}
Description state: $descriptionState

Available local implementor tools:
${agentInventory.promptBlock}

Current Task Description:
${task.body}
$dependencyConclusionStr$userAnswerStr
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
- Preserve existing preferred runner metadata, or add it if missing.
- Prefer the new metadata key "preferred llms/models/efforts/versions" when adding or replacing runner metadata.
- Choose implementor runners only from the available local implementor tools above.
  - Match the chosen runner to the job type, scope, and risk: small mechanical tasks can use cheaper/weaker tools, while broad Scala refactors, failing CI repair, and ambiguous debugging should use stronger tools.
  - Prefer the cheapest listed runner whose strengths satisfy the task's phase capability floor; only use a pricier runner when no cheaper runner is capable, and use priority only to break near-ties in cost.
  - Write preferred runners as a ranked list. Each item must be exactly agent/model/effort/version; leave effort or version empty when the tool has no value, for example claude/sonnet//1.0.0.
- Include Context, Goal, Scope, Acceptance Criteria, and Notes/Risks when useful.
- Evaluate whether the task should be split before implementation. Record the evaluation in the body.
- If splitting is needed, create GitHub subtasks before returning. Each subtask must have parent, dependencies if needed, acceptance criteria, narrow scope, and preferred llms/models/efforts/versions.
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
- Actively narrow "implement" and "test" leaves toward the cheapest tier: split further while a leaf is too big or ambiguous for a weak model such as Haiku. When a leaf resists narrowing (irreducible complexity, cross-cutting reasoning, high risk), STOP and route it to the cheapest runner that actually fits. Set a real capability floor per leaf; never down-tier below that floor for price alone. Under-powering a leaf is the primary failure mode: a too-weak model produces wrong code, forcing repair/replay and a net token LOSS.
- Token savings depend on plan/source-of-truth producing acceptance criteria tight enough for the cheaper implement/test model to succeed. Cross-phase context handoff rides through the issue bodies + dependency conclusion, so plan/source-of-truth subtasks must write their conclusions back into the issue body explicitly.

Phase -> capability-tier routing (select concrete runners from the available local implementor tools above by cost vs. fit across ALL vendors; never hardcode a vendor):
- plan            -> strong reasoning / decomposition        -> high tier
- source-of-truth -> judgment on authority / spec            -> high-medium tier
- implement       -> narrow, well-specified code change      -> medium (task-dependent); cheap ONLY when genuinely trivial + fully specified
- test            -> narrow verification                     -> low-medium; cheapest capable
Match each phase's ranked "preferred llms/models/efforts/versions" to this table: primary = cheapest runner that still fits the phase's required capability, fallbacks = progressively stronger runners for escalation. Prefer runners whose jobTypes/strengths advertise the phase name.
""")

  def splitTaskPrompt(
      task: Issue,
      dependencyConclusion: Option[String],
      agentInventory: AgentInventory
  ): AgentPrompt =
    val dependencyConclusionStr = dependencyConclusion
      .map(comment => s"\nDependency Task Conclusion Comment:\n$comment\n")
      .getOrElse("")

    AgentPrompt(
      s"""Create missing GitHub subtasks for an already-evaluated split task.

Task ID: #${task.number}
Title: ${task.title}

Available local implementor tools:
${agentInventory.promptBlock}

Already-evaluated task body:
${task.body}
$dependencyConclusionStr
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
- Each child issue must include dependencies if needed, acceptance criteria, narrow scope, and preferred llms/models/efforts/versions.
- Preserve the phase-typed split design when the evaluated task body describes one.
- Prefer a target scope that a weaker model such as Haiku could implement without another split.
- Use only GitHub issue creation/comment commands for the split side effects.
- Do not implement code changes.
- Return a short plain-text summary listing the child issue numbers you created.
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
        execution = Execution(
          json.obj
            .get("status")
            .collect { case ujson.Str(value) => normalizeExecution(value) }
            .getOrElse("needs-input")
        )
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
        execution = Execution("needs-input")
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

  def completedEvaluation(
      task: Issue,
      hasRealQuestion: Boolean
  ): Option[TaskEvaluation] =
    (evaluationStatus(task.body), executionStatus(task.body)) match
      case (Some("ready"), Some(status)) =>
        TaskEvaluation(task.body, None, execution = Execution(status)).some
      case (Some("needs-input"), _) | (_, Some("needs-input")) if hasRealQuestion =>
        TaskEvaluation(
          task.body,
          Some(
            "Task is already marked as needing user input. See the \"Questions before execution:\" comment on this issue for details."
          ),
          execution = Execution("needs-input")
        ).some
      // needs-input metadata but no question was ever actually posted:
      // not a genuine block, fall through to a real evaluation.
      case _ => None

  private def evaluationStatus(body: IssueBody): Option[String] =
    metadataValue(body, "evaluation")

  private def executionStatus(body: IssueBody): Option[String] =
    metadataValue(body, "execution").map(normalizeExecution)

  private def metadataValue(body: IssueBody, key: String): Option[String] =
    val prefix = s"$key:"
    body.value.linesIterator
      .map(_.trim.toLowerCase)
      .collectFirst {
        case line if line.startsWith(prefix) =>
          line.stripPrefix(prefix).trim
      }

  private def normalizeExecution(value: String): String =
    value.trim.toLowerCase match
      case "ready" | "implement" => "implement"
      case "split"               => "split"
      case _                     => "needs-input"

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
