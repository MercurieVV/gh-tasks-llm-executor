import arrowstep.core.*
import arrowstep.runtime.AgentMain
import cats.Monoid
import cats.arrow.ArrowChoice
import cats.data.Kleisli
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.Resource
import cats.effect.kernel.Sync
import cats.syntax.all.*

import scala.annotation.tailrec
import scala.util.Try

final case class AppInput(root: os.Path, taskNumber: Option[Int])

final case class RunContext(
    root: os.Path,
    agentInventory: AgentInventory,
    taskNumber: Option[Int]
)

final case class TaskRunner(
    agent: String,
    model: Option[String],
    effort: Option[String],
    version: Option[String]
):
  def display: String =
    val modelPart = model.fold("")(value => s", model: $value")
    val effortPart = effort.fold("")(value => s", effort: $value")
    val versionPart = version.fold("")(value => s", version: $value")
    s"agent: $agent$modelPart$effortPart$versionPart"

  def command(
      prompt: String,
      allowedTools: Seq[String] = Nil,
      jsonSchema: Option[String] = None
  ): Seq[String] =
    agent match
      case "claude" =>
        Seq(agent) ++ model.toList.flatMap(value => Seq("--model", value)) ++
          (if allowedTools.isEmpty then Nil
           else Seq("--allowedTools") ++ allowedTools) ++
          jsonSchema.toList.flatMap(schema => Seq("--json-schema", schema)) ++
          Seq("-p", prompt)
      case "codex" =>
        Seq(agent, "exec") ++
          model.toList.flatMap(value => Seq("--model", value)) ++
          effort.toList.flatMap(value =>
            Seq("--config", s"model_reasoning_effort=$value")
          ) ++
          Seq(prompt)
      case "aider" =>
        Seq(agent) ++ model.toList.flatMap(value => Seq("--model", value)) ++
          Seq("--yes-always", "--no-auto-commits", "--message", prompt)
      case _ =>
        Seq(agent) ++ model.toList.flatMap(value => Seq("-m", value)) ++
          Seq("-p", prompt)

final case class RunnableTask(issue: Issue, runner: TaskRunner)

final case class TaskSelection(
    context: RunContext,
    candidates: List[RunnableTask]
)

final case class NoTask(context: RunContext)

final case class TaskRun(
    context: RunContext,
    task: Issue,
    runner: TaskRunner,
    worktreePath: os.Path,
    branchName: String
)

final case class TaskWithPrompt(
    run: TaskRun,
    parentConclusion: Option[String],
    replayContext: Option[String]
)

final case class TaskWithOutput(run: TaskRun, output: String)

final case class NeedsUserInput(run: TaskRun, questions: String)

final case class SplitTask(run: TaskRun)

final case class TaskEvaluation(
    body: String,
    questions: Option[String],
    execution: String
)

final case class ExistingBranch(run: TaskWithPrompt)

final case class NewBranch(run: TaskWithPrompt)

final case class ChangedTask(run: TaskWithOutput)

final case class UnchangedTask(run: TaskWithOutput)

final case class AgentFinalization(
    commitTitle: Option[String],
    pullRequestBody: Option[String]
)

final case class RunSummary(
    status: String,
    message: String,
    task: Option[Issue]
):
  def toJson: ujson.Value =
    ujson.Obj.from(
      Seq(
        "status" -> ujson.Str(status),
        "message" -> ujson.Str(message),
        "task" -> task.fold[ujson.Value](ujson.Null) { issue =>
          ujson.Obj.from(
            Seq(
              "number" -> ujson.Num(issue.number),
              "title" -> ujson.Str(issue.title),
              "state" -> ujson.Str(issue.state)
            )
          )
        }
      )
    )

object Main extends IOApp:

  type -->[F[_], A, B] = Kleisli[F, A, B]
  type Flow[F[_]] = [A, B] =>> Kleisli[F, A, B]
  private val evaluatorRunner: TaskRunner =
    TaskRunner("claude", Some("opus"), None, None)
  def run(args: List[String]): IO[ExitCode] =
    val taskNumber = parseTaskNumber(args)
    val arrowstepArgs = removeScriptArgs(args)
    AgentMain
      .run[IO](arrowstepArgs, os.pwd)(_ =>
        program(AppInput(os.pwd, taskNumber))
      )
      .flatMap { outcome =>
        IO.print(outcome.stdout) *>
          IO.pure(ExitCode(outcome.exitCode))
      }

  private def program[F[_]: Sync](
      input: AppInput
  ): F[ProgramSays[ujson.Value]] =
    taskFlow[F].run(input).map(summary => ProgramSays.Done(summary.toJson))

  private def taskFlow[F[_]: Sync]: -->[F, AppInput, RunSummary] =
    traced("resolveContext", resolveContext[F]) >>>
      traced("selectTask", selectTask[F]) >>>
      traced("executeTask", executeTask[F])

  private def resolveContext[F[_]: Sync]: -->[F, AppInput, RunContext] =
    Kleisli { input =>
      AgentInventory
        .load[F](input.root)
        .map(RunContext(input.root, _, input.taskNumber))
    }

  private def selectTask[F[_]: Sync]: -->[F, RunContext, TaskSelection] =
    Kleisli { (context: RunContext) =>
      for
        _ <- progress("Fetching open issues from GitHub...")
        rawIssues <- GitHub.fetchIssues(context.root)
        issues <- rawIssues.traverse(effectiveIssue[F](context.root, _))
        openIssueNumbers = issues.map(_.number).toSet
        filteredByDeps = issues
          .filter(task => context.taskNumber.forall(_ === task.number))
          .filterNot(GitHub.hasUnresolvedDependencies(_, openIssueNumbers))
          .filterNot(GitHub.hasOpenChildren(_, issues))
        eligible <- filteredByDeps.filterA { task =>
          val needsInputMetadata =
            evaluationStatus(task.body).contains("needs-input") ||
              executionStatus(task.body).contains("needs-input")
          val needsInputCheck =
            if !needsInputMetadata then true.pure[F]
            else
              GitHub.hasQuestionComment(context.root, task).flatMap {
                case false =>
                  // needs-input metadata with no real question ever posted:
                  // not a genuine block, let evaluation resolve it.
                  true.pure[F]
                case true =>
                  GitHub
                    .userAnswer(context.root, task, progress)
                    .map(_.nonEmpty)
              }
          needsInputCheck.flatMap {
            case false => false.pure[F]
            case true =>
              // A branch/PR from an earlier run may still be open (e.g. its
              // CI is still failing or awaiting review): re-running the
              // implementer would create a second local branch of the same
              // name and diverge, so skip the task until that PR closes.
              GitHub
                .hasOpenPullRequestForBranch(
                  context.root,
                  s"task-${task.number}"
                )
                .map(!_)
          }
        }
        candidates = eligible.map(task =>
          RunnableTask(
            task,
            context.agentInventory
              .selectRunner(GitHub.taskRunners(task))
              .getOrElse(evaluatorRunner)
          )
        )
        _ <- progress(
          context.taskNumber.fold(
            s"Found ${candidates.size} runnable open tasks with preferred runner metadata."
          )(number =>
            s"Found ${candidates.size} runnable open tasks matching #$number."
          )
        )
      yield TaskSelection(context, candidates)
    }

  // Merges an issue's original body with every TaskMetadata comment posted
  // for it into one synthesized view, so the existing body-based parsers
  // (evaluationStatus, taskRunners, ...) keep working unchanged while the
  // real issue body is never rewritten.
  private def effectiveIssue[F[_]: Sync](root: os.Path, issue: Issue): F[Issue] =
    TaskMetadataStore.commentBased[F]
      .read(root, issue)
      .map(merged => issue.copy(body = TaskMetadata.render(merged)))

  private def executeTask[F[_]: Sync]: -->[F, TaskSelection, RunSummary] =
    Kleisli { selection =>
      claimAndRunTask[F](selection.context, selection.candidates)
    }

  // Try candidates in order, claiming each via IssueClaim before running it.
  // A claim conflict means another process already owns that issue, so we
  // move on to the next candidate instead of failing the whole run.
  private def claimAndRunTask[F[_]: Sync](
      context: RunContext,
      candidates: List[RunnableTask]
  ): F[RunSummary] =
    candidates match
      case Nil =>
        noTaskSummary[F].run(NoTask(context))
      case head :: tail =>
        IssueClaim
          .acquire[F](context.root, head.issue.number, progress)
          .use { _ =>
            taskExecution[F].run(
              taskRun(context, head.issue, head.runner)
            )
          }
          .recoverWith { case _: IssueAlreadyClaimedException =>
            progress(
              s"Task #${head.issue.number} is already claimed by another process. Trying next candidate..."
            ) *> claimAndRunTask[F](context, tail)
          }

  private def noTaskSummary[F[_]: Sync]: -->[F, NoTask, RunSummary] =
    Kleisli { noTask =>
      val context = noTask.context
      val message = context.taskNumber.fold(
        "No tasks found without unresolved dependencies, open child tasks, or another process already claiming them."
      )(number =>
        s"No runnable open task found for #$number. It may be closed, blocked by dependencies or open child tasks, marked needs-input, or already claimed by another process."
      )
      RunSummary(
        status = "no-task",
        message = message,
        task = None
      ).pure[F]
    }

  private def taskExecution[F[_]: Sync]: -->[F, TaskRun, RunSummary] =
    traced("announceTask", announceTask[F]) >>>
      traced("fetchTaskContext", fetchTaskContext[F]) >>>
      traced("evaluateTask", evaluateTask[F]) >>>
      choose[F, NeedsUserInput, Either[SplitTask, TaskWithPrompt], RunSummary](
        traced("needsUserInputSummary", needsUserInputSummary[F]),
        choose[F, SplitTask, TaskWithPrompt, RunSummary](
          traced("splitTaskSummary", splitTaskSummary[F]),
          traced("executePreparedTask", executePreparedTask[F])
        )
      )

  private def executePreparedTask[
      F[_]: Sync
  ]: -->[F, TaskWithPrompt, RunSummary] =
    runPreparedTask[F].map { run =>
      RunSummary(
        status = "completed",
        message = s"Task #${run.task.number} completed successfully.",
        task = Some(run.task)
      )
    }

  private def runPreparedTask[
      F[_]: Sync
  ]: -->[F, TaskWithPrompt, TaskRun] =
    traced("markInProgress", markInProgress[F]) >>>
      Kleisli { task =>
        worktreeResource[F](task).use { acquiredTask =>
          (traced("runExecutor", runExecutor[F]) >>>
            traced("runProjectValidation", runProjectValidation[F]) >>>
            traced("commentOutput", commentOutput[F]) >>>
            traced("commitIfChanged", commitIfChanged[F]) >>>
            traced("verifyReplayCi", verifyReplayCi[F]) >>>
            traced("closeTask", closeTask[F])).run(acquiredTask).onError { case _ =>
            Git[F].preserveUnpushedCommits(
              acquiredTask.run.worktreePath,
              acquiredTask.run.branchName,
              progress
            )
          }
        }
      }

  private def needsUserInputSummary[
      F[_]: Sync
  ]: -->[F, NeedsUserInput, RunSummary] =
    Kleisli { input =>
      GitHub
        .commentNeedsUserInput(
          input.run.context.root,
          input.run.task,
          input.questions,
          progress
        )
        .as(
          RunSummary(
            status = "needs-input",
            message =
              s"Task #${input.run.task.number} needs user input before execution.",
            task = Some(input.run.task)
          )
        )
    }

  private def splitTaskSummary[F[_]: Sync]: -->[F, SplitTask, RunSummary] =
    Kleisli { split =>
      GitHub
        .commentSplitEvaluation(
          split.run.context.root,
          split.run.task,
          progress
        )
        .as(
          RunSummary(
            status = "split",
            message =
              s"Task #${split.run.task.number} was evaluated for splitting and will not be implemented directly.",
            task = Some(split.run.task)
          )
        )
    }

  private def announceTask[F[_]: Sync]: -->[F, TaskRun, TaskRun] =
    Kleisli { run =>
      progress(
        s"Selected next task: #${run.task.number} - ${run.task.title}"
      ).as(run)
    }

  private def markInProgress[
      F[_]: Sync
  ]: -->[F, TaskWithPrompt, TaskWithPrompt] =
    Kleisli { task =>
      val run = task.run
      GitHub
        .setIssueStatus(
          run.context.root,
          run.task.number,
          "in progress",
          progress
        )
        .as(task)
    }

  private def fetchTaskContext[
      F[_]: Sync
  ]: -->[F, TaskRun, TaskWithPrompt] =
    Kleisli { run =>
      for
        dependencyConclusion <- GitHub.dependencyConclusion(
          run.context.root,
          run.task,
          progress
        )
        replayContext <- GitHub.replayContext(
          run.context.root,
          run.task,
          progress
        )
      yield TaskWithPrompt(run, dependencyConclusion, replayContext)
    }

  private def evaluateTask[
      F[_]: Sync
  ]: -->[F, TaskWithPrompt, Either[
    NeedsUserInput,
    Either[SplitTask, TaskWithPrompt]
  ]] =
    Kleisli { task =>
      val run = task.run
      for
        _ <- progress(
          s"Evaluating task #${run.task.number} with ${evaluatorRunner.display}..."
        )
        userAnswer <- GitHub.userAnswer(run.context.root, run.task, progress)
        hasRealQuestion <- GitHub.hasQuestionComment(run.context.root, run.task)
        evaluation <- existingEvaluation(run.task, hasRealQuestion)
          .filter(_ => userAnswer.isEmpty)
          .fold(
            AgentExecutor[F]
              .run(
                evaluatorRunner,
                evaluateTaskPrompt(
                  run.task,
                  task.parentConclusion,
                  run.context.agentInventory,
                  userAnswer
                ),
                run.context.root,
                EvaluatorAllowedTools,
                Some(EvaluationJsonSchema)
              )
              .map(parseTaskEvaluation(_, run.task.body))
          )(_.pure[F])
        verifiedEvaluation <-
          if evaluation.execution === "split" then
            GitHub.fetchIssues(run.context.root).map { allIssues =>
              if GitHub.hasOpenChildren(run.task, allIssues) then evaluation
              else
                // Evaluator claimed "split" but created no child issues referencing
                // this task as parent. Do not accept its proposed body (it may be
                // garbage/placeholder text) and do not silently proceed; surface a
                // real, explicit failure instead so the task can be retried.
                TaskEvaluation(
                  body = run.task.body,
                  questions = Some(
                    "Evaluator marked this task as needing a split but did not create any child issues referencing it as parent. Task evaluation failed; re-run evaluation or split the task manually."
                  ),
                  execution = "needs-input"
                )
            }
          else evaluation.pure[F]
        cleanBody = stripMarkdownFence(verifiedEvaluation.body).trim
        // Evaluation/Execution are always taken from verifiedEvaluation, not
        // from whatever text happens to be embedded in cleanBody: for the
        // split-verification fallback above, cleanBody is the task's old
        // (pre-fallback) body, and trusting its stale "Execution: split"
        // would silently re-persist the wrong status.
        priorMetadata = TaskMetadata.parse(run.task.body)
        newMetadata = TaskMetadata.parse(cleanBody).copy(
          evaluation = Some(
            if verifiedEvaluation.questions.exists(_.trim.nonEmpty) then
              "needs-input"
            else "ready"
          ),
          execution = Some(normalizeExecution(verifiedEvaluation.execution))
        )
        finalMetadata = Monoid[TaskMetadata].combine(priorMetadata, newMetadata)
        updatedTask = run.task.copy(body = TaskMetadata.render(finalMetadata))
        updatedRunner = run.context.agentInventory
          .selectRunner(GitHub.taskRunners(updatedTask))
          .getOrElse(run.runner)
        updatedRun = run.copy(task = updatedTask, runner = updatedRunner)
        // Never rewrite the issue body: persist the evaluator's decision as a
        // new "Task metadata:" comment instead, folded back in on the next
        // read by TaskMetadataStore (see effectiveIssue).
        _ <-
          if updatedTask.body.trim =!= run.task.body.trim then
            TaskMetadataStore.commentBased[F].write(
              run.context.root,
              run.task.number,
              newMetadata,
              progress
            )
          else Sync[F].unit
      yield verifiedEvaluation.questions.filter(_.trim.nonEmpty) match
        case Some(questions) => Left(NeedsUserInput(updatedRun, questions.trim))
        case None if verifiedEvaluation.execution === "split" =>
          Right(Left(SplitTask(updatedRun)))
        case None =>
          Right(Right(task.copy(run = updatedRun)))
    }

  private def worktreeResource[F[_]: Sync](
      task: TaskWithPrompt
  ): Resource[F, TaskWithPrompt] =
    val run = task.run
    Resource.make {
      for
        _ <- TaskLogger.trace[F](
          s"enter acquireWorktree input=${summarize(task)}"
        )
        _ <- Git[F].acquireWorktree(
          run.context.root,
          run.worktreePath,
          run.branchName,
          progress
        )
        _ <- TaskLogger.trace[F](
          s"exit acquireWorktree output=${summarize(task)}"
        )
      yield task
    } { acquiredTask =>
      val acquiredRun = acquiredTask.run
      TaskLogger.trace[F](
        s"enter releaseWorktree input=${summarize(acquiredTask)}"
      ) *>
        Git[F]
          .releaseWorktree(
            acquiredRun.context.root,
            acquiredRun.worktreePath,
            acquiredRun.branchName,
            progress
          )
          .handleErrorWith(error =>
            TaskLogger.trace[F](
              s"fail releaseWorktree error=${error.getClass.getSimpleName}: ${error.getMessage}"
            )
          ) *>
        TaskLogger.trace[F](
          s"exit releaseWorktree output=${summarize(acquiredTask)}"
        )
    }

  private def runExecutor[F[_]: Sync]: -->[F, TaskWithPrompt, TaskWithOutput] =
    Kleisli { task =>
      runTaskWithRunner[F](task).handleErrorWith { error =>
        task.run.context.agentInventory
          .nextStrongerImplementor(task.run.runner) match
          case Some(fallbackRunner) =>
            val fallbackTask =
              task.copy(run = task.run.copy(runner = fallbackRunner))
            progress(
              s"Runner ${task.run.runner.display} failed after retries: ${error.getMessage}. Retrying task #${task.run.task.number} with stronger fallback ${fallbackRunner.display}..."
            ) *>
              runTaskWithRunner[F](fallbackTask)
          case None =>
            progress(
              s"Runner ${task.run.runner.display} failed after retries and no stronger fallback runner is available."
            ) *>
              Sync[F].raiseError(error)
      }
    }

  private def runTaskWithRunner[F[_]: Sync](
      task: TaskWithPrompt
  ): F[TaskWithOutput] =
    val run = task.run
    val prompt =
      taskPrompt(
        run.task,
        run.runner,
        task.parentConclusion,
        task.replayContext
      )
    for
      _ <- progress(
        s"Running task #${run.task.number} with ${run.runner.display}..."
      )
      output <- AgentExecutor[F].run(
        run.runner,
        prompt,
        run.worktreePath,
        ImplementerAllowedTools
      )
      _ <- Sync[F]
        .raiseError(
          RuntimeException(
            s"Agent ${run.runner.display} reported it could not proceed (permission/tool wall). Output: ${output.trim}"
          )
        )
        .whenA(looksBlocked(output))
    yield TaskWithOutput(run, output)

  // Exit code 0 only means the process returned; a stuck agent that gave up
  // after every tool call was denied also exits 0 with prose explaining why,
  // and with no files changed that reads as a legitimate no-op success
  // (see reportUnchangedTask/closeTask). Catch that specific failure shape
  // here so it surfaces as a real error (and triggers runExecutor's
  // stronger-runner fallback) instead of silently closing the task.
  private val BlockedSignals = List(
    "tool calls (and file-creating bash) are being denied",
    "being denied",
    "need approval i'm not getting",
    "hit wall:",
    "permission denied and cannot continue"
  )

  private def looksBlocked(output: String): Boolean =
    val lower = output.toLowerCase
    BlockedSignals.exists(lower.contains)

  private def commentOutput[F[_]: Sync]
      : -->[F, TaskWithOutput, TaskWithOutput] =
    Kleisli { task =>
      GitHub
        .commentRunOutput(
          task.run.context.root,
          task.run.task.number,
          task.output,
          progress
        )
        .as(task)
    }

  private def runProjectValidation[F[_]: Sync]
      : -->[F, TaskWithOutput, TaskWithOutput] =
    Kleisli { task =>
      Git[F]
        .runProjectValidation(task.run.worktreePath, progress)
        .as(task)
    }

  private def commitIfChanged[F[_]: Sync]
      : -->[F, TaskWithOutput, TaskWithOutput] =
    traced("changedPlan", changedPlan[F]) >>>
      choose[F, ChangedTask, UnchangedTask, TaskWithOutput](
        traced("commitChangedTask", commitChangedTask[F]),
        traced("reportUnchangedTask", reportUnchangedTask[F])
      )

  private def changedPlan[F[_]: Sync]
      : -->[F, TaskWithOutput, Either[ChangedTask, UnchangedTask]] =
    Kleisli { task =>
      for
        filesChanged <- Git[F].filesChanged(task.run.worktreePath)
        hasPublishableCommits <- Git[F].hasPublishableCommits(
          task.run.worktreePath,
          task.run.branchName
        )
        hasOpenPullRequest <- GitHub.hasOpenPullRequestForBranch(
          task.run.worktreePath,
          task.run.branchName
        )
      yield
        if filesChanged || hasPublishableCommits || hasOpenPullRequest then
          Left(ChangedTask(task))
        else Right(UnchangedTask(task))
    }

  private def commitChangedTask[
      F[_]: Sync
  ]: -->[F, ChangedTask, TaskWithOutput] =
    Kleisli { changed =>
      val run = changed.run.run
      commitAndMergeOrPublish(
        run.context.root,
        run.worktreePath,
        run.branchName,
        run.task,
        extractAgentFinalization(changed.run.output)
      ).as(changed.run)
    }

  private def reportUnchangedTask[
      F[_]: Sync
  ]: -->[F, UnchangedTask, TaskWithOutput] =
    Kleisli { unchanged =>
      progress("No files changed.").as(unchanged.run)
    }

  private def verifyReplayCi[F[_]: Sync]
      : -->[F, TaskWithOutput, TaskWithOutput] =
    Kleisli { task =>
      GitHub
        .verifyTaskReplayCi(
          task.run.context.root,
          task.run.task,
          task.run.branchName,
          progress
        )
        .handleErrorWith { error =>
          GitHub
            .commentTaskFailure(
              task.run.context.root,
              task.run.task,
              error.getMessage,
              progress
            ) *> Sync[F].raiseError(error)
        }
        .as(task)
    }

  private def cleanupTaskWorktree[
      F[_]: Sync
  ]: -->[F, TaskWithOutput, TaskWithOutput] =
    Kleisli { task =>
      Git[F]
        .cleanupWorktree(
          task.run.context.root,
          task.run.worktreePath,
          task.run.branchName,
          progress
        )
        .as(task)
    }

  private def closeTask[F[_]: Sync]: -->[F, TaskWithOutput, TaskRun] =
    Kleisli { task =>
      val run = task.run
      for
        _ <- progress(s"Closing task #${run.task.number} with comment...")
        _ <- GitHub.commentConclusion(
          run.context.root,
          run.task,
          run.runner,
          progress
        )
        _ <- GitHub.setIssueStatus(
          run.context.root,
          run.task.number,
          "completed",
          progress
        )
        _ <- GitHub.closeIssue(run.context.root, run.task.number)
        _ <- GitHub.checkParentsForCompletion(
          run.context.root,
          run.task,
          progress
        )
        _ <- progress("Task execution finished successfully.")
      yield run
    }

  private def choose[F[_]: Sync, A, B, C](
      left: -->[F, A, C],
      right: -->[F, B, C]
  ): -->[F, Either[A, B], C] =
    Kleisli { input =>
      val branch = input.fold(_ => "left", _ => "right")
      val value = input.fold(identity, identity)
      TaskLogger.trace[F](
        s"decision choose branch=$branch input=${summarize(value)}"
      ) *>
        ArrowChoice[Flow[F]].choice(left, right).run(input)
    }

  private def traced[F[_]: Sync, A, B](
      name: String,
      flow: -->[F, A, B]
  ): -->[F, A, B] =
    Kleisli { input =>
      TaskLogger.trace[F](s"enter $name input=${summarize(input)}") *>
        flow
          .run(input)
          .flatTap(output =>
            TaskLogger.trace[F](s"exit $name output=${summarize(output)}")
          )
          .handleErrorWith { error =>
            TaskLogger.trace[F](
              s"fail $name error=${error.getClass.getSimpleName}: ${error.getMessage}"
            ) *> Sync[F].raiseError(error)
          }
    }

  private def summarize(value: Any): String =
    value match
      case null => "null"
      case AppInput(root, taskNumber) =>
        s"AppInput(root=$root,task=${taskNumber.fold("auto")(_.toString)})"
      case RunContext(root, agentInventory, taskNumber) =>
        s"RunContext(root=$root,task=${taskNumber.fold("auto")(_.toString)},availableAgents=${agentInventory.availableTools.size})"
      case Issue(number, title, body, state) =>
        s"Issue(#$number,title=${quote(title)},state=$state,bodyChars=${body.length})"
      case TaskRunner(agent, model, effort, version) =>
        s"TaskRunner(agent=$agent,model=${model.getOrElse("")},effort=${effort
            .getOrElse("")},version=${version.getOrElse("")})"
      case RunnableTask(issue, runner) =>
        s"RunnableTask(issue=#${issue.number},runner=${runner.display})"
      case TaskSelection(_, candidates) =>
        s"TaskSelection(candidates=${candidates.map(t => s"#${t.issue.number}").mkString(",")})"
      case NoTask(_) =>
        "NoTask"
      case TaskRun(_, task, runner, worktreePath, branchName) =>
        s"TaskRun(issue=#${task.number},runner=${runner.display},worktree=$worktreePath,branch=$branchName)"
      case TaskWithPrompt(run, parentConclusion, replayContext) =>
        s"TaskWithPrompt(issue=#${run.task.number},hasDependencyConclusion=${parentConclusion.nonEmpty},hasReplayContext=${replayContext.nonEmpty})"
      case TaskWithOutput(run, output) =>
        s"TaskWithOutput(issue=#${run.task.number},outputChars=${output.length})"
      case NeedsUserInput(run, questions) =>
        s"NeedsUserInput(issue=#${run.task.number},questionChars=${questions.length})"
      case SplitTask(run) =>
        s"SplitTask(issue=#${run.task.number})"
      case TaskEvaluation(body, questions, execution) =>
        s"TaskEvaluation(execution=$execution,bodyChars=${body.length},hasQuestions=${questions
            .exists(_.trim.nonEmpty)})"
      case ExistingBranch(run) =>
        s"ExistingBranch(issue=#${run.run.task.number},branch=${run.run.branchName})"
      case NewBranch(run) =>
        s"NewBranch(issue=#${run.run.task.number},branch=${run.run.branchName})"
      case ChangedTask(run) =>
        s"ChangedTask(issue=#${run.run.task.number})"
      case UnchangedTask(run) =>
        s"UnchangedTask(issue=#${run.run.task.number})"
      case RunSummary(status, message, task) =>
        s"RunSummary(status=$status,task=${task
            .map(_.number)
            .fold("none")(number => s"#$number")},message=${quote(message)})"
      case Left(value) =>
        s"Left(${summarize(value)})"
      case Right(value) =>
        s"Right(${summarize(value)})"
      case Some(value) =>
        s"Some(${summarize(value)})"
      case None =>
        "None"
      case list: List[?] =>
        s"List(size=${list.size})"
      case seq: Seq[?] =>
        s"Seq(size=${seq.size})"
      case other =>
        truncate(other.toString, 240)

  private def quote(value: String): String =
    "\"" + truncate(value.linesIterator.mkString("\\n"), 120) + "\""

  private def truncate(value: String, maxLength: Int): String =
    if value.length <= maxLength then value
    else value.take(maxLength) + "...[truncated]"

  private def taskRun(
      context: RunContext,
      task: Issue,
      runner: TaskRunner
  ): TaskRun =
    val taskId = task.number
    val taskName = taskSlug(task.title).getOrElse(s"task-$taskId")
    TaskRun(
      context = context,
      task = task,
      runner = runner,
      worktreePath = context.root / ".worktrees" / s"$taskName-$taskId",
      branchName = s"task-$taskId"
    )

  private def taskSlug(title: String): Option[String] =
    val slug = title.toLowerCase
      .map(char => if char.isLetterOrDigit then char else '-')
      .mkString
      .replaceAll("-+", "-")
      .stripPrefix("-")
      .stripSuffix("-")
      .take(60)
    Option.when(slug.nonEmpty)(slug)

  private def taskPrompt(
      task: Issue,
      runner: TaskRunner,
      dependencyConclusion: Option[String],
      replayContext: Option[String]
  ): String =
    val dependencyConclusionStr = dependencyConclusion
      .map(comment => s"\nDependency Task Conclusion Comment:\n$comment\n")
      .getOrElse("")
    val replayContextStr = replayContext
      .map(context => s"""
Replay / repair context:
$context

Replay rules:
- This task was reopened or resumed after a prior script/agent run.
- Continue from the current repository, branch, PR, and worktree state.
- Do not repeat completed work unless needed to repair the failure.
- Focus on the latest failure context above, for example failed CI, build output, or user restart comment.
- If the previous implementation was already merged, create the minimal follow-up fix in this task branch.
""").getOrElse("")

    s"""Task ID: #${task.number}
Title: ${task.title}
Agent: ${runner.agent}
Model: ${runner.model.getOrElse("")}
Effort: ${runner.effort.getOrElse("")}
Version: ${runner.version.getOrElse("")}

Task Description:
${task.body}
$dependencyConclusionStr
$replayContextStr
Workflow:
1. First estimate the task size and complexity before editing files.
2. If the task is too broad, ambiguous, risky, or naturally decomposes into independent steps, split it instead of implementing it directly.
3. When splitting, create GitHub subtasks with clear, detailed descriptions and narrow scope. Each subtask should include:
   - parent: #${task.number}
   - dependencies on earlier subtasks when order matters
   - concrete acceptance criteria
   - preferred llms/models/efforts/versions as a ranked list
4. Prefer splitting until each subtask is small enough that a weaker model such as Haiku could implement it without needing another split.
5. Use this exact preferred-runner metadata format in every subtask description:
   preferred llms/models/efforts/versions:
   - claude/haiku//<version>
   - codex/gpt-5/high/<version>
6. If you split the task, do not implement the parent task. Comment on the parent with the created subtask numbers and the reason for the split.
7. If the task is already narrow enough, implement it in the current repository and make any necessary file changes.

Agent boundary:
- Do not run tree2m.
- Do not run git worktree commands.
- Do not run git commit.
- Do not run git push.
- Do not create, update, merge, or close pull requests.
- The executor script owns worktree setup, commit, push, pull request creation/merge, and cleanup after you return.
- You may run local inspection, edit, format, compile, and test commands needed to complete and verify the implementation.
- If you need a commit or pull request message, include it in your final answer instead of running the command.

Final answer contract:
- Summarize the implementation.
- List validation commands you ran and whether they passed.
- Include a proposed commit title.
- Include a proposed pull request body when useful.
"""

  // Scoped tool permission for the evaluator run only: "split" verdicts
  // require it to actually create child issues, which a non-interactive
  // `claude -p` run otherwise can't do — there's no TTY to approve the Bash
  // call, so it stalls out asking for confirmation instead of producing the
  // required JSON. `gh issue edit` is deliberately NOT granted: that would
  // let the evaluator rewrite an issue body directly, bypassing the
  // comment-only TaskMetadata architecture (see taskMetadata.scala) the same
  // way the old updateIssueBody call used to. Linking a new subtask back to
  // its parent should go through `gh issue comment` instead.
  private val EvaluatorAllowedTools = Seq(
    "Bash(gh issue create:*)",
    "Bash(gh issue comment:*)"
  )

  // Forces the claude CLI's final response to conform to this shape (via
  // --json-schema) instead of relying on the prompt's "Return only JSON"
  // instruction and hoping the model complies. This is what parseTaskEvaluation
  // expects; matching it here should turn most JSON-parse failures into
  // guaranteed-valid output instead. Only "claude" consumes jsonSchema
  // (TaskRunner.command) — a no-op for other evaluator runners.
  private val EvaluationJsonSchema =
    """{"type":"object","properties":{"status":{"type":"string","enum":["ready","split","questions"]},"body":{"type":"string"},"questions":{"type":"string"}},"required":["status","body"]}"""

  // Implementer runs unattended (`-p`, stdin closed) with zero tool grants
  // previously: no --allowedTools meant every Edit/Write/Bash call hit the
  // permission wall with nobody to approve it, so the agent gave up, printed
  // an explanation, and exited 0 with no files changed — which the pipeline
  // then read as a legitimate no-op success. Grant the tools implementation
  // actually needs; cwd is already confined to the task's own worktree.
  private val ImplementerAllowedTools = Seq(
    "Edit",
    "Write",
    "MultiEdit",
    "Read",
    "Glob",
    "Grep",
    "Bash"
  )

  private def evaluateTaskPrompt(
      task: Issue,
      dependencyConclusion: Option[String],
      agentInventory: AgentInventory,
      userAnswer: Option[String] = None
  ): String =
    val dependencyConclusionStr = dependencyConclusion
      .map(comment => s"\nDependency Task Conclusion Comment:\n$comment\n")
      .getOrElse("")
    val userAnswerStr = userAnswer
      .map(answer =>
        s"\nUser's answer to previous clarifying questions:\n$answer\n\nIncorporate this answer. Only set status to \"questions\" again if the answer still leaves essential information missing.\n"
      )
      .getOrElse("")
    val descriptionState =
      if task.body.trim.isEmpty then "missing"
      else if strongDescription(task.body) then "strong"
      else "weak"

    s"""Evaluate and prepare this GitHub task before implementation.

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
- Write preferred runners as a ranked list. Each item must be exactly agent/model/effort/version; leave effort or version empty when the tool has no value, for example claude/sonnet//1.0.0.
- Include Context, Goal, Scope, Acceptance Criteria, and Notes/Risks when useful.
- Evaluate whether the task should be split before implementation. Record the evaluation in the body.
- If splitting is needed, create GitHub subtasks before returning. Each subtask must have parent, dependencies if needed, acceptance criteria, narrow scope, and preferred llms/models/efforts/versions.
- Prefer a target scope that a weaker model such as Haiku could implement without another split.
- If important information is missing and cannot be inferred, set status to "questions" and put concrete user questions in "questions".
- If questions are needed, still provide the best improved body possible in "body".
- Do not implement code changes.
"""

  private def parseTaskEvaluation(
      output: String,
      fallbackBody: String
  ): TaskEvaluation =
    val stripped =
      extractJsonObject(output).getOrElse(stripMarkdownFence(output)).trim
    Try {
      val json = ujson.read(stripped)
      TaskEvaluation(
        body = json.obj
          .get("body")
          .collect { case ujson.Str(value) => value }
          .filter(_.trim.nonEmpty)
          .getOrElse(fallbackBody),
        questions = json.obj
          .get("questions")
          .collect { case ujson.Str(value) => value }
          .filter(_.trim.nonEmpty),
        execution = json.obj
          .get("status")
          .collect { case ujson.Str(value) => normalizeExecution(value) }
          .getOrElse("needs-input")
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
        execution = "needs-input"
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

  private def existingEvaluation(
      task: Issue,
      hasRealQuestion: Boolean
  ): Option[TaskEvaluation] =
    (evaluationStatus(task.body), executionStatus(task.body)) match
      case (Some("ready"), Some("implement")) if hasRunnerMetadata(task.body) =>
        Some(TaskEvaluation(task.body, None, execution = "implement"))
      case (Some("ready"), Some("split")) if hasRunnerMetadata(task.body) =>
        Some(TaskEvaluation(task.body, None, execution = "split"))
      case (Some("needs-input"), _) | (_, Some("needs-input"))
          if hasRealQuestion =>
        TaskEvaluation(
          task.body,
          Some(
            "Task is already marked as needing user input. See the \"Questions before execution:\" comment on this issue for details."
          ),
          execution = "needs-input"
        ).some
      // needs-input metadata but no question was ever actually posted:
      // not a genuine block, fall through to a real evaluation.
      case _ => None

  private def evaluationStatus(body: String): Option[String] =
    metadataValue(body, "evaluation")

  private def executionStatus(body: String): Option[String] =
    metadataValue(body, "execution").map(normalizeExecution)

  private def metadataValue(body: String, key: String): Option[String] =
    val prefix = s"$key:"
    body.linesIterator
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

  private def strongDescription(body: String): Boolean =
    val lower = body.toLowerCase
    val hasStructure =
      List("context", "goal", "scope", "acceptance").count(lower.contains) >= 2
    val hasEnoughDetail = body.trim.length >= 500
    hasStructure && hasEnoughDetail && hasRunnerMetadata(body)

  private def hasRunnerMetadata(body: String): Boolean =
    val lower = body.toLowerCase
    lower.contains("preferred llms/models/versions") ||
    lower.contains("preferred llms/models/efforts/versions") ||
    lower.contains("agent/model/version") ||
    lower.contains("agent/model/effort/version")

  private def stripMarkdownFence(value: String): String =
    val trimmed = value.trim
    if trimmed.startsWith("```") && trimmed.endsWith("```") then
      trimmed.linesIterator.toList.drop(1).dropRight(1).mkString("\n")
    else trimmed

  private def extractAgentFinalization(output: String): AgentFinalization =
    AgentFinalization(
      commitTitle = extractPrefixedLine(output, "Proposed commit title"),
      pullRequestBody = extractSection(output, "Proposed pull request body")
    )

  private def extractPrefixedLine(
      output: String,
      label: String
  ): Option[String] =
    val prefix = s"$label:"
    output.linesIterator
      .map(_.trim)
      .collectFirst {
        case line if line.toLowerCase.startsWith(prefix.toLowerCase) =>
          line.drop(prefix.length).trim
      }
      .filter(_.nonEmpty)

  private def extractSection(output: String, label: String): Option[String] =
    val prefix = s"$label:"
    val lines = output.linesIterator.toList
    val start =
      lines.indexWhere(_.trim.toLowerCase.startsWith(prefix.toLowerCase))
    Option
      .when(start >= 0) {
        val firstLine = lines(start).trim.drop(prefix.length).trim
        val following = lines.drop(start + 1)
        val body =
          if firstLine.nonEmpty then firstLine
          else
            following
              .takeWhile(line => !isFinalizationLabel(line))
              .mkString("\n")
              .trim
        body
      }
      .filter(_.nonEmpty)

  private def isFinalizationLabel(line: String): Boolean =
    val normalized = line.trim.toLowerCase
    normalized.startsWith("proposed commit title:") ||
    normalized.startsWith("proposed pull request body:")

  private def commitAndMerge[F[_]](
      root: os.Path,
      worktreePath: os.Path,
      branchName: String,
      task: Issue,
      finalization: AgentFinalization
  )(using Sync[F]): F[Unit] =
    for
      _ <- progress("Files changed. Committing and merging changes...")
      _ <- Git[F].commitAll(worktreePath, task, finalization.commitTitle)
      hasRemote <- Git[F].hasRemote(root)
      _ <-
        if hasRemote then
          GitHub.pushCreateAndMergePr(
            root,
            worktreePath,
            branchName,
            task,
            finalization.commitTitle,
            finalization.pullRequestBody,
            progress
          )
        else Git[F].mergeLocally(root, worktreePath, branchName, progress)
    yield ()

  private def commitAndMergeOrPublish[F[_]](
      root: os.Path,
      worktreePath: os.Path,
      branchName: String,
      task: Issue,
      finalization: AgentFinalization
  )(using Sync[F]): F[Unit] =
    for
      filesChanged <- Git[F].filesChanged(worktreePath)
      _ <-
        if filesChanged then
          commitAndMerge(root, worktreePath, branchName, task, finalization)
        else
          publishExistingCommits(
            root,
            worktreePath,
            branchName,
            task,
            finalization
          )
    yield ()

  private def publishExistingCommits[F[_]](
      root: os.Path,
      worktreePath: os.Path,
      branchName: String,
      task: Issue,
      finalization: AgentFinalization
  )(using Sync[F]): F[Unit] =
    for
      _ <- progress("No file changes, publishing existing local commits...")
      hasRemote <- Git[F].hasRemote(root)
      _ <-
        if hasRemote then
          GitHub.pushCreateAndMergePr(
            root,
            worktreePath,
            branchName,
            task,
            finalization.commitTitle,
            finalization.pullRequestBody,
            progress
          )
        else Git[F].mergeLocally(root, worktreePath, branchName, progress)
    yield ()

  private def progress[F[_]: Sync](message: String): F[Unit] =
    TaskLogger.script(message)

  private def parseTaskNumber(args: List[String]): Option[Int] =
    args
      .collectFirst {
        case value if value.startsWith("--task=") =>
          value.stripPrefix("--task=")
        case value if value.startsWith("--issue=") =>
          value.stripPrefix("--issue=")
      }
      .orElse {
        args.sliding(2).collectFirst { case List("--task" | "--issue", value) =>
          value
        }
      }
      .flatMap(_.trim.stripPrefix("#").toIntOption)

  private def removeScriptArgs(args: List[String]): List[String] =
    @tailrec
    def loop(
        remaining: List[String],
        clean: List[String]
    ): List[String] =
      remaining match
        case Nil => clean.reverse
        case ("--executor" | "--llm" | "--agent" | "--model" | "--task" |
            "--issue") :: _ :: tail =>
          loop(tail, clean)
        case flag :: Nil
            if flag === "--executor" || flag === "--llm" ||
              flag === "--agent" || flag === "--model" ||
              flag === "--task" || flag === "--issue" =>
          loop(Nil, clean)
        case flag :: tail
            if flag.startsWith("--task=") || flag.startsWith("--issue=") =>
          loop(tail, clean)
        case head :: tail =>
          loop(tail, head :: clean)

    loop(args, Nil)
