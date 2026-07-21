import arrowstep.core.*
import arrowstep.runtime.AgentMain
import cats.Monoid
import cats.data.Kleisli
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.Ref
import cats.effect.Resource
import cats.effect.kernel.Sync
import cats.syntax.all
import cats.syntax.all.*
import io.github.mercurievv.minuscles.fieldsnames.derivation.semiauto.FieldNamesDerivation.fieldsNames

import scala.annotation.tailrec
import scala.concurrent.duration.*
import scala.util.Try

import ArrowLogging.*

object Main extends IOApp:

  type -->[F[_], A, B] = Kleisli[F, A, B]
  type Flow[F[_]] = [A, B] =>> Kleisli[F, A, B]
  private val evaluatorRunner: TaskRunner =
    TaskRunner("claude", Some("opus"), None, None)
  private val UserInputWaitMillis =
    envLong("GH_TASKS_USER_INPUT_WAIT_MINUTES", 120).minutes.toMillis
  private val UserInputPollMillis =
    envLong("GH_TASKS_USER_INPUT_POLL_SECONDS", 30).seconds.toMillis
  private val UserInputSoundEnabled =
    sys.env
      .get("GH_TASKS_USER_INPUT_SOUND")
      .forall(value =>
        !Set("0", "false", "no", "off").contains(value.toLowerCase)
      )

  def run(args: List[String]): IO[ExitCode] =
    val taskNumber = parseTaskNumber(args)
    val recursive = parseRecursiveFlag(args)
    val arrowstepArgs = removeScriptArgs(args)
    AgentMain
      .run[IO](arrowstepArgs, os.pwd)(_ =>
        businessLogic[IO].programArrows.program(
          AppInput(os.pwd, taskNumber, recursive)
        )
      )
      .flatMap { outcome =>
        IO.print(outcome.stdout) *>
          IO.pure(ExitCode(outcome.exitCode))
      }

  private def businessLogic[F[_]: Sync]: BusinessLogic[Flow[F]] =
    val changeArrows = ChangeArrows[Flow[F]](
      changedPlan = changedPlan[F],
      commitChangedTask = commitChangedTask[F],
      reportUnchangedTask = reportUnchangedTask[F]
    )
    BusinessLogic[Flow[F]](
      programArrows = ProgramArrows[Flow[F]](
        resolveContext = resolveContext[F],
        selectTask = selectTask[F],
        partitionCandidates = partitionCandidates[F],
        noTaskSummary = noTaskSummary[F],
        executeNonEmptySelection = executeNonEmptySelection[F],
        toProgramSays =
          Kleisli(summary => ProgramSays.Done(summary.toJson).pure[F])
      ),
      taskArrows = TaskArrows[Flow[F]](
        resumePlan = resumePlan[F],
        resumeExistingPullRequest = resumeExistingPullRequest[F],
        announceTask = announceTask[F],
        fetchTaskContext = fetchTaskContext[F],
        evaluateTask = evaluateTask[F],
        needsUserInputSummary = needsUserInputSummary[F],
        splitTaskSummary = splitTaskSummary[F],
        markInProgress = markInProgress[F],
        runPreparedTask = runPreparedTask[F],
        completedTaskSummary = completedTaskSummary[F]
      ),
      changeArrows = changeArrows,
      publicationArrows = PublicationArrows[Flow[F]](
        publicationPlan = publicationPlan[F],
        prepareChangedPublication = prepareChangedPublication[F],
        prepareExistingPublication = prepareExistingPublication[F],
        publishTransportPlan = publishTransportPlan[F],
        publishRemote = publishRemote[F],
        publishLocal = publishLocal[F]
      ),
      preparedTaskArrows = PreparedTaskArrows[Flow[F]](
        runExecutor = runExecutor[F],
        runProjectValidation = runProjectValidation[F],
        commentOutput = commentOutput[F],
        verifyReplayCi = verifyReplayCi[F],
        closeTask = closeTask[F]
      )
    ).withArrowLogging(arrowLogger[F])

  private def resolveContext[F[_]: Sync]: -->[F, AppInput, RunContext] =
    Kleisli { input =>
      AgentInventory
        .loadF[F](input.root)
        .map(RunContext(input.root, _, input.taskNumber, input.recursive))
    }

  private def selectTask[F[_]: Sync]: -->[F, RunContext, TaskSelection] =
    Kleisli { (context: RunContext) =>
      for
        _ <- progress("Fetching open issues from GitHub...")
        rawIssues <- GitHub.fetchIssues(context.root)
        issues <- rawIssues.traverse(effectiveIssue[F](context.root, _))
        openIssueNumbers = issues.map(_.number).toSet
        candidatesByNumber = issues.filter(task =>
          context.taskNumber.forall(_ === task.number)
        )
        filteredByDeps <- candidatesByNumber.filterA { task =>
          if context.taskNumber.isDefined then
            // If a specific task is targeted, do not filter out by dependencies/children.
            // We want to run it and traverse its tree recursively!
            true.pure[F]
          else if context.recursive then
            // --recursive walks each root's dependency tree to closure, so an
            // unresolved dependency is not a reason to exclude a root candidate
            // (unlike above, still exclude issues that are themselves still a
            // child/dependency of another open issue, to avoid entering the
            // same subtree from more than one root entry point).
            val openChildren = GitHub.hasOpenChildren(task, issues)
            Option
              .when(openChildren)("has open child tasks")
              .traverse_(why =>
                TaskLogger.trace(s"selectTask excluding #${task.number}: $why")
              )
              .as(!openChildren)
          else
            val unresolvedDeps =
              GitHub.hasUnresolvedDependencies(task, openIssueNumbers)
            val openChildren =
              if unresolvedDeps then false
              else GitHub.hasOpenChildren(task, issues)
            val excluded = unresolvedDeps || openChildren
            val reason =
              if unresolvedDeps then Some("has unresolved dependencies")
              else if openChildren then Some("has open child tasks")
              else None
            reason
              .traverse_(why =>
                TaskLogger.trace(s"selectTask excluding #${task.number}: $why")
              )
              .as(!excluded)
        }
        notAlreadyClaimed <- filteredByDeps.filterA { task =>
          val claimed = task.labels.exists(label =>
            label === "status: in progress" || label === "in progress"
          )
          Option
            .when(claimed)("already labeled in progress (claimed by another run)")
            .traverse_(why =>
              TaskLogger.trace(s"selectTask excluding #${task.number}: $why")
            )
            .as(!claimed)
        }
        eligible <- notAlreadyClaimed.filterA { task =>
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
                    .flatTap { hasAnswer =>
                      if hasAnswer then ().pure[F]
                      else
                        TaskLogger.trace(
                          s"selectTask excluding #${task.number}: needs-input question posted, no user answer yet"
                        )
                    }
              }
          needsInputCheck
        }
        // A branch/PR from an earlier run may still be open (e.g. it was
        // interrupted before merging): re-running the implementer would
        // create a second local branch of the same name and diverge, so
        // resume that PR (verify checks, merge) instead of re-implementing.
        eligibleWithResumeFlag <- eligible.traverse { task =>
          GitHub
            .hasOpenPullRequestForBranch(context.root, s"task-${task.number}")
            .flatTap { hasOpenPr =>
              if !hasOpenPr then ().pure[F]
              else
                TaskLogger.trace(
                  s"selectTask resuming #${task.number}: an open Pull Request for task-${task.number} already exists; will verify/merge instead of re-implementing"
                )
            }
            .map(hasOpenPr => (task, hasOpenPr))
        }
        candidates = eligibleWithResumeFlag.map { case (task, hasOpenPr) =>
          RunnableTask(
            context,
            task,
            context.agentInventory
              .selectRunner(GitHub.taskRunners(task))
              .getOrElse(evaluatorRunner),
            resumePullRequest = hasOpenPr
          )
        }
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
  private def effectiveIssue[F[_]: Sync](
      root: os.Path,
      issue: Issue
  ): F[Issue] =
    TaskMetadataStore
      .commentBased[F]
      .read(root, issue)
      .map(merged => issue.copy(body = TaskMetadata.render(merged)))

  private def partitionCandidates[F[_]: Sync]
      : -->[F, TaskSelection, Either[NoTask, TaskSelection]] =
    Kleisli.fromFunction { selection =>
      Either.cond(!(selection.candidates.isEmpty), selection, NoTask(selection.context))
    }

  private def executeNonEmptySelection[F[_]: Sync]
      : -->[F, TaskSelection, RunSummary] =
    Kleisli { selection =>
      val context = selection.context
      val candidates = selection.candidates
      for
        openIssues <- GitHub.fetchIssues(context.root)
        openIssuesMap = openIssues.map(i => i.number -> i).toMap
        openIssuesRef <- Ref[F].of(openIssuesMap)
        recursiveFlow = executeRecursive[F](context, openIssuesRef)
        summaries <- candidates.traverse { candidate =>
          if context.recursive then
            runRootUntilClosed[F](
              context,
              openIssuesRef,
              recursiveFlow,
              candidate.issue.number
            )
          else
            for
              currentMap <- openIssuesRef.get
              summary <- currentMap.get(candidate.issue.number) match {
                case Some(latestIssue) =>
                  recursiveFlow.run(latestIssue)
                case None =>
                  RunSummary(status = "completed", message = s"Task #${
    candidate.issue.number
  } is already completed.", task = Some(candidate.issue)).pure[F]
              }
            yield summary
        }
      yield summaries.lastOption.getOrElse(
        RunSummary(
          status = "completed",
          message = "All candidates completed.",
          task = None
        )
      )
    }

  private val RecursiveRootIterationCap = 50

  // Repeats the dependency-tree walk against a single root task until it
  // closes: a split mid-tree creates new sub-issues that `openIssues` (a
  // one-shot snapshot) doesn't know about, so a single pass can short-circuit
  // without ever running them. Refetching and retrying picks those up on the
  // next pass, same as re-invoking the script by hand would.
  private def runRootUntilClosed[F[_]: Sync](
      context: RunContext,
      openIssues: Ref[F, Map[Int, Issue]],
      recursiveFlow: -->[F, Issue, RunSummary],
      rootNumber: Int
  ): F[RunSummary] =
    def loop(iteration: Int, previous: Option[RunSummary]): F[RunSummary] =
      for
        freshIssues <- GitHub.fetchIssues(context.root)
        freshMap = freshIssues.map(i => i.number -> i).toMap
        _ <- openIssues.set(freshMap)
        result <- freshMap.get(rootNumber) match
          case None =>
            RunSummary(
              status = "completed",
              message = s"Task #$rootNumber is already completed.",
              task = None
            ).pure[F]
          case Some(rootIssue) =>
            for
              summary <- recursiveFlow.run(rootIssue)
              next <-
                if summary.status === "needs-input" then summary.pure[F]
                else if previous.contains(summary) then
                  progress(
                    s"Task #$rootNumber made no further progress under --recursive; stopping."
                  ).as(summary)
                else if iteration >= RecursiveRootIterationCap then
                  progress(
                    s"Task #$rootNumber hit the --recursive iteration cap ($RecursiveRootIterationCap); stopping."
                  ).as(summary)
                else loop(iteration + 1, Some(summary))
            yield next
      yield result
    loop(1, None)

  private def executeRecursive[F[_]: Sync](
      context: RunContext,
      openIssues: Ref[F, Map[Int, Issue]]
  ): -->[F, Issue, RunSummary] =
    RecursiveArrows[Flow[F]](
      checkIfCompleted = checkIfCompleted[F],
      runDependencies = self => runDependencies[F](openIssues, self),
      claimAndRun = claimAndRun[F](context),
      defer = self => Kleisli(issue => self.run(issue))
    ).executeRecursive

  private def checkIfCompleted[F[_]: Sync]
      : -->[F, Issue, Either[RunSummary, Issue]] =
    Kleisli { issue =>
      if issue.state.toLowerCase == "closed" then
        Left(RunSummary(status = "completed", message = s"Task #${
  issue.number
} is already completed.", task = Some(issue))).pure[F]
      else Right(issue).pure[F]
    }

  private def runDependencies[F[_]: Sync](
      openIssues: Ref[F, Map[Int, Issue]],
      executeRecursive: -->[F, Issue, RunSummary]
  ): -->[F, Issue, Either[RunSummary, Issue]] =
    Kleisli { issue =>
      val depIds = GitHub.getDependencies(issue.body).distinct
      for
        issuesMap <- openIssues.get
        openDeps = depIds.flatMap(issuesMap.get)
        dependencyResult <- openDeps.foldLeftM[F, Either[RunSummary, Unit]](
          Right(())
        ) {
          case (Left(failedSummary), _) => Left(failedSummary).pure[F]
          case (Right(_), depIssue) =>
            executeRecursive.run(depIssue).map { summary =>
              Either.cond(summary.status == "completed", (), summary)
            }
        }
      yield dependencyResult.as(issue)
    }

  private def claimAndRun[F[_]: Sync](
      context: RunContext
  ): -->[F, Issue, RunSummary] =
    Kleisli { issue =>
      val runner = context.agentInventory
        .selectRunner(GitHub.taskRunners(issue))
        .getOrElse(evaluatorRunner)
      val runnable = RunnableTask(context, issue, runner)

      IssueClaim
        .acquire[F](context.root, issue.number, progress)
        .use { _ =>
          businessLogic[F].taskArrows.resumeChoice.run(runnable)
        }
        .recoverWith { case _: IssueAlreadyClaimedException =>
          progress(
            s"Task #${issue.number} is claimed by another process. Waiting for completion..."
          ) *>
            pollGitHubForCompletion[F](context.root, issue.number).map {
              finalIssue =>
                RunSummary(
                  status = "completed",
                  message =
                    s"Task #${issue.number} was completed by another process.",
                  task = Some(finalIssue)
                )
            }
        }
    }

  private def pollGitHubForCompletion[F[_]: Sync](
      root: os.Path,
      taskId: Int
  ): F[Issue] =
    val pollInterval = 30.seconds
    def loop: F[Issue] =
      GitHub.fetchIssues(root).flatMap { issues =>
        issues.find(_.number == taskId) match
          case Some(issue) if issue.state.toLowerCase == "closed" =>
            issue.pure[F]
          case None =>
            Issue(taskId, s"Task #$taskId", "", "closed").pure[F]
          case _ =>
            Sync[F].blocking(Thread.sleep(pollInterval.toMillis)) *> loop
      }
    loop

  private def noTaskSummary[F[_]: Sync]: -->[F, NoTask, RunSummary] =
    Kleisli { noTask =>
      val context = noTask.context
      val message = context.taskNumber.fold(
        "No tasks found without unresolved dependencies, open child tasks, or another process already claiming them."
      )(number =>
        s"No runnable open task found for #$number. It may be closed, blocked by dependencies or open child tasks, marked needs-input, or already claimed by another process."
      )
      RunSummary(status = "no-task", message = message, task = None).pure[F]
    }

  private def resumePlan[F[_]: Sync]
      : -->[F, RunnableTask, Either[TaskRun, TaskRun]] =
    Kleisli { task =>
      val run = taskRun(task.context, task.issue, task.runner)
      val shouldResume =
        if task.resumePullRequest then
          GitHub
            .hasOpenPullRequestForBranch(task.context.root, run.branchName)
            .flatTap { stillHasOpenPr =>
              if stillHasOpenPr then ().pure[F]
              else
                progress(
                  s"No open Pull Request remains for ${run.branchName}; creating a new run instead of resuming."
                )
            }
        else false.pure[F]

      shouldResume.map(resumePullRequest =>
        Either.cond(resumePullRequest, run, run)
      )
    }

  private def completedTaskSummary[F[_]: Sync]: -->[F, TaskRun, RunSummary] =
    Kleisli { run =>
      RunSummary(
        status = "completed",
        message = s"Task #${run.task.number} completed successfully.",
        task = Some(run.task)
      ).pure[F]
    }

  private def runPreparedTask[
      F[_]: Sync
  ]: -->[F, TaskWithPrompt, TaskRun] =
    Kleisli { task =>
      worktreeResource[F](task).use { acquiredTask =>
        businessLogic[F].executeAcquiredTask
          .run(acquiredTask)
          .onError { case _ =>
            Git[F].preserveUnpushedCommits(
              acquiredTask.run.worktreePath,
              acquiredTask.run.branchName,
              acquiredTask.run.baseBranch,
              progress
            )
          }
      }
    }

  private def needsUserInputSummary[
      F[_]: Sync
  ]: -->[F, NeedsUserInput, RunSummary] =
    TaskLogger
      .progress[F, NeedsUserInput](input =>
        s"Task #${input.run.task.number} still needs user input; stopping this iteration."
      )
      .map(input =>
        RunSummary(
          status = "needs-input",
          message =
            s"Task #${input.run.task.number} needs user input before execution.",
          task = Some(input.run.task)
        )
      )

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
    TaskLogger.progress(run =>
      s"Selected next task: #${run.task.number} - ${run.task.title}"
    )

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
        userAnswer <- GitHub.userAnswer(run.context.root, run.task, progress)
        hasRealQuestion <- GitHub.hasQuestionComment(run.context.root, run.task)
        evaluation <- existingEvaluation(run.task, hasRealQuestion)
          .filter(_ => userAnswer.isEmpty)
          .fold(
            progress(
              s"Evaluating task #${run.task.number} with ${evaluatorRunner.display}..."
            ) >> AgentExecutor[F]
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
          )(cached =>
            progress(
              s"Reusing existing evaluation for #${run.task.number}."
            ).as(cached)
          )
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
        newMetadata = TaskMetadata
          .parse(cleanBody)
          .copy(
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
            TaskMetadataStore
              .commentBased[F]
              .write(
                run.context.root,
                run.task.number,
                newMetadata,
                progress
              )
          else Sync[F].unit
        result <- {
          verifiedEvaluation.questions.filter(_.trim.nonEmpty) match
            case Some(questions) =>
              waitForUserInput[F](task.copy(run = updatedRun), questions.trim)
            case None if verifiedEvaluation.execution === "split" =>
              Right(Left(SplitTask(updatedRun))).pure[F]
            case None =>
              Right(Right(task.copy(run = updatedRun))).pure[F]
        }
      yield result
    }

  private def waitForUserInput[
      F[_]: Sync
  ](
      task: TaskWithPrompt,
      questions: String
  ): F[Either[NeedsUserInput, Either[SplitTask, TaskWithPrompt]]] =
    val run = task.run
    for
      _ <- GitHub.commentNeedsUserInput(
        run.context.root,
        run.task,
        questions,
        progress
      )
      _ <- notifyUserInputRequired[F](run.task)
      answer <- awaitUserAnswer[F](run.context.root, run.task)
      result <-
        answer match
          case Some(_) =>
            progress(
              s"Continuing task #${run.task.number} after receiving user input..."
            ) *> evaluateTask[F].run(task)
          case None =>
            Left(NeedsUserInput(run, questions)).pure[F]
    yield result

  private def awaitUserAnswer[F[_]: Sync](
      root: os.Path,
      task: Issue
  ): F[Option[String]] =
    def loop(deadlineMillis: Long): F[Option[String]] =
      for
        answer <- GitHub.userAnswer(root, task, progress)
        result <-
          answer match
            case some @ Some(_) => some.pure[F]
            case None =>
              Sync[F].blocking(System.currentTimeMillis()).flatMap { now =>
                if now >= deadlineMillis then
                  progress(
                    s"No user answer received for task #${task.number} within ${UserInputWaitMillis / 60000} minutes."
                  ).as(None)
                else
                  progress(
                    s"Waiting for a user answer on task #${task.number}..."
                  ) *>
                    Sync[F].blocking(Thread.sleep(UserInputPollMillis)) *>
                    loop(deadlineMillis)
              }
      yield result

    for
      _ <- progress(
        s"Awaiting user answer on task #${task.number} for up to ${UserInputWaitMillis / 60000} minutes..."
      )
      started <- Sync[F].blocking(System.currentTimeMillis())
      answer <- loop(started + UserInputWaitMillis)
    yield answer

  private def notifyUserInputRequired[F[_]: Sync](task: Issue): F[Unit] =
    progress(
      s"User input required for task #${task.number}; notification sound requested."
    ) *> Sync[F].blocking {
      if UserInputSoundEnabled then
        print("\u0007")
        System.out.flush()
        Try {
          val sound = os.Path("/System/Library/Sounds/Glass.aiff")
          if os.exists(sound) then
            os.proc("afplay", sound.toString)
              .call(stdout = os.Pipe, stderr = os.Pipe, check = false)
          else
            os.proc("osascript", "-e", "beep 2")
              .call(stdout = os.Pipe, stderr = os.Pipe, check = false)
        }.toOption
    }

  private def worktreeResource[F[_]: Sync](
      task: TaskWithPrompt
  ): Resource[F, TaskWithPrompt] =
    val run = task.run
    Resource.makeCase {
      for
        _ <- TaskLogger.trace[F](
          s"enter acquireWorktree input=${summarize(task)}"
        )
        _ <- Git[F].acquireWorktree(
          run.context.root,
          run.worktreePath,
          run.branchName,
          run.baseBranch,
          progress
        )
        _ <- TaskLogger.trace[F](
          s"exit acquireWorktree output=${summarize(task)}"
        )
      yield task
    } { (acquiredTask, exitCase) =>
      val acquiredRun = acquiredTask.run
      exitCase match
        case Resource.ExitCase.Succeeded =>
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
        case Resource.ExitCase.Errored(error) =>
          progress(
            s"Task #${acquiredRun.task.number} failed (${error.getClass.getSimpleName}: ${error.getMessage}). " +
              s"Leaving worktree at ${acquiredRun.worktreePath} in place for inspection/recovery instead of deleting it."
          ) *>
            TaskLogger.trace[F](
              s"skip releaseWorktree (errored) output=${summarize(acquiredTask)}"
            )
        case Resource.ExitCase.Canceled =>
          progress(
            s"Task #${acquiredRun.task.number} canceled. Leaving worktree at ${acquiredRun.worktreePath} in place."
          ) *>
            TaskLogger.trace[F](
              s"skip releaseWorktree (canceled) output=${summarize(acquiredTask)}"
            )
    }

  private def attemptWithInput[F[_]: Sync, A, B](
      arrow: -->[F, A, B]
  ): -->[F, A, Either[(A, Throwable), B]] =
    (Kleisli.ask[F, A] &&& arrow.attempt).map {
      case (_, Right(out)) => Right(out)
      case (a, Left(err))  => Left((a, err))
    }

  private def runExecutor[F[_]: Sync]: -->[F, TaskWithPrompt, TaskWithOutput] =
    attemptWithInput(runTaskWithRunner[F]) >>>
      (retryWithFallback[F] ||| Kleisli.ask[F, TaskWithOutput])

  private def progressK[F[_]: Sync, A]: -->[F, (String, A), A] =
    Kleisli { case (msg, a) => progress(msg).as(a) }

  private def raiseK[F[_]: Sync, A]: -->[F, Throwable, A] =
    Kleisli(Sync[F].raiseError)

  private def retryWithFallback[F[_]: Sync]
      : -->[F, (TaskWithPrompt, Throwable), TaskWithOutput] =
    Kleisli
      .fromFunction[F, (TaskWithPrompt, Throwable)] { case (task, error) =>
        task.run.context.agentInventory
          .nextStrongerImplementor(task.run.runner) match
          case Some(fallbackRunner) =>
            val fallbackTask =
              task.copy(run = task.run.copy(runner = fallbackRunner))
            val msg =
              s"Runner ${task.run.runner.display} failed after retries: ${error.getMessage}. Retrying task #${task.run.task.number} with stronger fallback ${fallbackRunner.display}..."
            Right((msg, fallbackTask))
          case None =>
            val msg =
              s"Runner ${task.run.runner.display} failed after retries and no stronger fallback runner is available."
            Left((msg, error))
      } >>> ((progressK[F, Throwable] >>> raiseK[F, TaskWithOutput])
      ||| (progressK[F, TaskWithPrompt] >>> runTaskWithRunner[F]))

  private def runTaskWithRunner[F[_]: Sync]
      : -->[F, TaskWithPrompt, TaskWithOutput] =
    Kleisli { task =>
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
    }

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
      task.pure[F]
    }

  private def runProjectValidation[F[_]: Sync]
      : -->[F, TaskWithOutput, TaskWithOutput] =
    Kleisli { task =>
      Git[F]
        .runProjectValidation(task.run.worktreePath, progress)
        .as(task)
    }

  private def changedPlan[F[_]: Sync]
      : -->[F, TaskWithOutput, Either[ChangedTask, UnchangedTask]] =
    Kleisli { task =>
      for
        filesChanged <- Git[F].filesChanged(task.run.worktreePath)
        hasPublishableCommits <- Git[F].hasPublishableCommits(
          task.run.worktreePath,
          task.run.branchName,
          task.run.baseBranch
        )
        hasOpenPullRequest <- GitHub.hasOpenPullRequestForBranch(
          task.run.worktreePath,
          task.run.branchName
        )
      yield
        Either.cond(!(filesChanged || hasPublishableCommits || hasOpenPullRequest), UnchangedTask(task), ChangedTask(task))
    }

  private def commitChangedTask[
      F[_]: Sync
  ]: -->[F, ChangedTask, TaskWithOutput] =
    val toPublishRequest = Kleisli[F, ChangedTask, PublishRequest] { changed =>
      val run = changed.run.run
      PublishRequest(run.context.root, run.worktreePath, run.branchName, run.baseBranch, run.task, extractAgentFinalization(changed.run.output), run.runner).pure[F]
    }

    val handleError = Kleisli[F, (ChangedTask, Throwable), TaskWithOutput] {
      case (changed, error) =>
        val run = changed.run.run
        GitHub.commentTaskFailure(
          run.context.root,
          run.task,
          error.getMessage,
          progress
        ) *> Sync[F].raiseError(error)
    }

    val publicationArrows = PublicationArrows[Flow[F]](
      publicationPlan = publicationPlan[F],
      prepareChangedPublication = prepareChangedPublication[F],
      prepareExistingPublication = prepareExistingPublication[F],
      publishTransportPlan = publishTransportPlan[F],
      publishRemote = publishRemote[F],
      publishLocal = publishLocal[F]
    )

    val publishChanges =
      toPublishRequest >>> publicationArrows.publishChanges

    attemptPreservingInput(publishChanges) >>> (handleError ||| Kleisli.ask.map(
      _._1.run
    ))

  private def reportUnchangedTask[
      F[_]: Sync
  ]: -->[F, UnchangedTask, TaskWithOutput] =
    TaskLogger.progress[F, UnchangedTask](_ => "No files changed.").map(_.run)

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

  private def closeTask[F[_]: Sync]: -->[F, TaskWithOutput, TaskRun] =
    Kleisli { task =>
      val run = task.run
      val conclusion = extractPrefixedLine(task.output, "Conclusion")
      for
        _ <- progress(s"Closing task #${run.task.number} with comment...")
        _ <- GitHub.commentConclusion(
          run.context.root,
          run.task,
          run.runner,
          conclusion,
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

  // Completes a task whose implementer already ran in a prior, interrupted
  // invocation and left an open Pull Request behind: verify/merge that PR,
  // then close the task the same way a fresh run's closeTask would, and
  // sweep up any leftover local worktree/branch (usually already gone).
  private def resumeExistingPullRequest[F[_]: Sync]
      : -->[F, TaskRun, RunSummary] =
    val resumeAndClose: -->[F, TaskRun, RunSummary] =
      Kleisli[F, TaskRun, TaskWithOutput] { run =>
        for
          _ <- progress(
            s"Task #${run.task.number} already has an open Pull Request for ${run.branchName}; resuming to verify and merge instead of re-implementing..."
          )
          _ <- GitHub.resumeOpenPullRequest(
            run.context.root,
            run.branchName,
            progress
          )
        yield TaskWithOutput(run, output = "")
      } >>> closeTask[F] >>> Kleisli[F, TaskRun, RunSummary] { completedRun =>
        for _ <- Git[F].cleanupWorktree(
            completedRun.context.root,
            completedRun.worktreePath,
            completedRun.branchName,
            progress
          )
        yield RunSummary(
          status = "completed",
          message =
            s"Task #${completedRun.task.number} completed successfully (resumed existing Pull Request).",
          task = Some(completedRun.task)
        )
      }

    val partition
        : -->[F, (TaskRun, Throwable), Either[TaskRun, (TaskRun, Throwable)]] =
      Kleisli.fromFunction {
        case (run, _: GitHub.NoOpenPullRequestToResumeException) => Left(run)
        case (run, error) => Right((run, error))
      }

    val localExecutePreparedTask: -->[F, TaskWithPrompt, RunSummary] =
      markInProgress[F] >>> runPreparedTask[F] >>> completedTaskSummary[F]

    val localTaskExecution: -->[F, TaskRun, RunSummary] =
      announceTask[F] >>>
        fetchTaskContext[F] >>>
        evaluateTask[F] >>>
        (needsUserInputSummary[F] ||| (splitTaskSummary[
          F
        ] ||| localExecutePreparedTask))

    val handleNoPr =
      TaskLogger.progress[F, TaskRun](run =>
        s"No open Pull Request remains for ${run.branchName}; creating a new run instead of resuming."
      ) >>> localTaskExecution

    val handleOtherError = Kleisli[F, (TaskRun, Throwable), RunSummary] {
      case (run, error) =>
        GitHub.commentTaskFailure(
          run.context.root,
          run.task,
          error.getMessage,
          progress
        ) *> Sync[F].raiseError(error)
    }

    val handleError = partition >>> (handleNoPr ||| handleOtherError)

    attemptPreservingInput(resumeAndClose) >>> (handleError ||| Kleisli.ask.map(
      _._2
    ))

  private def traced[F[_]: Sync, A, B](
      name: String,
      flow: -->[F, A, B]
  ): -->[F, A, B] =
    val logEnter =
      TaskLogger.trace[F, A](input => s"enter $name input=${summarize(input)}")
    val logExit =
      TaskLogger.trace[F, B](output =>
        s"exit $name output=${summarize(output)}"
      )
    val logError = TaskLogger.trace[F, Throwable](error =>
      s"fail $name error=${error.getClass.getSimpleName}: ${error.getMessage}"
    ) >>> Kleisli(Sync[F].raiseError[B])

    (logEnter >>> flow).attempt >>> (logError ||| logExit)

  private def arrowLogger[F[_]: Sync]: ArrowLogger[Flow[F]] =
    new ArrowLogger[Flow[F]]:
      def apply[A, B](name: String, flow: -->[F, A, B]): -->[F, A, B] =
        traced(name, flow)

  private def summarize(value: Any): String =
    value match
      case null => "null"
      case AppInput(root, taskNumber, recursive) =>
        s"AppInput(root=$root,task=${taskNumber.fold("auto")(_.toString)},recursive=$recursive)"
      case RunContext(root, agentInventory, taskNumber, recursive) =>
        s"RunContext(root=$root,task=${taskNumber.fold("auto")(_.toString)},recursive=$recursive,availableAgents=${agentInventory.availableTools.size})"
      case Issue(number, title, body, state, _) =>
        s"Issue(#$number,title=${quote(title)},state=$state,bodyChars=${body.length})"
      case TaskRunner(agent, model, effort, version) =>
        s"TaskRunner(agent=$agent,model=${model.getOrElse("")},effort=${effort
            .getOrElse("")},version=${version.getOrElse("")})"
      case RunnableTask(_, issue, runner, resumePullRequest) =>
        s"RunnableTask(issue=#${issue.number},runner=${runner.display},resumePullRequest=$resumePullRequest)"
      case TaskSelection(_, candidates) =>
        s"TaskSelection(candidates=${candidates.map(t => s"#${t.issue.number}").mkString(",")})"
      case NoTask(_) =>
        "NoTask"
      case TaskRun(_, task, runner, worktreePath, branchName, baseBranch) =>
        s"TaskRun(issue=#${task.number},runner=${runner.display},worktree=$worktreePath,branch=$branchName,base=${baseBranch
            .getOrElse("default")})"
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
      case AgentFinalization(commitTitle, pullRequestBody) =>
        s"AgentFinalization(hasCommitTitle=${commitTitle.nonEmpty},hasPullRequestBody=${pullRequestBody.nonEmpty})"
      case PublishRequest(
            _,
            _,
            branchName,
            baseBranch,
            task,
            finalization,
            runner
          ) =>
        s"PublishRequest(issue=#${task.number},branch=$branchName,base=${baseBranch
            .getOrElse("default")},runner=${runner.display},${summarize(finalization)})"
      case ChangedPublication(request) =>
        s"ChangedPublication(${summarize(request)})"
      case ExistingPublication(request) =>
        s"ExistingPublication(${summarize(request)})"
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
      branchName = s"task-$taskId",
      baseBranch =
        GitHub.parentIds(task).headOption.map(parentId => s"task-$parentId")
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
- Include a one-line "Conclusion:" summary for tasks that depend on this one (what changed, what's now available to build on).
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

  private def publicationPlan[F[_]: Sync]: -->[F, PublishRequest, Either[
    ChangedPublication,
    ExistingPublication
  ]] =
    Kleisli { request =>
      Git[F]
        .filesChanged(request.worktreePath)
        .map(filesChanged =>
          Either.cond(!(filesChanged), ExistingPublication(request), ChangedPublication(request))
        )
    }

  private def prepareChangedPublication[F[_]: Sync]
      : -->[F, ChangedPublication, PublishRequest] =
    TaskLogger.progress[F, ChangedPublication](_ =>
      "Files changed. Committing and merging changes..."
    ) >>> Kleisli { changed =>
      val request = changed.request
      Git[F]
        .commitAll(
          request.worktreePath,
          request.task,
          request.finalization.commitTitle
        )
        .as(request)
    }

  private def prepareExistingPublication[F[_]: Sync]
      : -->[F, ExistingPublication, PublishRequest] =
    TaskLogger
      .progress[F, ExistingPublication](_ =>
        "No file changes, publishing existing local commits..."
      )
      .map(_.request)

  private def publishTransportPlan[F[_]: Sync]
      : -->[F, PublishRequest, Either[RemotePublication, LocalPublication]] =
    Kleisli { request =>
      Git[F]
        .hasRemote(request.root)
        .map(hasRemote =>
          Either.cond(!(hasRemote), LocalPublication(request), RemotePublication(request))
        )
    }

  private def publishRemote[F[_]: Sync]: -->[F, RemotePublication, Unit] =
    Kleisli { remote =>
      val request = remote.request
      GitHub.pushCreateAndMergePr(
        request.root,
        request.worktreePath,
        request.branchName,
        request.baseBranch,
        request.task,
        request.finalization.commitTitle,
        request.finalization.pullRequestBody,
        request.runner,
        progress
      )
    }

  private def publishLocal[F[_]: Sync]: -->[F, LocalPublication, Unit] =
    Kleisli { local =>
      val request = local.request
      Git[F].mergeLocally(
        request.root,
        request.worktreePath,
        request.branchName,
        request.baseBranch,
        progress
      )
    }

  private def progress[F[_]: Sync](message: String): F[Unit] =
    TaskLogger.script(message)

  private def attemptPreservingInput[F[_]: Sync, A, B](
      k: -->[F, A, B]
  ): -->[F, A, Either[(A, Throwable), (A, B)]] =
    (Kleisli.ask[F, A] &&& k.attempt).map {
      case (a, Left(err)) => Left((a, err))
      case (a, Right(b))  => Right((a, b))
    }

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

  private def parseRecursiveFlag(args: List[String]): Boolean =
    args.contains("--recursive")

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
        case "--recursive" :: tail =>
          loop(tail, clean)
        case head :: tail =>
          loop(tail, head :: clean)

    loop(args, Nil)

  private def envLong(name: String, fallback: Long): Long =
    sys.env
      .get(name)
      .flatMap(value => Try(value.trim.toLong).toOption)
      .filter(_ > 0)
      .getOrElse(fallback)
