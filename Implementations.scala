import cats.data.Kleisli
import cats.effect.Resource
import cats.effect.kernel.Async
import cats.effect.kernel.Sync
import cats.syntax.all.*
import cats.syntax.arrow.*
import arrowstep.core.ProgramSays
import cats.arrow.Arrow

import scala.concurrent.duration.*
import scala.util.Try

/** Leaf implementations: every arrow that actually talks to git, GitHub, the filesystem or an agent process.
  *
  * Nothing here composes arrows across groups and nothing here constructs an `*Arrows` record - the shape of the
  * program lives in `BusinessLogic.scala` and its assembly in `Wiring.scala`. Leaves that need to re-enter the program
  * (claiming and running a candidate, executing inside a worktree, evaluating again after a user answers) take that
  * continuation as a parameter instead of rebuilding the program.
  *
  * A leaf that reads `route`/`classify` in its name returns an `Either` and decides nothing else: the branch it selects
  * is taken by an `ArrowChoice` in the algebra, never by an `if` around an effect here.
  */
object Impl:
  def git[F[_]: Sync] = Git.apply[F](progress)
  val evaluatorRunner: TaskRunner =
    TaskRunner(AgentBinary("claude"), Some("opus"), None, None)
  val UserInputWaitMillis =
    DeadlineMillis(
      Cli.envLong("GH_TASKS_USER_INPUT_WAIT_MINUTES", 120).minutes.toMillis
    )
  val UserInputPollMillis =
    DeadlineMillis(
      Cli.envLong("GH_TASKS_USER_INPUT_POLL_SECONDS", 30).seconds.toMillis
    )
  val UserInputSoundEnabled =
    sys.env
      .get("GH_TASKS_USER_INPUT_SOUND")
      .forall(value => !Set("0", "false", "no", "off").contains(value.toLowerCase))

  // TTL-gated, not per-task: refreshing means shelling out to codex/gemini/
  // deepseek/ccusage probes, so it only runs once per process invocation
  // (here, before AgentInventory reads the snapshot) and only when the
  // on-disk snapshot is older than the TTL. A probe failure never blocks
  // task execution - `.attempt.void` swallows it and AgentInventory just
  // reads whatever snapshot (possibly stale, possibly absent) is on disk.
  def refreshVendorBudgetsIfStale[F[_]: Sync](root: os.Path): F[Unit] =
    Sync[F]
      .blocking {
        val ttlMillis = Cli.envLong("GH_TASKS_VENDOR_BUDGET_TTL_MINUTES", 15).minutes.toMillis
        val isStale = VendorBudgets.ageMillis(root).forall(_ > ttlMillis)
        if isStale then VendorBudgets.collectAndWrite(root)
        ()
      }
      .attempt
      .void

  // model-prices.json is hand-reviewed vendor pricing (see
  // scripts/refresh-model-prices.scala), not something to auto-probe. But a
  // brand-new target repo has no copy at all, so seed it once from the
  // classpath resource bundled with this project (resources/model-prices.json,
  // packaged via `//> using resourceDir resources`) - never overwrites an
  // existing file, so a repo's own reviewed prices are left alone.
  def seedModelPricesIfMissing[F[_]: Sync](root: os.Path): F[Unit] =
    Sync[F]
      .blocking {
        val destination = root / ".gh-tasks-llm-executor" / "model-prices.json"
        if !os.exists(destination) then
          Option(getClass.getResourceAsStream("/model-prices.json")).foreach { stream =>
            try
              val bytes = stream.readAllBytes()
              os.makeDir.all(destination / os.up)
              os.write.over(destination, bytes)
            finally stream.close()
          }
      }
      .attempt
      .void

  // Same TTL-gated, once-per-invocation shape as refreshVendorBudgetsIfStale:
  // probing installed agent CLIs (claude/codex/aider --version) is cheap but
  // not free, so only do it when the on-disk snapshot is missing or older
  // than the TTL. A probe failure never blocks task execution - AgentInventory
  // just reads whatever snapshot (possibly stale, possibly absent) is on disk.
  def refreshAgentRunnersIfStale[F[_]: Sync](root: os.Path): F[Unit] =
    Sync[F]
      .blocking {
        val ttlMillis = Cli.envLong("GH_TASKS_AGENT_RUNNERS_TTL_MINUTES", 60).minutes.toMillis
        val isStale = AgentRunnersDiscovery.ageMillis(root).forall(_ > ttlMillis)
        if isStale then AgentRunnersDiscovery.collectAndWrite(root)
      }
      .attempt
      .void

  def resolveContext[F[_]: Sync]: -->[F, AppInput, RunContext] =
    Kleisli { input =>
      for
        _ <- refreshVendorBudgetsIfStale[F](input.root)
        _ <- seedModelPricesIfMissing[F](input.root)
        _ <- refreshAgentRunnersIfStale[F](input.root)
        inventory <- AgentInventory.loadF[F](input.root)
      yield RunContext(input.root, inventory, input.taskNumber, input.recursive, input.parallelExecution)
    }

  def selectTask[F[_]: Sync]: -->[F, RunContext, TaskSelection] =
    Kleisli { (context: RunContext) =>
      for
        _ <- progress("Fetching open issues from GitHub...")
        rawIssues <- GitHub.fetchIssues(context.root)
        issues <- rawIssues.traverse(effectiveIssue[F](context.root, _))
        openIssueNumbers = issues.map(_.number).toSet
        candidatesByNumber = issues.filter(task => context.taskNumber.forall(_ === task.number))
        filteredByDeps <- candidatesByNumber.filterA { task =>
          if context.taskNumber.isDefined then
            // If a specific task is targeted, do not filter out by dependencies/children.
            // We want to run it and traverse its tree recursively!
            true.pure[F]
          else if context.recursive.value then
            // --recursive walks each root's dependency tree to closure, so an
            // unresolved dependency is not a reason to exclude a root candidate
            // (unlike above, still exclude issues that are themselves still a
            // child/dependency of another open issue, to avoid entering the
            // same subtree from more than one root entry point).
            val openChildren = GitHub.hasOpenChildren(task, issues)
            Option
              .when(openChildren)("has open child tasks")
              .traverse_(why => TaskLogger.trace(s"selectTask excluding #${task.number}: $why"))
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
              .traverse_(why => TaskLogger.trace(s"selectTask excluding #${task.number}: $why"))
              .as(!excluded)
        }
        notAlreadyClaimed <- filteredByDeps.filterA { task =>
          val claimed = task.labels.exists(label => label === "status: in progress" || label === "in progress")
          Option
            .when(claimed)(
              "already labeled in progress (claimed by another run)"
            )
            .traverse_(why => TaskLogger.trace(s"selectTask excluding #${task.number}: $why"))
            .as(!claimed)
        }
        notAlreadyCompleted <- notAlreadyClaimed.filterA { task =>
          if GitHub.hasCompletedLabel(task) then
            TaskLogger
              .trace(
                s"selectTask excluding #${task.number}: already labeled completed"
              )
              .as(false)
          else
            GitHub
              .hasCompletionComment(context.root, task)
              .flatTap { completed =>
                if !completed then ().pure[F]
                else
                  TaskLogger.trace(
                    s"selectTask excluding #${task.number}: completion comment already exists"
                  )
              }
              .map(!_)
        }
        eligible <- notAlreadyCompleted.filterA { task =>
          val needsInputMetadata =
            evaluationStatus(task.body).contains("needs-input") ||
              executionStatus(task.body).contains(Execution.NeedsInput)
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
                    .userAnswer(progress)(context.root, task)
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
        runnableAfterReplaySkips <- eligible.filterA { task =>
          val alreadySplit =
            executionStatus(task.body).contains(Execution.Split) &&
              GitHub.hasOpenChildren(task, issues)
          val skipDirectReplay = alreadySplit && !context.recursive.value
          Option
            .when(skipDirectReplay)(
              "already split into open child tasks; skipping direct replay"
            )
            .traverse_(why => TaskLogger.trace(s"selectTask excluding #${task.number}: $why"))
            .as(!skipDirectReplay)
        }
        // A branch/PR from an earlier run may still be open (e.g. it was
        // interrupted before merging): re-running the implementer would
        // create a second local branch of the same name and diverge, so
        // resume that PR (verify checks, merge) instead of re-implementing.
        eligibleWithResumeFlag <- runnableAfterReplaySkips.traverse { task =>
          GitHub
            .hasOpenPullRequestForBranch(
              context.root,
              BranchName(s"task-${task.number}")
            )
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
          TaskCandidate(
            context,
            task,
            context.agentInventory
              .selectRunnerFor(
                TaskMetadata.parse(task.body.value).requiredAbilities,
                GitHub.taskRunners(task)
              )
              .getOrElse(evaluatorRunner),
            resumePullRequest = ResumePullRequest(hasOpenPr)
          )
        }
        _ <- progress(
          context.taskNumber.fold(
            s"Found ${candidates.size} runnable open tasks with preferred runner metadata."
          )(number => s"Found ${candidates.size} runnable open tasks matching #$number.")
        )
      yield TaskSelection(context, candidates)
    }

  // Merges an issue's original body with every TaskMetadata comment posted
  // for it into one synthesized view, so the existing body-based parsers
  // (evaluationStatus, taskRunners, ...) keep working unchanged while the
  // real issue body is never rewritten.
  def effectiveIssue[F[_]: Sync](
      root: os.Path,
      issue: Issue
  ): F[Issue] =
    TaskMetadataStore
      .commentBased[F](progress)
      .read(root, issue)
      .map(merged => issue.copy(body = TaskMetadata.render(merged)))

  def routeEmptySelection[F[_]: Sync]: -->[F, TaskSelection, Either[NoTask, TaskSelection]] =
    Kleisli.fromFunction { selection =>
      Either.cond(
        !(selection.candidates.isEmpty),
        selection,
        NoTask(selection.context)
      )
    }

  // Seeds the run-scoped open-issue map. Used to be inlined at the top of
  // executeSelectedCandidates, together with the Ref allocation and the
  // construction of every traversal/recursion arrow.
  def loadOpenIssues[F[_]: Sync]: -->[RunF[F], TaskSelection, TaskSelection] =
    RunEnv.read { (env, selection) =>
      GitHub
        .fetchIssues(selection.context.root)
        .flatMap(issues => env.openIssues.set(issues.map(issue => issue.number -> issue).toMap))
        .as(selection)
    }

  /** Left = run root candidates concurrently (`--parallel`). */
  def routeParallelExecution[F[_]: Sync]: -->[F, TaskSelection, Either[TaskSelection, TaskSelection]] =
    Kleisli.fromFunction { selection =>
      Either.cond(!selection.context.parallelExecution.value, selection, selection)
    }

  // One root candidate raising an uncaught error must not take down every
  // other queued candidate in the same batch. Push/prePush failures should be
  // handled by the publication repair loop first; if one still escapes, report
  // it against that candidate and keep the outer run moving.
  def recoverCandidateFailure[F[_]: Sync]: -->[F, (TaskCandidate, Throwable), RunSummary] =
    Kleisli { case (candidate, error) =>
      val message = Option(error.getMessage).getOrElse(error.toString)
      progress(
        s"Task #${candidate.issue.number.value} failed unrecoverably: $message. Skipping and continuing with remaining tasks."
      ).as(
        RunSummary(
          status = Status("error"),
          message = Message5(message),
          task = Some(candidate.issue)
        )
      )
    }

  def lastSummary[F[_]: Sync]: -->[F, List[RunSummary], RunSummary] =
    Kleisli.fromFunction { summaries =>
      summaries.lastOption.getOrElse(
        RunSummary(
          status = Status("completed"),
          message = Message5("All candidates completed."),
          task = None
        )
      )
    }

  def toProgramSays[F[_]: Sync]: -->[F, RunSummary, ProgramSays[ujson.Value]] =
    Kleisli(summary => ProgramSays.Done(summary.toJson).pure[F])

  val RecursiveRootIterationCap = 50

  /** Left = walk this root repeatedly until it closes (`--recursive`). */
  def routeRecursiveMode[F[_]: Sync]: -->[F, TaskCandidate, Either[TaskCandidate, TaskCandidate]] =
    Kleisli.fromFunction { candidate =>
      Either.cond(!candidate.context.recursive.value, candidate, candidate)
    }

  /** One dependency-first pass over a root candidate's tree, against the latest known state of that issue.
    */
  def runOnce[F[_]: Sync](
      executeRecursive: -->[RunF[F], TaskNode, RunSummary]
  ): -->[RunF[F], TaskCandidate, RunSummary] =
    Kleisli { candidate =>
      Kleisli { (env: RunEnv[F]) =>
        env.openIssues.get.flatMap { currentMap =>
          currentMap.get(candidate.issue.number) match
            case Some(latestIssue) =>
              executeRecursive.run(TaskNode(candidate.context, latestIssue)).run(env)
            case None =>
              RunSummary(
                status = Status("completed"),
                message = Message5(
                  s"Task #${candidate.issue.number} is already completed."
                ),
                task = Some(candidate.issue)
              ).pure[F]
        }
      }
    }

  // Refetching between passes is the point of the repeat: a split mid-tree
  // creates sub-issues the previous snapshot never had, so without this a
  // single pass short-circuits without ever running them.
  def refreshRoot[F[_]: Sync]: -->[RunF[F], RootWalk, Either[RunSummary, RootWalk]] =
    RunEnv.read { (env, walk) =>
      for
        freshIssues <- GitHub.fetchIssues(walk.candidate.context.root)
        freshMap = freshIssues.map(issue => issue.number -> issue).toMap
        _ <- env.openIssues.set(freshMap)
      yield freshMap.get(walk.candidate.issue.number) match
        case None =>
          Left(
            RunSummary(
              status = Status("completed"),
              message = Message5(s"Task #${walk.candidate.issue.number} is already completed."),
              task = None
            )
          )
        case Some(rootIssue) =>
          Right(walk.copy(candidate = walk.candidate.copy(issue = rootIssue)))
    }

  def runRootOnce[-->[_, _]: Arrow](
      executeRecursive: TaskNode --> RunSummary
  ): RootWalk --> (RootWalk, RunSummary) = {
    val A = Arrow[-->]
    A.id[RootWalk] &&& (
      A.id[RootWalk].map(walk => TaskNode(walk.candidate.context, walk.candidate.issue)) >>> executeRecursive
    )
  }

  def routeContinuation[F[_]: Sync]: -->[F, (RootWalk, RunSummary), Either[RunSummary, RootWalk]] =
    Kleisli { case (walk, summary) =>
      val rootNumber = walk.candidate.issue.number
      if summary.status.value === "needs-input" then Left(summary).pure[F]
      else if walk.previous.contains(summary) then
        progress(
          s"Task #$rootNumber made no further progress under --recursive; stopping."
        ).as(Left(summary))
      else if walk.iteration >= RecursiveRootIterationCap then
        progress(
          s"Task #$rootNumber hit the --recursive iteration cap ($RecursiveRootIterationCap); stopping."
        ).as(Left(summary))
      else Right(walk.copy(iteration = walk.iteration + 1, previous = Some(summary))).pure[F]
    }

  def checkIfCompleted[F[_]: Sync]: -->[F, TaskNode, Either[RunSummary, TaskNode]] =
    Kleisli { node =>
      if node.issue.state.value.toLowerCase == "closed" then
        Left(
          RunSummary(
            status = Status("completed"),
            message = Message5(s"Task #${node.issue.number} is already completed."),
            task = Some(node.issue)
          )
        ).pure[F]
      else Right(node).pure[F]
    }

  // Dependencies first, then (for an already split task) its open children,
  // deduplicated and in issue order.
  def collectPendingDependencies[F[_]: Sync]: -->[RunF[F], TaskNode, DependencyPlan] =
    RunEnv.read { (env, node) =>
      val issue = node.issue
      val depIds = GitHub.getDependencies(issue.body).distinct
      env.openIssues.get.map { issuesMap =>
        val openDeps = depIds.flatMap(issuesMap.get)
        val openChildren =
          if executionStatus(issue.body).contains(Execution.Split) then
            issuesMap.values.toList
              .filter(child => GitHub.parentIds(child).contains(issue.number))
              .sortBy(_.number.value)
          else Nil
        DependencyPlan(
          node = node,
          pending = (openDeps ++ openChildren)
            .distinctBy(_.number)
            .map(TaskNode(node.context, _)),
          hasOpenChildren = openChildren.nonEmpty
        )
      }
    }

  // A dependency that did not complete stops the walk (Left); one that did is
  // dropped from the open-issue map so later passes skip it.
  def recordDependencyOutcome[F[_]: Sync]: -->[RunF[F], (TaskNode, RunSummary), Either[RunSummary, Unit]] =
    RunEnv.read { case (env, (node, summary)) =>
      val completed = summary.status.value == "completed"
      env.openIssues
        .update(map => if completed then map.removed(node.issue.number) else map)
        .as(Either.cond(completed, (), summary))
    }

  // All dependencies closed, but this task was itself already split: its
  // children just ran, so let the next pass replay the parent rather than
  // implementing it now.
  def routeDependencyOutcome[F[_]: Sync]
      : -->[F, (DependencyPlan, Either[RunSummary, Unit]), Either[RunSummary, TaskNode]] =
    Kleisli.fromFunction {
      case (_, Left(summary)) => Left(summary)
      case (plan, Right(())) =>
        if plan.hasOpenChildren then
          Left(
            RunSummary(
              status = Status("split"),
              message = Message5(
                s"Task #${plan.node.issue.number} is already split; processed open child tasks before parent replay."
              ),
              task = Some(plan.node.issue)
            )
          )
        else Right(plan.node)
    }

  // Takes the candidate pipeline as a parameter rather than rebuilding the
  // whole program to call into it; Wiring supplies the deferred, already
  // instrumented arrow.
  def claimAndRun[F[_]: Async](
      executeCandidate: -->[F, TaskCandidate, RunSummary]
  ): -->[F, TaskNode, RunSummary] =
    Kleisli { node =>
      val context = node.context
      val issue = node.issue
      val runner = context.agentInventory
        .selectRunnerFor(
          TaskMetadata.parse(issue.body.value).requiredAbilities,
          GitHub.taskRunners(issue)
        )
        .getOrElse(evaluatorRunner)
      val runnable = TaskCandidate(context, issue, runner)

      IssueClaim
        .acquire[F](context.root, issue.number, progress)
        .use { _ =>
          executeCandidate
            .run(runnable)
            .onError { case _ =>
              GitHub.clearInProgressStatus(progress)(Some(runner))((context.root, issue.number))
            }
        }
        .recoverWith { case _: IssueAlreadyClaimedException =>
          progress(
            s"Task #${issue.number} is claimed by another process. Waiting for completion..."
          ) *>
            pollGitHubForCompletion[F]((context.root, issue.number)).map { finalIssue =>
              RunSummary(
                status = Status("completed"),
                message = Message5(
                  s"Task #${issue.number} was completed by another process."
                ),
                task = Some(finalIssue)
              )
            }
        }
    }

  def pollGitHubForCompletion[F[_]: Sync]: Kleisli[F, (os.Path, TaskNumber), Issue] =
    Kleisli.apply { case (root, taskId) =>
      val pollInterval = 30.seconds
      def loop: F[Issue] =
        GitHub.fetchIssues(root).flatMap { issues =>
          issues.find(_.number === taskId) match
            case Some(issue) if issue.state.value.toLowerCase == "closed" =>
              issue.pure[F]
            case None =>
              Issue(
                taskId,
                IssueTitle(s"Task #$taskId"),
                IssueBody(""),
                State("closed")
              ).pure[F]
            case _ =>
              Sync[F].blocking(Thread.sleep(pollInterval.toMillis)) *> loop
        }
      loop
    }

  def noTaskSummary[F[_]: Sync]: -->[F, NoTask, RunSummary] =
    Kleisli { noTask =>
      val context = noTask.context
      val message = context.taskNumber.fold(
        "No tasks found without unresolved dependencies, open child tasks, or another process already claiming them."
      )(number =>
        s"No runnable open task found for #$number. It may be closed, blocked by dependencies or open child tasks, marked needs-input, or already claimed by another process."
      )
      RunSummary(
        status = Status("no-task"),
        message = Message5(message),
        task = None
      ).pure[F]
    }

  def routeResumeOrRun[F[_]: Sync]: -->[F, TaskCandidate, Either[ClaimedTask, ClaimedTask]] =
    Kleisli { task =>
      val run = taskRun(task.context, task.issue, task.runner)
      val shouldResume =
        if task.resumePullRequest.value then
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

      shouldResume.map(resumePullRequest => Either.cond(resumePullRequest, run, run))
    }

  def completedTaskSummary[F[_]: Sync]: -->[F, ClaimedTask, RunSummary] =
    Kleisli { run =>
      RunSummary(
        status = Status("completed"),
        message = Message5(s"Task #${run.task.number} completed successfully."),
        task = Some(run.task)
      ).pure[F]
    }

  // The execution pipeline is a parameter, not something rebuilt here: this
  // leaf only owns the worktree bracket around it.
  def acquireWorktreeAndExecute[F[_]: Async](
      executeInWorktree: -->[F, PreparedTask, ClaimedTask]
  ): -->[F, PreparedTask, ClaimedTask] =
    Kleisli { task =>
      worktreeResource[F](task).use { acquiredTask =>
        executeInWorktree
          .run(acquiredTask)
          .onError { case _ =>
            git.preserveUnpushedCommits(
              acquiredTask.claimedTask.worktreePath,
              acquiredTask.claimedTask.branchName,
              acquiredTask.claimedTask.baseBranch
            )
          }
      }
    }

  def needsUserInputSummary[
      F[_]: Sync
  ]: -->[F, NeedsUserInput, RunSummary] =
    TaskLogger
      .progress[F, NeedsUserInput](input =>
        s"Task #${input.run.task.number} still needs user input; stopping this iteration."
      )
      .map(input =>
        RunSummary(
          status = Status("needs-input"),
          message = Message5(
            s"Task #${input.run.task.number} needs user input before execution."
          ),
          task = Some(input.run.task)
        )
      )

  def splitTaskSummary[F[_]: Sync]: -->[F, SplitTask, RunSummary] =
    Kleisli { split =>
      val summary = RunSummary(
        status = Status("split"),
        message = Message5(
          s"Task #${split.run.task.number} was evaluated for splitting and will not be implemented directly."
        ),
        task = Some(split.run.task)
      )
      if split.replayed then summary.pure[F]
      else
        GitHub
          .commentSplitEvaluation(progress)(
            split.run.context.root,
            split.run.task
          )
          .as(summary)
    }

  def announceTask[F[_]: Sync]: -->[F, ClaimedTask, ClaimedTask] =
    TaskLogger.progress(run => s"Selected next task: #${run.task.number} - ${run.task.title}")

  def markTaskInProgress[
      F[_]: Sync
  ]: -->[F, PreparedTask, PreparedTask] =
    Kleisli { task =>
      val run = task.claimedTask
      GitHub
        .setIssueStatusWithRunner(progress)(Some(run.runner))(
          run.context.root,
          run.task.number,
          "in progress"
        )
        .as(task)
    }

  def fetchTaskContext[
      F[_]: Sync
  ]: -->[F, ClaimedTask, PreparedTask] =
    (Kleisli.ask[F, ClaimedTask] &&& Kleisli { (run: ClaimedTask) =>
      GitHub.dependencyConclusion(progress)(run.context.root, run.task)
    } &&& Kleisli { (run: ClaimedTask) =>
      GitHub.replayContext(progress)(run.context.root, run.task)
    }).map({ case ((run, dependencyConclusion), replayContext) =>
      PreparedTask(run, dependencyConclusion, replayContext)
    })

  // Re-evaluating after an answer arrives is the whole point of waiting, so
  // this leaf closes a cycle: evaluateTask -> waitForUserInput -> evaluateTask.
  // It takes the continuation rather than calling the evaluator it belongs to.
  def waitForUserInput[F[_]: Sync](
      evaluateTask: -->[F, PreparedTask, EvaluationArrows.Result]
  ): Kleisli[
    F,
    (PreparedTask, String),
    Either[NeedsUserInput, Either[SplitTask, PreparedTask]]
  ] =
    Kleisli.apply { case (task, questions) =>
      val run = task.claimedTask
      for
        _ <- GitHub.commentNeedsUserInput(progress)(
          run.context.root,
          run.task,
          questions
        )
        _ <- notifyUserInputRequired[F](run.task)
        answer <- awaitUserAnswer[F]((run.context.root, run.task))
        result <-
          answer match
            case Some(_) =>
              progress(
                s"Continuing task #${run.task.number} after receiving user input..."
              ) *> evaluateTask.run(task)
            case None =>
              Left(NeedsUserInput(run, Questions(questions))).pure[F]
      yield result
    }

  def awaitUserAnswer[F[_]: Sync]: Kleisli[F, (os.Path, Issue), Option[String]] =
    Kleisli.apply { case (root, task) =>
      def loop(deadlineMillis: DeadlineMillis): F[Option[String]] =
        for
          answer <- GitHub.userAnswer(progress)(root, task)
          result <-
            answer match
              case some @ Some(_) => some.pure[F]
              case None =>
                Sync[F].blocking(System.currentTimeMillis()).flatMap { now =>
                  if now >= deadlineMillis.value then
                    progress(
                      s"No user answer received for task #${task.number} within ${UserInputWaitMillis.value / 60000} minutes."
                    ).as(None)
                  else
                    progress(
                      s"Waiting for a user answer on task #${task.number}..."
                    ) *>
                      Sync[F]
                        .blocking(Thread.sleep(UserInputPollMillis.value)) *>
                      loop(deadlineMillis)
                }
        yield result

      for
        _ <- progress(
          s"Awaiting user answer on task #${task.number} for up to ${UserInputWaitMillis.value / 60000} minutes..."
        )
        started <- Sync[F].blocking(System.currentTimeMillis())
        answer <- loop(DeadlineMillis(started) + UserInputWaitMillis)
      yield answer
    }

  def notifyUserInputRequired[F[_]: Sync]: Kleisli[F, Issue, Unit] =
    Kleisli.apply { task =>
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
    }

  def worktreeResource[F[_]: Sync](
      task: PreparedTask
  ): Resource[F, PreparedTask] =
    val run = task.claimedTask
    Resource.makeCase {
      for
        _ <- TaskLogger.trace[F](
          s"enter acquireWorktree input=${Wiring.summarize(task)}"
        )
        _ <- git[F].acquireWorktree(
          run.context.root,
          run.worktreePath,
          run.branchName,
          run.baseBranch
        )
        _ <- TaskLogger.trace[F](
          s"exit acquireWorktree output=${Wiring.summarize(task)}"
        )
      yield task
    } { (acquiredTask, exitCase) =>
      val acquiredRun = acquiredTask.claimedTask
      exitCase match
        case Resource.ExitCase.Succeeded =>
          TaskLogger.trace[F](
            s"enter releaseWorktree input=${Wiring.summarize(acquiredTask)}"
          ) *>
            git[F]
              .releaseWorktree(
                acquiredRun.context.root,
                acquiredRun.worktreePath,
                acquiredRun.branchName
              )
              .handleErrorWith(error =>
                TaskLogger.trace[F](
                  s"fail releaseWorktree error=${error.getClass.getSimpleName}: ${error.getMessage}"
                )
              ) *>
            TaskLogger.trace[F](
              s"exit releaseWorktree output=${Wiring.summarize(acquiredTask)}"
            )
        case Resource.ExitCase.Errored(error) =>
          progress(
            s"Task #${acquiredRun.task.number} failed (${error.getClass.getSimpleName}: ${error.getMessage}). " +
              s"Leaving worktree at ${acquiredRun.worktreePath} in place for inspection/recovery instead of deleting it."
          ) *>
            TaskLogger.trace[F](
              s"skip releaseWorktree (errored) output=${Wiring.summarize(acquiredTask)}"
            )
        case Resource.ExitCase.Canceled =>
          progress(
            s"Task #${acquiredRun.task.number} canceled. Leaving worktree at ${acquiredRun.worktreePath} in place."
          ) *>
            TaskLogger.trace[F](
              s"skip releaseWorktree (canceled) output=${Wiring.summarize(acquiredTask)}"
            )
    }

  // Heavy guarantee: the implementer LLM is never re-invoked on work it
  // already finished. If a prior run left the durable "implemented" mark AND
  // that work is still reachable (open PR or surviving origin branch), skip
  // the agent call entirely and let the downstream publish/close pipeline
  // finish from the existing commits. The reachability check keeps this safe:
  // when the mark is stale (local-only branch that acquireWorktree wiped),
  // there is nothing to resume, so re-running the implementer is correct.
  def routeAlreadyImplemented[F[_]: Sync]: -->[F, PreparedTask, Either[ExecutedTask, PreparedTask]] =
    Kleisli { task =>
      alreadyImplemented[F](task).flatMap {
        case Some(branch) =>
          progress(
            s"Task #${task.claimedTask.task.number} already implemented on branch $branch " +
              s"(durable mark + reachable work); skipping implementer ${task.claimedTask.runner.display}."
          ).as(Left(ExecutedTask(task.claimedTask, AgentOutput(""))))
        case None => Right(task).pure[F]
      }
    }

  def alreadyImplemented[F[_]: Sync](task: PreparedTask): F[Option[String]] =
    val run = task.claimedTask
    TaskMetadataStore
      .commentBased[F](progress)
      .read(run.context.root, run.task)
      .flatMap { metadata =>
        metadata.implemented match
          case None => none[String].pure[F]
          case Some(branch) =>
            for
              hasPr <- GitHub.hasOpenPullRequestForBranch(run.context.root, run.branchName)
              reachable <-
                if hasPr then true.pure[F]
                else git[F].hasOriginBranch.run((run.context.root, run.branchName))
            yield Option.when(reachable)(branch)
      }

  // Records the durable "implemented" mark once the agent output has been
  // committed and published. Idempotent: re-runs that resumed via the
  // already-implemented short-circuit skip re-writing an identical mark.
  def markTaskImplemented[F[_]: Sync]: -->[F, ExecutedTask, ExecutedTask] =
    Kleisli { task =>
      val run = task.run
      val mark = run.branchName.value
      val store = TaskMetadataStore.commentBased[F](progress)
      store.read(run.context.root, run.task).flatMap { existing =>
        if existing.implemented.contains(mark) then task.pure[F]
        else
          progress(s"Marking task #${run.task.number} implemented on branch $mark.") *>
            store
              .write(
                run.context.root,
                run.task.number,
                TaskMetadata(implemented = Some(mark))
              )
              .as(task)
      }
    }

  def raiseK[F[_]: Sync, A]: -->[F, Throwable, A] =
    Kleisli(Sync[F].raiseError)

  // Right = re-aim the same task at the next stronger implementer for one
  // more attempt; Left = nothing stronger is configured, so the failure stands.
  def routeRunnerFallback[F[_]: Sync]: -->[F, (PreparedTask, Throwable), Either[Throwable, PreparedTask]] =
    Kleisli { case (task, error) =>
      task.claimedTask.context.agentInventory
        .nextStrongerImplementor(task.claimedTask.runner) match
        case Some(fallbackRunner) =>
          progress(
            s"Runner ${task.claimedTask.runner.display} failed after retries: ${error.getMessage}. Retrying task #${task.claimedTask.task.number} with stronger fallback ${fallbackRunner.display}..."
          ).as(
            Right(task.copy(claimedTask = task.claimedTask.copy(runner = fallbackRunner)))
          )
        case None =>
          progress(
            s"Runner ${task.claimedTask.runner.display} failed after retries and no stronger fallback runner is available."
          ).as(Left(error))
    }

  def runTaskWithRunner[F[_]: Sync]: -->[F, PreparedTask, ExecutedTask] =
    Kleisli { task =>
      val run = task.claimedTask
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
          ImplementerAllowedTools,
          taskNumber = Some(run.task.number),
          metricsRoot = Some(run.context.root),
          metricsScope = "implement"
        )
        _ <- Sync[F]
          .raiseError(
            RuntimeException(
              s"Agent ${run.runner.display} reported it could not proceed (permission/tool wall). Output: ${output.value.trim}"
            )
          )
          .whenA(looksBlocked(output))
      yield ExecutedTask(run, output)
    }

  // Exit code 0 only means the process returned; a stuck agent that gave up
  // after every tool call was denied also exits 0 with prose explaining why,
  // and with no files changed that reads as a legitimate no-op success
  // (see reportUnchangedTask/closeTaskIssue). Catch that specific failure shape
  // here so it surfaces as a real error (and triggers runAgent's
  // stronger-runner fallback) instead of silently closing the task.
  val BlockedSignals = List(
    "tool calls (and file-creating bash) are being denied",
    "being denied",
    "need approval i'm not getting",
    "hit wall:",
    "permission denied and cannot continue"
  )

  def looksBlocked(output: Output): Boolean =
    val lower = output.value.toLowerCase
    BlockedSignals.exists(lower.contains)

  def recordAgentOutput[F[_]: Sync]: -->[F, ExecutedTask, ExecutedTask] =
    Kleisli.ask[F, ExecutedTask]

  def runProjectValidation[F[_]: Sync]: -->[F, ExecutedTask, ExecutedTask] =
    Kleisli.ask <* (
      Kleisli.fromFunction { (t: ExecutedTask) => t.run.worktreePath } >>>
        git[F].runProjectValidation
    )

  def classifyAgentResultForPublication[F[_]: Sync]: -->[F, ExecutedTask, Either[ChangedTask, UnchangedTask]] =
    (Kleisli.ask[F, ExecutedTask] &&& Kleisli { (task: ExecutedTask) =>
      git[F].filesChanged(task.run.worktreePath)
    } &&& Kleisli { (task: ExecutedTask) =>
      git[F].hasPublishableCommits(
        task.run.worktreePath,
        task.run.branchName,
        task.run.baseBranch
      )
    } &&& Kleisli { (task: ExecutedTask) =>
      GitHub.hasOpenPullRequestForBranch(
        task.run.worktreePath,
        task.run.branchName
      )
    }).map({
      case (
            ((task, filesChanged), hasPublishableCommits),
            hasOpenPullRequest
          ) =>
        Either.cond(
          !(filesChanged || hasPublishableCommits || hasOpenPullRequest),
          UnchangedTask(task),
          ChangedTask(task)
        )
    })

  def toPublishRequestOfChanged[F[_]: Sync]: -->[F, ChangedTask, PublishRequest] =
    Kleisli.fromFunction { changed =>
      val run = changed.run.run
      PublishRequest(
        run.context.root,
        run.worktreePath,
        run.branchName,
        run.baseBranch,
        run.task,
        extractAgentFinalization(changed.run.output),
        run.runner
      )
    }

  def reportPublicationFailure[F[_]: Sync]: -->[F, (ChangedTask, Throwable), ExecutedTask] =
    Kleisli { case (changed, error) =>
      val run = changed.run.run
      GitHub.commentTaskFailure(progress)(
        run.context.root,
        run.task,
        error.getMessage
      ) *> Sync[F].raiseError(error)
    }

  def reportUnchangedTask[
      F[_]: Sync
  ]: -->[F, UnchangedTask, ExecutedTask] =
    TaskLogger.progress[F, UnchangedTask](_ => "No files changed.").map(_.run)

  def verifyRelatedPullRequestCi[F[_]: Sync]: -->[F, ExecutedTask, ExecutedTask] =
    Kleisli { task =>
      GitHub
        .verifyRelatedPullRequestCiForTask(progress)(
          task.run.context.root,
          task.run.task,
          task.run.branchName
        )
        .handleErrorWith { error =>
          GitHub
            .commentTaskFailure(progress)(
              task.run.context.root,
              task.run.task,
              error.getMessage
            ) *> Sync[F].raiseError(error)
        }
        .as(task)
    }

  def closeTaskIssue[F[_]: Sync]: -->[F, ExecutedTask, ClaimedTask] =
    Kleisli { task =>
      val run = task.run
      val conclusion = extractPrefixedLine(task.output, "Conclusion")
      for
        _ <- progress(s"Closing task #${run.task.number} with comment...")
        _ <- GitHub.commentConclusion(progress)(
          run.context.root,
          run.task,
          run.runner,
          conclusion
        )
        _ <- GitHub.setIssueStatus(progress)(
          run.context.root,
          run.task.number,
          "completed"
        )
        _ <- GitHub.closeIssue(run.context.root, run.task.number)
        _ <- progress("Task execution finished successfully.")
      yield run
    }

  def checkParentsForCompletion[F[_]: Sync]: -->[F, ClaimedTask, ClaimedTask] =
    Kleisli { run =>
      GitHub
        .checkParentsForCompletion(progress)(
          run.context.root,
          run.task
        )
        .as(run)
    }

  // Completes a task whose implementer already ran in a prior, interrupted
  // invocation and left an open Pull Request behind: verify/merge that PR,
  // then close the task the same way a fresh run's closeTaskIssue would, and
  // sweep up any leftover local worktree/branch (usually already gone).
  // Same merge-conflict repair fallback as createAndMergePrWithConflictRepair:
  // resuming a PR still hits GitHub's "cannot trigger checks while conflicted"
  // wall, so retry via resolveMergeConflict before giving up on the resume.
  // Bounds the CI-check repair loop below: an agent that can't actually fix
  // the failing check would otherwise retry forever, burning agent runs on
  // every resume of this task.
  val MaxPullRequestChecksRepairAttempts = 2

  def startResume[F[_]: Sync]: -->[F, ClaimedTask, PullRequestResume] =
    Kleisli.fromFunction(run => PullRequestResume(run, MaxPullRequestChecksRepairAttempts))

  def resumePullRequest[F[_]: Sync]: -->[F, PullRequestResume, Unit] =
    Kleisli { resume =>
      GitHub.resumeOpenPullRequest(progress)(
        resume.run.context.root,
        resume.run.branchName
      )
    }

  // The two repairable failures, and what each does to the loop's state:
  // a merge conflict is resolved and retried with the budget untouched;
  // a failing check is repaired, pushed, and retried for one fewer attempt.
  def routeResumeFailure[F[_]: Sync]: -->[F, (PullRequestResume, Throwable), Either[Throwable, PullRequestResume]] =
    Kleisli { case (resume, error) =>
      val run = resume.run
      if isMergeConflictError(error) then
        val request = PublishRequest(
          root = run.context.root,
          worktreePath = run.worktreePath,
          branchName = run.branchName,
          baseBranch = run.baseBranch,
          task = run.task,
          finalization = AgentFinalization(None, None),
          runner = run.runner
        )
        for
          _ <- progress(
            s"Merge conflict detected resuming task #${run.task.number}; attempting automatic resolution..."
          )
          resolved <- resolveMergeConflict(progress).run(request)
        yield if resolved then Right(resume) else Left(error)
      else if isPullRequestChecksFailedError(error) && resume.checksRepairAttemptsRemaining > 0 then
        for
          _ <- progress(
            s"Pull Request checks failed for task #${run.task.number}; running repair agent (${run.runner.display}) and retrying (${resume.checksRepairAttemptsRemaining} attempt(s) left)..."
          )
          _ <- repairAndCommitWith(progress)(
            run.worktreePath,
            run.task,
            run.runner,
            prCheckRepairPrompt(run.task, error),
            s"Repair failing Pull Request check for task #${run.task.number}"
          )
          _ <- repairablePush(progress).run(
            PushRequest(run.worktreePath, run.branchName, run.task, run.runner)
          )
        yield Right(
          resume.copy(checksRepairAttemptsRemaining = resume.checksRepairAttemptsRemaining - 1)
        )
      else Left(error).pure[F]
    }

  def isPullRequestChecksFailedError(error: Throwable): Boolean =
    Option(error.getMessage).exists(
      _.contains("Pull Request checks failed for")
    )

  def announceResume[F[_]: Sync]: -->[F, ClaimedTask, ClaimedTask] =
    TaskLogger.progress(run =>
      s"Task #${run.task.number} already has an open Pull Request for ${run.branchName}; resuming to verify and merge instead of re-implementing..."
    )

  def resumedExecution[F[_]: Sync]: -->[F, ClaimedTask, ExecutedTask] =
    Kleisli.fromFunction(run => ExecutedTask(run, output = AgentOutput("")))

  def cleanupAndSummarize[F[_]: Sync]: -->[F, ClaimedTask, RunSummary] =
    Kleisli { completedRun =>
      git[F]
        .cleanupWorktree(
          completedRun.context.root,
          completedRun.worktreePath,
          completedRun.branchName
        )
        .as(
          RunSummary(
            status = Status("completed"),
            message = Message5(
              s"Task #${completedRun.task.number} completed successfully (resumed existing Pull Request)."
            ),
            task = Some(completedRun.task)
          )
        )
    }

  // Left = the Pull Request is gone after all, which is not a failure: fall
  // back to an ordinary run of the task.
  def routeResumeError[F[_]: Sync]: -->[F, (ClaimedTask, Throwable), Either[ClaimedTask, (ClaimedTask, Throwable)]] =
    Kleisli.fromFunction {
      case (run, _: GitHub.NoOpenPullRequestToResumeException) => Left(run)
      case (run, error)                                        => Right((run, error))
    }

  def announceNoPullRequest[F[_]: Sync]: -->[F, ClaimedTask, ClaimedTask] =
    TaskLogger.progress(run =>
      s"No open Pull Request remains for ${run.branchName}; creating a new run instead of resuming."
    )

  def reportResumeFailure[F[_]: Sync]: -->[F, (ClaimedTask, Throwable), RunSummary] =
    Kleisli { case (run, error) =>
      GitHub.commentTaskFailure(progress)(
        run.context.root,
        run.task,
        error.getMessage
      ) *> Sync[F].raiseError(error)
    }

  def taskRun(
      context: RunContext,
      task: Issue,
      runner: TaskRunner
  ): ClaimedTask =
    val taskId = task.number
    val taskName = taskSlug(task.title).getOrElse(s"task-$taskId")
    ClaimedTask(
      context = context,
      task = task,
      runner = runner,
      worktreePath = context.root / ".worktrees" / s"$taskName-$taskId",
      branchName = BranchName(s"task-$taskId"),
      baseBranch = GitHub
        .parentIds(task)
        .headOption
        .map(parentId => BranchName(s"task-$parentId"))
    )

  def taskSlug(title: IssueTitle): Option[String] =
    val slug = title.value.toLowerCase
      .map(char => if char.isLetterOrDigit then char else '-')
      .mkString
      .replaceAll("-+", "-")
      .stripPrefix("-")
      .stripSuffix("-")
      .take(60)
    Option.when(slug.nonEmpty)(slug)

  def taskPrompt(
      task: Issue,
      runner: TaskRunner,
      dependencyConclusion: Option[String],
      replayContext: Option[String]
  ): AgentPrompt =
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

    AgentPrompt(s"""Task ID: #${task.number}
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
   - required abilities/importance (ability -> coefficient), not a pinned runner
4. Prefer splitting until each subtask is small enough that a weaker model such as Haiku could implement it without needing another split.
5. Use this exact metadata format in every subtask description, so the concrete runner is picked at run time from live cost/fit data instead of being pinned now:
   Required abilities/importance:
   - complex-reasoning: 1.0
   - scala: 0.6
   Only add a "preferred llms/models/efforts/versions:" pin instead when the subtask genuinely needs one specific tool/version.
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
""")

  // Implementer runs unattended (`-p`, stdin closed) with zero tool grants
  // previously: no --allowedTools meant every Edit/Write/Bash call hit the
  // permission wall with nobody to approve it, so the agent gave up, printed
  // an explanation, and exited 0 with no files changed — which the pipeline
  // then read as a legitimate no-op success. Grant the tools implementation
  // actually needs; cwd is already confined to the task's own worktree.
  val ImplementerAllowedTools = Seq(
    "Edit",
    "Write",
    "MultiEdit",
    "Read",
    "Glob",
    "Grep",
    "Bash"
  )

  def evaluationStatus(body: IssueBody): Option[String] =
    metadataValue(body, "evaluation")

  def executionStatus(body: IssueBody): Option[Execution] =
    metadataValue(body, "execution").map(Execution.fromString)

  def metadataValue(body: IssueBody, key: String): Option[String] =
    val prefix = s"$key:"
    body.value.linesIterator
      .map(_.trim.toLowerCase)
      .collectFirst {
        case line if line.startsWith(prefix) =>
          line.stripPrefix(prefix).trim
      }

  def extractAgentFinalization(output: AgentOutput): AgentFinalization =
    AgentFinalization(
      commitTitle = extractPrefixedLine(output, "Proposed commit title"),
      pullRequestBody = extractSection(output, "Proposed pull request body")
    )

  def extractPrefixedLine(
      output: AgentOutput,
      label: String
  ): Option[String] =
    val prefix = s"$label:"
    output.value.linesIterator
      .map(_.trim)
      .collectFirst {
        case line if line.toLowerCase.startsWith(prefix.toLowerCase) =>
          line.drop(prefix.length).trim
      }
      .filter(_.nonEmpty)

  def extractSection(
      output: AgentOutput,
      label: String
  ): Option[String] =
    val prefix = s"$label:"
    val lines = output.value.linesIterator.toList
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

  def isFinalizationLabel(line: String): Boolean =
    val normalized = line.trim.toLowerCase
    normalized.startsWith("proposed commit title:") ||
    normalized.startsWith("proposed pull request body:")

  def classifyPublicationSource[F[_]: Sync]: -->[F, PublishRequest, Either[
    ChangedPublication,
    ExistingPublication
  ]] =
    Kleisli { request =>
      git[F]
        .filesChanged(request.worktreePath)
        .map(filesChanged =>
          Either.cond(
            !(filesChanged),
            ExistingPublication(request),
            ChangedPublication(request)
          )
        )
    }

  def prepareChangedPublication[F[_]: Sync]: -->[F, ChangedPublication, PublishRequest] =
    TaskLogger.progress[F, ChangedPublication](_ => "Files changed. Committing and merging changes...") >>> Kleisli {
      changed =>
        val request = changed.request
        git[F]
          .commitAll(
            request.worktreePath,
            request.task,
            request.finalization.commitTitle
          )
          .as(request)
    }

  def prepareExistingPublication[F[_]: Sync]: -->[F, ExistingPublication, PublishRequest] =
    TaskLogger
      .progress[F, ExistingPublication](_ => "No file changes, publishing existing local commits...")
      .map(_.request)

  def choosePublicationTransport[F[_]: Sync]: -->[F, PublishRequest, Either[RemotePublication, LocalPublication]] =
    Kleisli { request =>
      git[F]
        .hasRemote(request.root)
        .map(hasRemote =>
          Either.cond(
            !(hasRemote),
            LocalPublication(request),
            RemotePublication(request)
          )
        )
    }

  def toPushRequest[F[_]: Sync]: -->[F, RemotePublication, PushRequest] =
    Kleisli.fromFunction { remote =>
      PushRequest(
        remote.request.worktreePath,
        remote.request.branchName,
        remote.request.task,
        remote.request.runner
      )
    }

  def toPublishRequest[F[_]: Sync]: -->[F, RemotePublication, PublishRequest] =
    Kleisli.fromFunction(_.request)

  def createAndMergePullRequest[F[_]: Sync]: -->[F, PublishRequest, Unit] =
    Kleisli { request =>
      GitHub.createAndMergePr(progress)(
        request.root,
        request.worktreePath,
        request.branchName,
        request.baseBranch,
        request.task,
        request.finalization.commitTitle,
        request.finalization.pullRequestBody
      )
    }

  // GitHub cannot trigger checks on a PR with merge conflicts against its base
  // branch (see GitHub.awaitPullRequestChecks). Rather than failing the task,
  // try folding the base branch into the worktree ourselves; if that leaves
  // conflict markers, hand it to a repair agent (same pattern as
  // routePushFailure/repairAndCommit below), then retry the PR creation/merge.
  def routeMergeFailure[F[_]: Sync]: -->[F, (PublishRequest, Throwable), Either[Throwable, PublishRequest]] =
    Kleisli { case (request, error) =>
      if isMergeConflictError(error) then
        for
          _ <- progress(
            s"Merge conflict detected for task #${request.task.number}; attempting automatic resolution..."
          )
          resolved <- resolveMergeConflict(progress)(request)
        yield if resolved then Right(request) else Left(error)
      else Left(error).pure[F]
    }

  def isMergeConflictError(error: Throwable): Boolean =
    Option(error.getMessage).exists(
      _.contains("has merge conflicts with its base branch")
    )

  def resolveMergeConflict[F[_]](progress: String => F[Unit])(using
      F: Sync[F]
  ): Kleisli[F, PublishRequest, Boolean] =
    Kleisli.apply { request =>
      val baseBranch = request.baseBranch.getOrElse(BranchName("master"))
      val pushRequest = PushRequest(request.worktreePath, request.branchName, request.task, request.runner)
      for
        autoMerged <- git[F].mergeBaseBranch(
          request.worktreePath,
          baseBranch.value
        )
        resolved <-
          if autoMerged then
            progress(
              s"Automatically merged $baseBranch into ${request.branchName} for task #${request.task.number}."
            ) *> repairablePush(progress).run(pushRequest).as(true)
          else
            for
              conflictedFiles <- git[F].unresolvedConflictFiles(
                request.worktreePath
              )
              conflictedFilesText = conflictedFiles.mkString(", ")
              _ <- progress(
                s"Automatic merge failed for task #${request.task.number}; running repair agent (${request.runner.display}) on $conflictedFilesText..."
              )
              _ <- AgentExecutor[F].run(
                request.runner,
                mergeConflictRepairPrompt(request.task, baseBranch.value, conflictedFiles),
                request.worktreePath,
                RepairAllowedTools,
                contextFiles = conflictedFiles,
                taskNumber = Some(request.task.number),
                metricsRoot = Some(request.root),
                metricsScope = "merge-repair"
              )
              stillConflicted <- git[F].hasUnresolvedConflicts(
                request.worktreePath
              )
              resolved <-
                if stillConflicted then
                  progress(
                    s"Repair agent left unresolved conflicts for task #${request.task.number}; aborting merge."
                  ) *> git[F].abortMerge(request.worktreePath).as(false)
                else
                  git[F]
                    .commitAll(
                      request.worktreePath,
                      request.task,
                      Some(
                        s"Merge $baseBranch into ${request.branchName}, resolve conflicts"
                      )
                    ) *> repairablePush(progress).run(pushRequest).as(true)
            yield resolved
      yield resolved
    }

  def mergeConflictRepairPrompt(
      task: Issue,
      baseBranch: String,
      conflictedFiles: Seq[String]
  ): AgentPrompt = AgentPrompt(
    s"""This branch has a `git merge` in progress against `$baseBranch` that produced conflict
       |markers (`<<<<<<<` / `=======` / `>>>>>>>`). Resolve every conflict in this worktree so
       |the merge can complete cleanly, preserving the intended behavior of both sides, without
       |changing the task's intended behavior.
       |
       |Unmerged files reported by Git:
       |${conflictedFiles.map(file => s"- $file").mkString("\n")}
       |
       |Run `git status --short`, resolve every unmerged path, and `git add` each resolved file
       |so `git diff --name-only --diff-filter=U` prints nothing. Do not run `git commit`,
       |`git merge --abort`, or `git push` yourself.
       |
       |Task: #${task.number} ${task.title}
       |""".stripMargin
  )

  def pushBranch[F[_]: Sync]: -->[F, PushRequest, Unit] =
    Kleisli(request => git[F].push(request.worktreePath, request.branchName))

  // `git push` runs the repo's prePush hook (tests/lint/format). A failure
  // there is usually fixable in-worktree (e.g. a broken test), and the branch
  // must be pushed before the task can be considered handled, so repair and
  // retry instead of moving on to another task.
  def routePushFailure[F[_]: Sync]: -->[F, (PushRequest, Throwable), Either[Throwable, PushRequest]] =
    Kleisli { case (request, error) =>
      for
        _ <- progress(
          s"Push failed for task #${request.task.number}: ${error.getMessage}"
        )
        _ <- repairAndCommit(progress)(
          (request.worktreePath, request.task, request.runner, error)
        )
      yield Right(request)
    }

  def repairablePush[F[_]: Sync](progress: String => F[Unit]): Kleisli[F, PushRequest, Unit] =
    RepairLoop(pushBranch, routePushFailure, Kleisli[F, Throwable, Unit](Sync[F].raiseError))

  val RetryPromptTimeout = 30.seconds

  def askRetryWithRepair[F[_]](using
      F: Sync[F]
  ): Kleisli[F, TaskNumber, Boolean] =
    Kleisli.apply { taskNumber =>
      F.blocking {
        print(
          s"Repair push failure for task #$taskNumber with an agent and retry? [y/N]: "
        )
        System.out.flush()
        readLineWithTimeout(RetryPromptTimeout) match
          case Some(answer) =>
            answer.trim.equalsIgnoreCase("y") ||
            answer.trim.equalsIgnoreCase("yes")
          case None =>
            println(
              s"No response in ${RetryPromptTimeout.toSeconds}s, defaulting to y"
            )
            true
      }
    }

  // scala.io.StdIn.readLine() blocks with no timeout support, so read on a
  // daemon thread and join with a deadline; an unanswered prompt must not
  // hang the process forever. Closed/non-interactive stdin (the normal case
  // for this executor running unattended) makes readLine() return null
  // immediately - that must fall through to the same "no answer" default-yes
  // path as a real timeout, not be treated as an explicit empty answer.
  def readLineWithTimeout(timeout: FiniteDuration): Option[String] =
    val result = new java.util.concurrent.atomic.AtomicReference[String](null)
    val reader = new Thread(() =>
      scala.io.StdIn.readLine() match
        case null => ()
        case line => result.set(line)
    )
    reader.setDaemon(true)
    reader.start()
    reader.join(timeout.toMillis)
    Option(result.get())

  // Repair agent runs unattended just like the implementer (see
  // ImplementerAllowedTools) — without tool grants it hits the permission
  // wall on every Edit/Bash call, gives up instantly, and the retry loop
  // spins forever with zero progress.
  val RepairAllowedTools = ImplementerAllowedTools

  def repairAndCommit[F[_]](progress: String => F[Unit])(using
      F: Sync[F]
  ): Kleisli[F, (os.Path, Issue, TaskRunner, Throwable), Unit] =
    Kleisli.apply { case (worktreePath, task, runner, pushError) =>
      repairAndCommitWith(progress)(
        worktreePath,
        task,
        runner,
        repairPrompt(task, pushError),
        s"Repair prePush failure for task #${task.number}"
      )
    }

  // Shared by pushWithRepair (prePush hook failures) and
  // resumeOpenPullRequestWithConflictRepair (post-push CI check failures) —
  // only the prompt shown to the agent and the eventual commit message differ.
  def repairAndCommitWith[F[_]](progress: String => F[Unit])(
      worktreePath: os.Path,
      task: Issue,
      runner: TaskRunner,
      prompt: AgentPrompt,
      commitMessage: String
  )(using F: Sync[F]): F[Unit] =
    for
      _ <- progress(
        s"Running repair agent (${runner.display}) for task #${task.number}..."
      )
      _ <- AgentExecutor[F].run(
        runner,
        prompt,
        worktreePath,
        RepairAllowedTools,
        taskNumber = Some(task.number),
        metricsScope = "repair"
      )
      changed <- git[F].filesChanged(worktreePath)
      _ <-
        if changed then git[F].commitAll(worktreePath, task, Some(commitMessage))
        else
          progress(
            s"Repair agent made no file changes for task #${task.number}."
          )
    yield ()

  def repairPrompt(task: Issue, pushError: Throwable): AgentPrompt =
    AgentPrompt(
      s"""`git push` failed for task #${task.number} (${task.title}), most likely because the
       |repo's prePush hook (tests/lint/format) rejected the current commit. Fix the underlying
       |issue in this worktree so the prePush hook passes, without changing the task's intended
       |behavior. Do not run git push yourself.
       |
       |Failure output:
       |${pushError.getMessage}
       |""".stripMargin
    )

  def prCheckRepairPrompt(task: Issue, checksError: Throwable): AgentPrompt =
    AgentPrompt(
      s"""CI checks failed on the open Pull Request for task #${task.number} (${task.title}).
       |Fix the underlying issue in this worktree so the checks pass, without changing the
       |task's intended behavior. Do not run git push yourself.
       |
       |Failure output:
       |${checksError.getMessage}
       |""".stripMargin
    )

  def publishLocal[F[_]: Sync]: -->[F, LocalPublication, Unit] =
    Kleisli { local =>
      val request = local.request
      git[F].mergeLocally(
        request.root,
        request.worktreePath,
        request.branchName,
        request.baseBranch
      )
    }

  def progress[F[_]: Sync](message: String): F[Unit] =
    TaskLogger.script(message)
