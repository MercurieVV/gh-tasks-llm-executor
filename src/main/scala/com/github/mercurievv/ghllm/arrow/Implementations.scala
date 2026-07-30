package com.github.mercurievv.ghllm.arrow

import com.github.mercurievv.ghllm.*
import com.github.mercurievv.ghllm.agent.*
import com.github.mercurievv.ghllm.arrow.*
import com.github.mercurievv.ghllm.cli.*
import com.github.mercurievv.ghllm.git.*
import com.github.mercurievv.ghllm.metrics.*

import cats.data.Kleisli
import cats.effect.Deferred
import cats.effect.Resource
import cats.effect.kernel.Async
import cats.effect.kernel.Sync
import cats.syntax.all.*
import cats.syntax.arrow.*
import arrowstep.core.ProgramSays
import cats.arrow.Arrow

import scala.concurrent.duration.*
import scala.util.Try

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.util.Base64
import java.util.concurrent.CancellationException

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

  // Mandates the use of scalameta tools for navigating Scala sources.
  // This constant is appended to subagent prompts when the task touches .scala files,
  // as defined by the token efficiency plan (T13).
  val ScalaSemanticMandate: String =
    """
      |
      |SCALA SEMANTIC NAVIGATION RULE (must be obeyed):
      |For any question about symbols, types, signatures, hierarchy, implicits, references, or call paths in `.scala` files, use `mcp__scala-semantic__*` tools.
      |Never use `cat`, `rg`, `sed`, `grep`, `head`, or `tail` against `.scala` files.
      |Use `document_outline` to understand a file's API instead of reading the entire file.
      |""".stripMargin

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
        notActivelyClaimed <- filteredByDeps.filterA { task =>
          val labeledInProgress =
            task.labels.exists(label => label === "status: in progress" || label === "in progress")
          IssueClaim.hasActiveClaim(context.root, task.number, progress).flatMap { claimed =>
            if claimed then
              TaskLogger
                .trace(s"selectTask excluding #${task.number}: active claim ref exists")
                .as(false)
            else if labeledInProgress then
              TaskLogger.trace(
                s"selectTask allowing #${task.number}: clearing stale in-progress labels with no active claim ref"
              ) *> GitHub
                .clearInProgressStatus(progress)(None)((context.root, task.number))
                .as(true)
            else true.pure[F]
          }
        }
        notAlreadyCompleted <- notActivelyClaimed.filterA { task =>
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
        // One backend for the whole selection pass: `selectRunnerFor` reads
        // observed (phase, runner) success from it to apply the break-even
        // predicate, and falls back to Priority.score when the sample is short.
        metricsBackend = TokenMetrics.defaultBackend(context.root)
        candidates = eligibleWithResumeFlag.map { case (task, hasOpenPr) =>
          val metadata = TaskMetadata.parse(task.body.value)
          TaskCandidate(
            context,
            task,
            context.agentInventory
              .selectRunnerFor(
                metadata.requiredAbilities,
                GitHub.taskRunners(task),
                phase = metadata.phase,
                metricsBackend = Some(metricsBackend)
              )
              .getOrElse(evaluatorRunner),
            resumePullRequest = ResumePullRequest(hasOpenPr)
          )
        }
        _ <- progress(
          context.taskNumber.fold(
            s"Found ${candidates.size} runnable open tasks after dependency, child-task, needs-input, completion, and claim filters."
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
  // other queued candidate in the same batch. Report escaped failures against
  // that candidate and keep the outer run moving.
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
      val metadata = TaskMetadata.parse(issue.body.value)
      val runner = context.agentInventory
        .selectRunnerFor(
          metadata.requiredAbilities,
          GitHub.taskRunners(issue),
          phase = metadata.phase,
          metricsBackend = Some(TokenMetrics.defaultBackend(context.root))
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
        "No runnable open tasks found after dependency, child-task, needs-input, completion, and claim filters."
      )(number =>
        s"No runnable open task found for #$number. It may be closed, blocked by dependencies or open child tasks, marked needs-input, or already claimed by another process."
      )
      RunSummary(
        status = Status("no-task"),
        message = Message5(message),
        task = None
      ).pure[F]
    }

  def routeRun[F[_]: Sync]: -->[F, TaskCandidate, Either[ClaimedTask, ClaimedTask]] =
    Kleisli(task => Right(taskRun(task.context, task.issue, task.runner)).pure[F])

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
    }).map({ case (run, dependencyConclusion) =>
      PreparedTask(run, dependencyConclusion, replayContext = None)
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

  // Records the durable "implemented" mark once the agent output has been
  // committed and published. Idempotent: re-runs that resumed via the
  // already-implemented short-circuit skip re-writing an identical mark.
  def markTaskImplemented[F[_]: Sync]: -->[F, ExecutedTask, ExecutedTask] =
    Kleisli { (task: ExecutedTask) =>
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

  def runTaskWithRunner[F[_]: Async]: -->[RunF[F], PreparedTask, ExecutedTask] =
    RunEnv.read { (env, task) =>
      val run = task.claimedTask
      val prompt =
        taskPrompt(
          run.task,
          run.runner,
          task.parentConclusion,
          task.replayContext
        )
      val execution =
        for
          _ <- refreshSemanticDbBeforeDispatch(env, run.task, run.worktreePath)
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
            metricsScope = "implement",
            deferMetricsOutcome = true,
            phase = TaskMetadata.parse(run.task.body.value).phase
          )
          _ <- Sync[F]
            .raiseError(
              RuntimeException(
                s"Agent ${run.runner.display} reported it could not proceed (permission/tool wall). Output: ${output.value.trim}"
              )
            )
            .whenA(looksBlocked(output))
        yield ExecutedTask(run, output)

      execution.handleErrorWith { error =>
        AgentExecutor.completeTokenMetrics[F](
          run.context.root,
          run.task.number,
          run.runner,
          "implement",
          "error"
        ) *> Sync[F].raiseError(error)
      }
    }

  def taskTouchesScala(task: Issue): Boolean =
    s"${task.title.value}\n${task.body.value}".toLowerCase.contains(".scala")

  def scalaSourceHash(root: os.Path): String =
    val ignoredDirectories =
      Set(".bsp", ".git", ".scala-build", ".semanticdb", ".worktrees", "out", "target")
    val digest = MessageDigest.getInstance("SHA-256")

    def update(bytes: Array[Byte]): Unit =
      digest.update(ByteBuffer.allocate(java.lang.Long.BYTES).putLong(bytes.length.toLong).array())
      digest.update(bytes)

    os.walk(root)
      .filter(path =>
        os.isFile(path) &&
          path.ext.equalsIgnoreCase("scala") &&
          !path.relativeTo(root).segments.exists(ignoredDirectories)
      )
      .sortBy(_.relativeTo(root).toString)
      .foreach { path =>
        update(path.relativeTo(root).toString.getBytes(StandardCharsets.UTF_8))
        update(os.read.bytes(path))
      }

    Base64.getUrlEncoder.withoutPadding().encodeToString(digest.digest())

  def refreshSemanticDbBeforeDispatch[F[_]: Async](
      env: RunEnv[F],
      task: Issue,
      root: os.Path
  ): F[Unit] =
    if taskTouchesScala(task) then
      Async[F]
        .blocking(scalaSourceHash(root))
        .flatMap(hash =>
          refreshSemanticDbIfNeeded(env, SemanticDbSource(root, hash))(
            Async[F].blocking {
              os.proc((root / "scripts" / "refresh-semanticdb.sh").toString)
                .call(cwd = root, stdout = os.Inherit, stderr = os.Inherit)
              ()
            }
          )
        )
    else Async[F].unit

  def refreshSemanticDbIfNeeded[F[_]: Async](
      env: RunEnv[F],
      source: SemanticDbSource
  )(refresh: F[Unit]): F[Unit] =
    Deferred[F, Either[Throwable, Unit]].flatMap { fresh =>
      env.semanticDbRefreshes.modify { refreshes =>
        refreshes.get(source) match
          case Some(existing) =>
            refreshes -> existing.get.flatMap(_.liftTo[F])
          case None =>
            val removeFailed = env.semanticDbRefreshes.update(_ - source)
            val run =
              Async[F].onCancel(
                refresh.attempt.flatMap { result =>
                  result.fold(_ => removeFailed, _ => Async[F].unit) *>
                    fresh.complete(result).void *>
                    result.liftTo[F]
                },
                removeFailed *>
                  fresh
                    .complete(
                      Left(
                        CancellationException(
                          s"SemanticDB refresh canceled for ${source.root}"
                        )
                      )
                    )
                    .void
              )
            refreshes.updated(source, fresh) -> run
      }.flatten
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
    Kleisli { task =>
      git[F].runProjectValidation.run(task.run.worktreePath).attempt.flatMap {
        case Right(_) =>
          AgentExecutor
            .completeTokenMetrics[F](
              task.run.context.root,
              task.run.task.number,
              task.run.runner,
              "implement",
              "green"
            )
            .as(task)
        case Left(error) =>
          AgentExecutor.completeTokenMetrics[F](
            task.run.context.root,
            task.run.task.number,
            task.run.runner,
            "implement",
            "red"
          ) *> Sync[F].raiseError(error)
      }
    }

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

    // Same predicate as the SemanticDB refresh gate, deliberately: the two
    // Stage 3 features must agree on what "a Scala task" is, and body-only
    // matching missed tasks that name their files in the title.
    val scalaMandate = if taskTouchesScala(task) then s"\n\n$ScalaSemanticMandate" else ""

    // Ordered most-stable-first (TOKEN_EFFICIENCY_PLAN.md §2 Stage 6, T19).
    // Prompt caching is prefix-only: one volatile byte early invalidates every
    // segment after it, so the fully constant blocks lead, the task's own text
    // trails, and sibling leaves fanning out on the same runner share the
    // longest possible prefix. This matters more now that fan-out marks that
    // prefix with the 1-hour TTL (T20), which costs ~2x to write and only pays
    // back over ~3 reads. Segment CONTENT is unchanged from the original order.
    //
    // `Workflow` is conventions and belongs in the stable region, but it
    // interpolates the task number, so it can never be part of a shared prefix
    // and sits after the blocks that can.
    AgentPrompt(s"""Agent boundary:
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
$scalaMandate
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
$dependencyConclusionStr
$replayContextStr
Task ID: #${task.number}
Title: ${task.title}
Agent: ${runner.agent}
Model: ${runner.model.getOrElse("")}
Effort: ${runner.effort.getOrElse("")}
Version: ${runner.version.getOrElse("")}

Task Description:
${task.body}
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

  def pushBranch[F[_]: Sync]: -->[F, PushRequest, Unit] =
    Kleisli(request => git[F].push(request.worktreePath, request.branchName))

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
