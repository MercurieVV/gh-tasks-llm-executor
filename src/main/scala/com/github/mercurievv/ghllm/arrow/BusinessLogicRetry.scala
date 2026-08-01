package com.github.mercurievv.ghllm.arrow

import com.github.mercurievv.ghllm.*
import com.github.mercurievv.ghllm.agent.*
import com.github.mercurievv.ghllm.cli.Flow
import com.github.mercurievv.ghllm.git.*

import cats.data.Kleisli
import cats.effect.kernel.Sync
import cats.syntax.all.*

import scala.concurrent.duration.*
import cats.syntax.all.*

object BusinessLogicRetry:

  def apply[F[_]: Sync](
      progress: String => F[Unit]
  ): BusinessLogic[Retryability.RetryFlow[Flow[F]]] =
    type RealArr = Flow[F]
    type RetryArr = Retryability.RetryFlow[RealArr]
    import Retryability.given

    val empty = Monoid2K[BusinessLogic].emptyK[RetryArr]

    empty.copy(
      taskArrows = empty.taskArrows.copy(
        resumeExistingPullRequest = empty.taskArrows.resumeExistingPullRequest.copy(
          pullRequest = empty.taskArrows.resumeExistingPullRequest.pullRequest.copy(
            resumeOpenPullRequest = retryResumePullRequest(progress)
          )
        )
      ),
      publicationArrows = empty.publicationArrows.copy(
        publishRemote = empty.publicationArrows.publishRemote.copy(
          pushBranch = retryWithRepair(
            routeFailure = routePushFailure(progress),
            raiseFailure = raiseK
          ),
          createAndMergePullRequest = retryWithRepair(
            routeFailure = routeMergeFailure(progress),
            raiseFailure = raiseK
          )
        )
      ),
      executeTaskArrows = empty.executeTaskArrows.copy(
        runAgent = empty.executeTaskArrows.runAgent.copy(
          // The verifier runs INSIDE the escalation loop, not after it.
          //
          // `BusinessLogic.executePreparedTaskInWorktree` composes
          // `runAgent >>> runProjectValidation`, and this decorator only ever
          // wrapped `runAgent` - so compile and test failures, which is what
          // "red" almost always means, raised past the ladder and killed the
          // task instead of escalating it. Only errors thrown inside the agent
          // run itself (a turn cap, a dead process) ever reached
          // `routeRunnerFallback`. That is the wrong half: the plan's Stage 1
          // is "run -> compile + test -> red: escalate", and the free verifier
          // is the whole reason a cheap runner can be tried first.
          // `rejectEmptyRun` joins it for the same reason: a run that delivered
          // nothing is a rejection of the agent's work, and outside this loop it
          // would kill the task instead of escalating it - or, worse, reach
          // publication and close the issue green over commits it never made.
          //
          // It runs BEFORE the validation, and the order is not cosmetic. A run
          // that changed nothing leaves a tree that still compiles and still
          // passes, so `runProjectValidation` recorded `outcome = "green"` for
          // it - and since completing the metrics removes the pending event,
          // the rejection that followed recorded nothing at all. Every empty run
          // was a success in `successRate`, biasing selection toward whichever
          // runner shrugs the fastest. Rejecting first records the "red" the
          // attempt earned, and skips a validation of work that does not exist.
          runTaskWithRunner = run =>
            retryRunTaskWithRunner(progress)(
              run.andThen(Impl.rejectEmptyRun[F]).andThen(Impl.runProjectValidation[F])
            )
        ),
        // Already run above, once per attempt. Left in place it would re-run the
        // whole suite on the winning attempt and record a second sample for it.
        runProjectValidation = _ => Kleisli.ask[F, ExecutedTask]
      )
    )

  def raiseK[F[_]: Sync, A]: Flow[F][Throwable, A] =
    Kleisli(Sync[F].raiseError)

  def retryRunTaskWithRunner[F[_]: Sync](
      progress: String => F[Unit]
  )(
      runTaskWithRunner: Flow[F][PreparedTask, ExecutedTask]
  ): Flow[F][PreparedTask, ExecutedTask] =
    retryRunTaskWithRunner(routeRunnerFallback(progress), raiseK)(runTaskWithRunner)

  def retryRunTaskWithRunner[F[_]: Sync](
      routeFallback: Flow[F][(PreparedTask, VerificationResult), Either[VerificationResult, PreparedTask]],
      raiseFailure: Flow[F][Throwable, ExecutedTask]
  )(
      runTaskWithRunner: Flow[F][PreparedTask, ExecutedTask]
  ): Flow[F][PreparedTask, ExecutedTask] =
    Kleisli { initial =>
      // A loop, not a single retry: the ladder can have more than two rungs, and
      // `routeFallback` is what bounds it (see routeRunnerFallback's depth cap).
      def loop(task: PreparedTask): F[ExecutedTask] =
        runTaskWithRunner.run(task).attempt.flatMap {
          case Right(result) => result.pure[F]
          case Left(error) =>
            routeFallback.run((task, VerificationResult.fromThrowable(error))).flatMap {
              case Left(verdict)       => raiseFailure.run(verdict.asThrowable)
              case Right(fallbackTask) => Sync[F].defer(loop(fallbackTask))
            }
        }

      loop(initial)
    }

  def retryWithRepair[F[_]: Sync, A](
      routeFailure: Flow[F][(A, Throwable), Either[Throwable, A]],
      raiseFailure: Flow[F][Throwable, Unit]
  )(
      action: Flow[F][A, Unit]
  ): Flow[F][A, Unit] =
    Kleisli { initial =>
      def loop(state: A): F[Unit] =
        action.run(state).handleErrorWith { error =>
          routeFailure.run((state, error)).flatMap {
            case Left(finalError) => raiseFailure.run(finalError)
            case Right(nextState) => Sync[F].defer(loop(nextState))
          }
        }

      loop(initial)
    }

  val MaxEscalationDepth = 2

  def routeRunnerFallback[F[_]: Sync](
      progress: String => F[Unit]
  ): Flow[F][(PreparedTask, VerificationResult), Either[VerificationResult, PreparedTask]] =
    Kleisli { case (task, verdict) =>
      val claimed = task.claimedTask
      val reason = verdict.escalationSeed.getOrElse("no detail reported")
      if verdict.isGreen then
        // Green must never reach this arrow. Return it unchanged rather than
        // escalating a success into a paid re-run.
        Left(verdict).pure[F]
      else if verdict.isPolicyRejection then
        // Not a capability gap: the task's phase forbids the change the task
        // needs, so a stronger runner reaches the same refusal. Escalating this
        // cost three runs (one of them gpt-5 at high effort) before surfacing
        // the same message the first run already produced.
        progress(
          s"Task #${claimed.task.number} was rejected on policy, not capability: $reason. Not escalating - fix the task instead."
        ).as(Left(verdict))
      else if task.escalationDepth >= MaxEscalationDepth then
        progress(
          s"Task #${claimed.task.number} still failing after $MaxEscalationDepth escalation(s): $reason. Giving up so a human can look at it."
        ).as(Left(verdict))
      else
        claimed.context.agentInventory.nextStrongerImplementor(claimed.runner) match
          case Some(fallbackRunner) =>
            for
              _ <- progress(
                s"Runner ${claimed.runner.display} failed after retries: $reason. Retrying task #${claimed.task.number} with stronger fallback ${fallbackRunner.display}..."
              )
              // Before, not after: the stronger runner must start from HEAD, not
              // from the cheap runner's half-finished edits.
              _ <- Git[F](progress).resetWorktree(claimed.worktreePath).attempt.flatMap {
                case Right(_)    => Sync[F].unit
                case Left(error) =>
                  // A worktree we cannot clean is not a reason to abandon the
                  // task — escalate anyway and say so.
                  progress(
                    s"Could not reset worktree ${claimed.worktreePath} before escalating: ${error.getMessage}"
                  )
              }
            yield Right(
              task.copy(
                claimedTask = claimed.copy(runner = fallbackRunner),
                escalationDepth = task.escalationDepth + 1
              )
            )
          case None =>
            progress(
              s"Runner ${claimed.runner.display} failed after retries and no stronger fallback runner is available."
            ).as(Left(verdict))
    }

  val MaxPullRequestChecksRepairAttempts = 2

  private final case class PullRequestResume(
      run: ClaimedTask,
      checksRepairAttemptsRemaining: Int
  )

  def retryResumePullRequest[F[_]: Sync](
      progress: String => F[Unit]
  )(
      resumePullRequest: Flow[F][ClaimedTask, Unit]
  ): Flow[F][ClaimedTask, Unit] =
    val resumeOnce: Flow[F][PullRequestResume, Unit] =
      Kleisli(resume => resumePullRequest.run(resume.run))
    val retrying = retryWithRepair(
      routeFailure = routeResumeFailure(progress),
      raiseFailure = raiseK[F, Unit]
    )(resumeOnce)

    Kleisli(run => retrying.run(PullRequestResume(run, MaxPullRequestChecksRepairAttempts)))

  private def routeResumeFailure[F[_]: Sync](
      progress: String => F[Unit]
  ): Flow[F][(PullRequestResume, Throwable), Either[Throwable, PullRequestResume]] =
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
        yield Either.cond(resolved, resume, error)
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
            s"Repair failing Pull Request check for task #${run.task.number}",
            run.context.agentInventory
          )
          _ <- repairablePush(progress).run(
            PushRequest(run.context.root, run.worktreePath, run.branchName, run.task, run.runner)
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

  def routeMergeFailure[F[_]: Sync](
      progress: String => F[Unit]
  ): Flow[F][(PublishRequest, Throwable), Either[Throwable, PublishRequest]] =
    Kleisli { case (request, error) =>
      if isMergeConflictError(error) then
        for
          _ <- progress(
            s"Merge conflict detected for task #${request.task.number}; attempting automatic resolution..."
          )
          resolved <- resolveMergeConflict(progress)(request)
        yield Either.cond(resolved, request, error)
      else if isPullRequestChecksFailedError(error) then
        for
          _ <- progress(
            s"Pull Request checks failed for task #${request.task.number}; running repair agent (${request.runner.display}) and retrying publication..."
          )
          _ <- repairAndCommitWith(progress)(
            request.worktreePath,
            request.task,
            request.runner,
            prCheckRepairPrompt(request.task, error),
            s"Repair failing Pull Request check for task #${request.task.number}",
            AgentInventory.load(request.root)
          )
          _ <- repairablePush(progress).run(
            PushRequest(request.root, request.worktreePath, request.branchName, request.task, request.runner)
          )
        yield Right(request)
      else Left(error).pure[F]
    }

  /** Recognises a conflict from whichever layer reported it.
    *
    * The three original signatures are GitHub's GraphQL wording, which is what surfaces when the API rejects the merge.
    * `gh pr merge` phrases the same condition differently ("is not mergeable: the merge commit cannot be cleanly
    * created", followed by instructions to resolve locally), and on 2026-08-01 that wording fell through to `Left`,
    * failing task #56 outright with a conflict the repair path exists to resolve.
    */
  def isMergeConflictError(error: Throwable): Boolean =
    Option(error.getMessage).exists { message =>
      val normalized = message.toLowerCase
      normalized.contains("has merge conflicts with its base branch") ||
      normalized.contains("pull request has merge conflicts") ||
      normalized.contains("mergepullrequest") ||
      normalized.contains("is not mergeable") ||
      normalized.contains("merge commit cannot be cleanly created") ||
      normalized.contains("resolve the merge conflicts")
    }

  def resolveMergeConflict[F[_]](progress: String => F[Unit])(using
      F: Sync[F]
  ): Kleisli[F, PublishRequest, Boolean] =
    Kleisli.apply { request =>
      val baseBranch = request.baseBranch.getOrElse(BranchName("master"))
      val pushRequest =
        PushRequest(request.root, request.worktreePath, request.branchName, request.task, request.runner)
      for
        autoMerged <- Impl
          .git[F]
          .mergeBaseBranch(
            request.worktreePath,
            baseBranch.value
          )
        _ <-
          if autoMerged then
            progress(
              s"Automatically merged $baseBranch into ${request.branchName} for task #${request.task.number}."
            ) *> repairablePush(progress).run(pushRequest)
          else
            repairMergeConflictsUntilClean(progress)(request, baseBranch) *>
              Impl
                .git[F]
                .commitAll(
                  request.worktreePath,
                  request.task,
                  Some(
                    s"Merge $baseBranch into ${request.branchName}, resolve conflicts"
                  )
                ) *>
              repairablePush(progress).run(pushRequest)
      yield true
    }

  private def repairMergeConflictsUntilClean[F[_]](
      progress: String => F[Unit]
  )(request: PublishRequest, baseBranch: BranchName)(using F: Sync[F]): F[Unit] =
    repairMergeConflictsUntilClean(progress)(
      request,
      baseBranch,
      request.runner,
      attemptedRunners = Nil
    )

  private def repairMergeConflictsUntilClean[F[_]](
      progress: String => F[Unit]
  )(
      request: PublishRequest,
      baseBranch: BranchName,
      runner: TaskRunner,
      attemptedRunners: List[TaskRunner]
  )(using F: Sync[F]): F[Unit] =
    for
      conflictedFiles <- Impl
        .git[F]
        .unresolvedConflictFiles(
          request.worktreePath
        )
      _ <-
        if conflictedFiles.isEmpty then F.unit
        else
          val conflictedFilesText = conflictedFiles.mkString(", ")
          for
            _ <- progress(
              s"Automatic merge failed for task #${request.task.number}; running repair agent (${runner.display}) on $conflictedFilesText..."
            )
            repairResult <- AgentExecutor[F]
              .run(
                runner,
                mergeConflictRepairPrompt(request.task, request.branchName.value, baseBranch.value, conflictedFiles),
                request.worktreePath,
                RepairAllowedTools,
                contextFiles = conflictedFiles,
                taskNumber = Some(request.task.number),
                metricsRoot = Some(request.root),
                metricsScope = AgentExecutor.MetricsScope.MergeRepair,
                // Without this the repair events are dimensionless: recorded,
                // but counted by `metrics readiness` as invisible to runner
                // selection, because meanUsage is keyed on (phase, runner).
                phase = TestEditGuard.phaseOf(request.task.body)
              )
              .attempt
            _ <-
              repairResult match
                case Left(error) =>
                  continueMergeRepair(progress)((request, baseBranch, runner, attemptedRunners, Some(error)))
                case Right(_) =>
                  for
                    stillConflicted <- Impl
                      .git[F]
                      .hasUnresolvedConflicts(
                        request.worktreePath
                      )
                    _ <-
                      if stillConflicted then
                        continueMergeRepair(progress)((request, baseBranch, runner, attemptedRunners, None))
                      else F.unit
                  yield ()
          yield ()
    yield ()

  private def continueMergeRepair[F[_]](progress: String => F[Unit])(using F: Sync[F]): Kleisli[F, (PublishRequest, BranchName, TaskRunner, List[TaskRunner], Option[Throwable]), Unit] =
  Kleisli.apply { case (request, baseBranch, runner, attemptedRunners, error) =>
    val nextAttempted = runner :: attemptedRunners
    val inventory = AgentInventory.load(request.root)
    inventory.alternateImplementor(runner, attemptedRunners) match
      case Some(fallbackRunner) =>
        val reason = error.fold("left unresolved conflicts")(e => s"failed: ${e.getMessage}")
        progress(
          s"Repair agent ${runner.display} $reason for task #${request.task.number}; retrying merge repair with ${fallbackRunner.display}..."
        ) *> Sync[F].defer(
          repairMergeConflictsUntilClean(progress)(request, baseBranch, fallbackRunner, nextAttempted)
        )
      case None =>
        val suffix = error.fold("left unresolved conflicts")(e => s"failed: ${e.getMessage}")
        RuntimeException(
          s"Merge repair for task #${request.task.number} could not complete: ${runner.display} $suffix and no alternate implementor is available."
        ).raiseError[F, Unit]
  }

  def mergeConflictRepairPrompt(
      task: Issue,
      headBranch: String,
      baseBranch: String,
      conflictedFiles: Seq[String]
  ): AgentPrompt = AgentPrompt(
    s"""This worktree is on head branch `$headBranch` and has a `git merge` in progress against
       |base branch `$baseBranch` that produced conflict markers (`<<<<<<<` / `=======` /
       |`>>>>>>>`). Resolve every conflict so the merge can complete cleanly, preserving the
       |intended behavior of both branches without changing the task's intended behavior.
       |
       |Unmerged files reported by Git:
       |${conflictedFiles.map(file => s"- $file").mkString("\n")}
       |
       |For each conflicted file, inspect both sides before editing:
       |- `git diff --ours -- <file>` shows the head branch `$headBranch` side.
       |- `git diff --theirs -- <file>` shows the base branch `$baseBranch` side being merged in.
       |- `git log --oneline --decorate --graph HEAD...origin/$baseBranch` shows the divergent
       |  branch history when you need more context.
       |
       |Run `git status --short`, resolve every unmerged path, and `git add` each resolved file
       |so `git diff --name-only --diff-filter=U` prints nothing. Do not run `git commit`,
       |`git merge --abort`, or `git push` yourself.
       |
       |Task: #${task.number} ${task.title}
       |""".stripMargin
  )

  def pushBranch[F[_]: Sync]: Flow[F][PushRequest, Unit] =
    Impl.git[F].push.local((request: PushRequest) => (request.worktreePath, request.branchName))

  def routePushFailure[F[_]: Sync](
      progress: String => F[Unit]
  ): Flow[F][(PushRequest, Throwable), Either[Throwable, PushRequest]] =
    Kleisli { case (request, error) =>
      for
        _ <- progress(
          s"Push failed for task #${request.task.number}: ${error.getMessage}"
        )
        _ <- repairAndCommit(progress)(
          (request.root, request.worktreePath, request.task, request.runner, error)
        )
      yield Right(request)
    }

  def repairablePush[F[_]: Sync](
      progress: String => F[Unit]
  ): Kleisli[F, PushRequest, Unit] =
    RepairLoop(pushBranch, routePushFailure(progress), Kleisli[F, Throwable, Unit](Sync[F].raiseError))

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

  val RepairAllowedTools = Impl.ImplementerAllowedTools
  val MaxRepairBuildCheckAttempts = 3

  def repairAndCommit[F[_]](progress: String => F[Unit])(using
      F: Sync[F]
  ): Kleisli[F, (os.Path, os.Path, Issue, TaskRunner, Throwable), Unit] =
    Kleisli.apply { case (root, worktreePath, task, runner, pushError) =>
      repairAndCommitWith(progress)(
        worktreePath,
        task,
        runner,
        repairPrompt(task, pushError),
        s"Repair prePush failure for task #${task.number}",
        AgentInventory.load(root)
      )
    }

  /** Run a repair agent until the project validates, rotating runners on red.
    *
    * The retry was previously the same runner three times over. That is the defect `routeRunnerFallback` exists to
    * avoid, on a second route: a runner that could not fix the build on attempt one is not more likely to fix it on
    * attempt three, and each identical re-run is paid for. `alternateImplementor` with the attempted list is exactly
    * what `repairMergeConflictsUntilClean` already does one function below - the two repair paths now agree.
    *
    * The sequence is precomputed rather than chosen inside the loop because it does not depend on the error - which
    * makes it a pure function, and the only part of this effect-heavy loop that can be tested without a live agent.
    */
  def repairRunnerSequence(
      inventory: AgentInventory,
      first: TaskRunner,
      attempts: Int
  ): List[TaskRunner] =
    def build(current: TaskRunner, attempted: List[TaskRunner], remaining: Int): List[TaskRunner] =
      if remaining <= 0 then Nil
      else
        // No alternate left: repeat the current runner rather than abandon the
        // repair. That is what this loop did for every attempt before rotation.
        val next = inventory.alternateImplementor(current, attempted).getOrElse(current)
        current :: build(next, current :: attempted, remaining - 1)

    build(first, Nil, attempts.max(1))

  def repairAndCommitWith[F[_]](progress: String => F[Unit])(
      worktreePath: os.Path,
      task: Issue,
      runner: TaskRunner,
      prompt: AgentPrompt,
      commitMessage: String,
      inventory: AgentInventory
  )(using F: Sync[F]): F[Unit] =
    def loop(runner: TaskRunner, remainingRunners: List[TaskRunner]): F[Unit] =
      for
        _ <- progress(
          s"Running repair agent (${runner.display}) for task #${task.number}..."
        )
        _ <- AgentExecutor[F].run(
          runner,
          prompt,
          worktreePath,
          RepairAllowedTools,
          contextFiles = repairContextFiles(worktreePath),
          taskNumber = Some(task.number),
          metricsScope = AgentExecutor.MetricsScope.Repair,
          phase = TestEditGuard.phaseOf(task.body)
        )
        changed <- Impl.git[F].filesChanged(worktreePath)
        // Same false-green hole as the main execute path, on a second route: a
        // repair agent that weakens a test turns CI green and this commits it.
        // The repair agent works uncommitted, so the worktree alone is the diff.
        testEdits <- Impl
          .git[F]
          .modifiedOrDeletedInWorktree(worktreePath)
          .map(TestEditGuard.violations(TestEditGuard.phaseOf(task.body), _))
        _ <-
          if testEdits.nonEmpty then
            F.raiseError[Unit](TestEditGuard.Violation(TestEditGuard.phaseOf(task.body), testEdits))
          else if changed then
            Impl.git[F].runProjectValidation(worktreePath).attempt.flatMap {
              case Right(_) =>
                Impl.git[F].commitAll(worktreePath, task, Some(commitMessage))
              case Left(error) if remainingRunners.nonEmpty =>
                progress(
                  s"Repair build checks failed for task #${task.number}: ${error.getMessage}. Retrying repair with ${remainingRunners.head.display} (${remainingRunners.size} attempt(s) left)..."
                ) *> loop(remainingRunners.head, remainingRunners.tail)
              case Left(error) =>
                F.raiseError(error)
            }
          else
            // A repair that changed nothing is only a repair if there was nothing
            // to repair. Taken on trust it returns success to a caller that then
            // re-pushes the same unrepaired tree into the same rejection, burning
            // the repair budget without a single edit - which is what task #125
            // did on 2026-08-01. So ask the verifier: green means the tree was
            // already sound, red means this runner declined the job and the next
            // one gets it.
            Impl.git[F].runProjectValidation(worktreePath).attempt.flatMap {
              case Right(_) =>
                progress(
                  s"Repair agent made no file changes for task #${task.number}, and project checks pass. Nothing to repair."
                )
              case Left(error) if remainingRunners.nonEmpty =>
                progress(
                  s"Repair agent made no file changes for task #${task.number} but project checks still fail: ${error.getMessage}. Retrying repair with ${remainingRunners.head.display} (${remainingRunners.size} attempt(s) left)..."
                ) *> loop(remainingRunners.head, remainingRunners.tail)
              case Left(error) =>
                F.raiseError(error)
            }
      yield ()

    // +1: MaxRepairBuildCheckAttempts counts RETRIES after the first run, so the
    // sequence is one longer than the retry budget.
    repairRunnerSequence(inventory, runner, MaxRepairBuildCheckAttempts + 1) match
      case head :: tail => loop(head, tail)
      case Nil          => loop(runner, Nil)

  private def repairContextFiles(worktreePath: os.Path): Seq[String] =
    Seq(
      "scripts/git-pre-push.scala",
      "scripts/setup-git-hooks.scala",
      "scripts/git-pre-commit.scala",
      "build.mill",
      "build.sc",
      "build.sbt",
      "project.scala"
    ).filter(path => os.exists(worktreePath / os.RelPath(path)))

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

  def prCheckRepairPrompt(task: Issue, checkError: Throwable): AgentPrompt =
    AgentPrompt(
      s"""CI checks failed on the open Pull Request for task #${task.number} (${task.title}).
       |Fix the underlying issue in this worktree so the checks pass, without changing the
       |task's intended behavior. Do not run git push yourself.
       |
       |Failure output:
       |${checkError.getMessage}
       |""".stripMargin
    )
