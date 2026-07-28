package com.github.mercurievv.ghllm.arrow

import com.github.mercurievv.ghllm.*
import com.github.mercurievv.ghllm.agent.*
import com.github.mercurievv.ghllm.cli.Flow
import com.github.mercurievv.ghllm.git.*

import cats.data.Kleisli
import cats.effect.kernel.Sync
import cats.syntax.all.*

import scala.concurrent.duration.*

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
          runTaskWithRunner = retryRunTaskWithRunner(progress)
        )
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
      routeFallback: Flow[F][(PreparedTask, Throwable), Either[Throwable, PreparedTask]],
      raiseFailure: Flow[F][Throwable, ExecutedTask]
  )(
      runTaskWithRunner: Flow[F][PreparedTask, ExecutedTask]
  ): Flow[F][PreparedTask, ExecutedTask] =
    Kleisli { task =>
      runTaskWithRunner.run(task).attempt.flatMap {
        case Right(result) => result.pure[F]
        case Left(error) =>
          routeFallback.run((task, error)).flatMap {
            case Left(finalError) => raiseFailure.run(finalError)
            case Right(fallbackTask) =>
              runTaskWithRunner.run(fallbackTask)
          }
      }
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

  def routeRunnerFallback[F[_]: Sync](
      progress: String => F[Unit]
  ): Flow[F][(PreparedTask, Throwable), Either[Throwable, PreparedTask]] =
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
        autoMerged <- Impl
          .git[F]
          .mergeBaseBranch(
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
              conflictedFiles <- Impl
                .git[F]
                .unresolvedConflictFiles(
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
              stillConflicted <- Impl
                .git[F]
                .hasUnresolvedConflicts(
                  request.worktreePath
                )
              resolved <-
                if stillConflicted then
                  progress(
                    s"Repair agent left unresolved conflicts for task #${request.task.number}; aborting merge."
                  ) *> Impl.git[F].abortMerge(request.worktreePath).as(false)
                else
                  Impl
                    .git[F]
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

  def pushBranch[F[_]: Sync]: Flow[F][PushRequest, Unit] =
    Kleisli(request => Impl.git[F].push(request.worktreePath, request.branchName))

  def routePushFailure[F[_]: Sync](
      progress: String => F[Unit]
  ): Flow[F][(PushRequest, Throwable), Either[Throwable, PushRequest]] =
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
        contextFiles = repairContextFiles(worktreePath),
        taskNumber = Some(task.number),
        metricsScope = "repair"
      )
      changed <- Impl.git[F].filesChanged(worktreePath)
      _ <-
        if changed then Impl.git[F].commitAll(worktreePath, task, Some(commitMessage))
        else progress(s"Repair agent made no file changes for task #${task.number}.")
    yield ()

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
