package com.github.mercurievv.ghllm.arrow

import com.github.mercurievv.ghllm.*
import com.github.mercurievv.ghllm.cli.*
import com.github.mercurievv.ghllm.git.*
import com.github.mercurievv.ghllm.metrics.*

import cats.Monad
import cats.MonadThrow
import cats.arrow.ArrowChoice
import cats.data.Kleisli
import cats.data.OptionT
import cats.effect.kernel.Sync
import cats.syntax.all.*

/** Driving an existing open Pull Request to merged. */
final case class PullRequestResumeArrows[-->[_, _]](
    resumeOpenPullRequest: ClaimedTask --> Unit
):
  def mergeExistingPullRequest: ClaimedTask --> Unit =
    resumeOpenPullRequest

/** Completing a task from a Pull Request an earlier, interrupted run left open. */
final case class ExistingPullRequestResumeArrows[-->[_, _]](
    pullRequest: PullRequestResumeArrows[-->],
    announceResume: ClaimedTask --> ClaimedTask,
    toResumedExecution: ClaimedTask --> ExecutedTask,
    cleanupAndSummarize: ClaimedTask --> RunSummary,
    // Left = the Pull Request turned out to be gone; fall back to an ordinary
    // run. Right = a real failure to report and re-raise.
    routeResumeError: (ClaimedTask, Throwable) --> Either[ClaimedTask, (ClaimedTask, Throwable)],
    announceNoPullRequest: ClaimedTask --> ClaimedTask,
    reportResumeFailure: (ClaimedTask, Throwable) --> RunSummary
)

object Replayability:
  type ReplayFlow[F[_]] = [A, B] =>> Kleisli[[X] =>> OptionT[F, X], A, B]

  def resumeExistingPullRequest[-->[_, _]](
      resume: ExistingPullRequestResumeArrows[-->]
  )(using
      arrow: ArrowChoice[-->]
  ): ClaimedTask --> ExecutedTask = {
    val mergeExistingPullRequest: ClaimedTask --> ClaimedTask =
      ((resume.pullRequest.mergeExistingPullRequest &&& arrow.id[ClaimedTask]) >>> arrow.lift(_._2))
    resume.announceResume >>>
      mergeExistingPullRequest >>>
      resume.toResumedExecution
  }

  given replayFlowMonoid[F[_]: Monad]: Monoid2[ReplayFlow[F]] with
    def empty[A, B]: ReplayFlow[F][A, B] =
      Kleisli(_ => OptionT.none[F, B])

    def combine[A, B](
        replay: ReplayFlow[F][A, B],
        real: ReplayFlow[F][A, B]
    ): ReplayFlow[F][A, B] =
      Kleisli { input =>
        OptionT {
          replay.run(input).value.flatMap {
            case cached @ Some(_) => cached.pure[F]
            case None             => real.run(input).value
          }
        }
      }

  def lift[F[_]: Monad](
      real: BusinessLogic[Flow[F]]
  ): BusinessLogic[ReplayFlow[F]] =
    Functor2K[BusinessLogic].mapK(real)(
      [A, B] => (arrow: Flow[F][A, B]) => Kleisli((input: A) => OptionT.liftF(arrow.run(input)))
    )

  def combine[F[_]: Monad](
      replay: BusinessLogic[ReplayFlow[F]],
      real: BusinessLogic[Flow[F]]
  ): BusinessLogic[ReplayFlow[F]] =
    Monoid2K[BusinessLogic].combineK(replay, lift(real))

  def fetchTaskReplayContext[F[_]: Sync](
      progress: String => F[Unit]
  ): ReplayFlow[F][ClaimedTask, PreparedTask] =
    Kleisli { run =>
      OptionT {
        GitHub
          .replayContext(progress)(run.context.root, run.task)
          .flatMap {
            case Some(replayContext) =>
              GitHub
                .dependencyConclusion(progress)(run.context.root, run.task)
                .map(parentConclusion =>
                  Some(
                    PreparedTask(
                      run,
                      parentConclusion = parentConclusion,
                      replayContext = Some(replayContext)
                    )
                  )
                )
            case None => none[PreparedTask].pure[F]
          }
      }
    }

  def runAlreadyImplemented[F[_]: Sync](
      progress: String => F[Unit],
      hasOriginBranch: Flow[F][(os.Path, BranchName), Boolean]
  ): ReplayFlow[F][PreparedTask, ExecutedTask] =
    Kleisli { task =>
      OptionT {
        alreadyImplemented(progress, hasOriginBranch)(task).flatMap {
          case Some(branch) =>
            progress(
              s"Task #${task.claimedTask.task.number} already implemented on branch $branch " +
                s"(durable mark + reachable work); skipping implementer ${task.claimedTask.runner.display}."
            ).as(Some(ExecutedTask(task.claimedTask, AgentOutput(""))))
          case None => none[ExecutedTask].pure[F]
        }
      }
    }

  def splitTaskSummary[F[_]: Sync]: ReplayFlow[F][SplitTask, RunSummary] =
    Kleisli { split =>
      OptionT.fromOption[F](
        Option.when(split.replayed)(
          RunSummary(
            status = Status("split"),
            message = Message5(
              s"Task #${split.run.task.number} was evaluated for splitting and will not be implemented directly."
            ),
            task = Some(split.run.task)
          )
        )
      )
    }

  def routeResumeOrRun[F[_]: Sync](
      progress: String => F[Unit]
  ): ReplayFlow[F][TaskCandidate, Either[ClaimedTask, ClaimedTask]] =
    Kleisli { task =>
      val run = Impl.taskRun(task.context, task.issue, task.runner)
      OptionT {
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
            .map(hasOpenPr => Option.when(hasOpenPr)(Left(run)))
        else none[Either[ClaimedTask, ClaimedTask]].pure[F]
      }
    }

  def resumePullRequest[F[_]: Sync](
      progress: String => F[Unit]
  ): ReplayFlow[F][ClaimedTask, Unit] =
    Kleisli { run =>
      OptionT.liftF {
        GitHub.resumeOpenPullRequest(progress)(
          run.context.root,
          run.branchName
        )
      }
    }

  def announceResume[F[_]: Sync]: ReplayFlow[F][ClaimedTask, ClaimedTask] =
    liftFlow {
      TaskLogger.progress(run =>
        s"Task #${run.task.number} already has an open Pull Request for ${run.branchName}; resuming to verify and merge instead of re-implementing..."
      )
    }

  def toResumedExecution[F[_]: Monad]: ReplayFlow[F][ClaimedTask, ExecutedTask] =
    Kleisli(run => OptionT.some[F](ExecutedTask(run, AgentOutput(""))))

  def cleanupAndSummarize[F[_]: Sync](
      progress: String => F[Unit]
  ): ReplayFlow[F][ClaimedTask, RunSummary] =
    Kleisli { completedRun =>
      OptionT.liftF {
        Git[F](progress)
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
    }

  def routeResumeError[F[_]: Monad]: ReplayFlow[F][
    (ClaimedTask, Throwable),
    Either[ClaimedTask, (ClaimedTask, Throwable)]
  ] =
    Kleisli {
      case (run, _: GitHub.NoOpenPullRequestToResumeException) => OptionT.some[F](Left(run))
      case (run, error)                                        => OptionT.some[F](Right((run, error)))
    }

  def announceNoPullRequest[F[_]: Sync]: ReplayFlow[F][ClaimedTask, ClaimedTask] =
    liftFlow {
      TaskLogger.progress(run =>
        s"No open Pull Request remains for ${run.branchName}; creating a new run instead of resuming."
      )
    }

  def reportResumeFailure[F[_]: Sync](
      progress: String => F[Unit]
  ): ReplayFlow[F][(ClaimedTask, Throwable), RunSummary] =
    Kleisli { case (run, error) =>
      OptionT.liftF {
        GitHub.commentTaskFailure(progress)(
          run.context.root,
          run.task,
          error.getMessage
        ) *> Sync[F].raiseError(error)
      }
    }

  private def liftFlow[F[_]: Monad, A, B](arrow: Flow[F][A, B]): ReplayFlow[F][A, B] =
    Kleisli(input => OptionT.liftF(arrow.run(input)))

  private def alreadyImplemented[F[_]: Sync](
      progress: String => F[Unit],
      hasOriginBranch: Flow[F][(os.Path, BranchName), Boolean]
  )(task: PreparedTask): F[Option[String]] =
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
                else hasOriginBranch.run((run.context.root, run.branchName))
            yield Option.when(reachable)(branch)
      }

  def lowerOrRaise[F[_]: MonadThrow](
      replayable: BusinessLogic[ReplayFlow[F]]
  ): BusinessLogic[Flow[F]] =
    Functor2K[BusinessLogic].mapK(replayable)(
      [A, B] =>
        (arrow: ReplayFlow[F][A, B]) =>
          Kleisli { input =>
            arrow
              .run(input)
              .getOrElseF(
                MonadThrow[F].raiseError(
                  RuntimeException("Replayable and real business logic both returned no result.")
                )
              )
        }
    )
