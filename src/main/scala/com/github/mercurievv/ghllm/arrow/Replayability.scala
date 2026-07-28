package com.github.mercurievv.ghllm.arrow

import com.github.mercurievv.ghllm.*
import com.github.mercurievv.ghllm.cli.*
import com.github.mercurievv.ghllm.git.*

import cats.Monad
import cats.MonadThrow
import cats.data.Kleisli
import cats.data.OptionT
import cats.effect.kernel.Sync
import cats.syntax.all.*

object Replayability:
  type ReplayFlow[F[_]] = [A, B] =>> Kleisli[[X] =>> OptionT[F, X], A, B]

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
