package com.github.mercurievv.ghllm.arrow

import com.github.mercurievv.ghllm.*
import com.github.mercurievv.ghllm.cli.Flow

import cats.data.Kleisli
import cats.effect.kernel.Sync

object BusinessLogicReplay:

  def apply[F[_]: Sync](
      progress: String => F[Unit],
      evaluatorRunner: TaskRunner,
      waitForUserInput: Kleisli[F, (PreparedTask, String), EvaluationArrows.Result],
      hasOriginBranch: Flow[F][(os.Path, BranchName), Boolean]
  ): BusinessLogic[Replayability.ReplayFlow[F]] =
    type ReplayArr = Replayability.ReplayFlow[F]
    import Replayability.given

    val empty = Monoid2K[BusinessLogic].emptyK[ReplayArr]
    val evaluationArrows = EvaluationArrows.replay[F](
      progress = progress,
      evaluatorRunner = evaluatorRunner,
      waitForUserInput = waitForUserInput
    )

    empty.copy(
      taskArrows = empty.taskArrows.copy(
        routeResumeOrRun = Replayability.routeResumeOrRun(progress),
        resumeExistingPullRequest = ExistingPullRequestResumeArrows[ReplayArr](
          pullRequest = PullRequestResumeArrows[ReplayArr](
            resumeOpenPullRequest = Replayability.resumePullRequest(progress)
          ),
          announceResume = Replayability.announceResume,
          toResumedExecution = Replayability.toResumedExecution,
          cleanupAndSummarize = Replayability.cleanupAndSummarize(progress),
          routeResumeError = Replayability.routeResumeError,
          announceNoPullRequest = Replayability.announceNoPullRequest,
          reportResumeFailure = Replayability.reportResumeFailure(progress)
        ),
        fetchTaskContext = Replayability.fetchTaskReplayContext(progress),
        evaluateTask = evaluationArrows.evaluateTask,
        splitTaskSummary = Replayability.splitTaskSummary
      ),
      executeTaskArrows = empty.executeTaskArrows.copy(
        runAgent = empty.executeTaskArrows.runAgent.copy(
          runTaskWithRunner = Replayability.runAlreadyImplemented(
            progress,
            hasOriginBranch
          )
        )
      )
    )
