package com.github.mercurievv.ghllm

import com.github.mercurievv.ghllm.arrow.*

import arrowstep.core.ProgramSays
import cats.arrow.ArrowChoice
import cats.syntax.all.*

/** Complete business workflow assembled from independently testable arrow groups.
  *
  * Every arrow the program runs is reachable from here as a value, and every composition spanning more than one group
  * is a method on this class - so the shape of the program can be read without opening a single implementation, and
  * `withArrowLogging` sees all of it.
  */
final case class BusinessLogic[-->[_, _]](
    programArrows: ProgramArrows[-->],
    taskArrows: TaskArrows[-->],
    changeArrows: ChangeArrows[-->],
    publicationArrows: PublicationArrows[-->],
    executeTaskArrows: ExecuteTaskArrows[-->],
    recursiveArrows: RecursiveArrows[-->],
    traversalArrows: TraversalArrows[-->]
):
  def program(using
      ArrowChoice[-->],
      ArrowDefer[-->],
      ArrowTraverse[-->],
      ArrowAttempt[-->]
  ): AppInput --> ProgramSays[ujson.Value] =
    taskFlow >>> programArrows.toProgramSays

  def taskFlow(using
      ArrowChoice[-->],
      ArrowDefer[-->],
      ArrowTraverse[-->],
      ArrowAttempt[-->]
  ): AppInput --> RunSummary =
    programArrows.resolveContext >>> programArrows.selectTask >>> executeTask

  def executeTask(using
      ArrowChoice[-->],
      ArrowDefer[-->],
      ArrowTraverse[-->],
      ArrowAttempt[-->]
  ): TaskSelection --> RunSummary =
    programArrows.routeEmptySelection >>>
      (programArrows.noTaskSummary ||| executeSelectedCandidates)

  /** Runs every selected root candidate, concurrently or one by one. The `--parallel` decision is
    * `routeParallelExecution >>> (... ||| ...)`, not an `if` inside an effect: both branches are the same
    * `runCandidate` arrow at two `ArrowTraverse` strategies.
    */
  def executeSelectedCandidates(using
      arrow: ArrowChoice[-->],
      defer: ArrowDefer[-->],
      traverse: ArrowTraverse[-->],
      attempt: ArrowAttempt[-->]
  ): TaskSelection --> RunSummary =
    val isolated = runCandidateIsolated

    programArrows.loadOpenIssues >>>
      programArrows.routeParallelExecution >>>
      Kleisli { (selection: TaskSelection) =>
        val batchSize = selection.candidates.size
        Impl.setCurrentBatchSize(batchSize)

        val candidates = selection.candidates
        val isolatedWithBatch = isolated.lmap[TaskCandidate] { candidate =>
          Impl.setCurrentBatchSize(batchSize)
          candidate
        }

        if (selection.context.parallelExecution.value)
          traverse.parAll(isolatedWithBatch).run(candidates)
        else
          traverse.all(isolatedWithBatch).run(candidates)
      } >>>
      programArrows.lastSummary

  def runCandidate(using ArrowChoice[-->], ArrowDefer[-->]): TaskCandidate --> RunSummary =
    traversalArrows.runCandidate

  // Wraps runCandidate so one candidate's uncaught failure becomes a RunSummary
  // for that candidate instead of aborting every other candidate in the batch
  // (traverse.all/parAll propagate a raised error across the whole list).
  def runCandidateIsolated(using
      arrow: ArrowChoice[-->],
      defer: ArrowDefer[-->],
      attempt: ArrowAttempt[-->]
  ): TaskCandidate --> RunSummary =
    attempt.attempt(runCandidate) >>>
      (programArrows.recoverCandidateFailure ||| arrow.lift(_._2))

  def executeRecursive(using
      ArrowChoice[-->],
      ArrowDefer[-->],
      ArrowTraverse[-->]
  ): TaskNode --> RunSummary =
    recursiveArrows.executeRecursive

  def executeCandidate(using
      ArrowChoice[-->],
      ArrowDefer[-->],
      ArrowAttempt[-->]
  ): TaskCandidate --> RunSummary =
    taskArrows.routeResumeOrRun >>> (resumeExistingPullRequest ||| executeClaimedTask)

  def executeClaimedTask(using
      ArrowChoice[-->],
      ArrowDefer[-->],
      ArrowAttempt[-->]
  ): ClaimedTask --> RunSummary =
    taskArrows.announceTask >>>
      taskArrows.fetchTaskContext >>>
      taskArrows.evaluateTask >>>
      (taskArrows.needsUserInputSummary |||
        (taskArrows.splitTaskSummary ||| executePreparedTaskAndSummarize))

  def executePreparedTaskAndSummarize(using ArrowChoice[-->]): PreparedTask --> RunSummary =
    taskArrows.markTaskInProgress >>>
      taskArrows.acquireWorktreeAndExecute >>>
      taskArrows.completedTaskSummary

  /** Completes a task whose implementer already ran in an earlier, interrupted invocation and left an open Pull
    * Request: drive that Pull Request to merged, then close the task exactly as a fresh run would.
    *
    * If the Pull Request turns out to be gone after all, the fallback is `executeClaimedTask` - the ordinary run
    * pipeline, referenced rather than re-spelled.
    */
  def resumeExistingPullRequest(using
      arrow: ArrowChoice[-->],
      defer: ArrowDefer[-->],
      attempt: ArrowAttempt[-->]
  ): ClaimedTask --> RunSummary =
    val resume = taskArrows.resumeExistingPullRequest
    val resumePullRequestAndCloseTask =
      Replayability.resumeExistingPullRequest(resume) >>> closeResumedTask
    val recover =
      resume.routeResumeError >>>
        ((resume.announceNoPullRequest >>> executeClaimedTask) ||| resume.reportResumeFailure)
    attempt.attempt(resumePullRequestAndCloseTask) >>> (recover ||| arrow.lift(_._2))

  private def closeResumedTask(using ArrowChoice[-->]): ExecutedTask --> RunSummary =
    val resume = taskArrows.resumeExistingPullRequest
    executeTaskArrows.closeTaskIssue >>>
      executeTaskArrows.checkParentsForCompletion >>>
      resume.cleanupAndSummarize

  def commitIfChanged(using
      ArrowChoice[-->],
      ArrowDefer[-->],
      ArrowAttempt[-->]
  ): ExecutedTask --> ExecutedTask =
    changeArrows.classifyAgentResultForPublication >>>
      (publishChangedTask ||| changeArrows.reportUnchangedTask)

  def publishChangedTask(using
      arrow: ArrowChoice[-->],
      defer: ArrowDefer[-->],
      attempt: ArrowAttempt[-->]
  ): ChangedTask --> ExecutedTask =
    attempt.attempt(changeArrows.toPublishRequest >>> publicationArrows.publishChanges) >>>
      (changeArrows.reportPublicationFailure ||| arrow.lift(_._1.run))

  def executePreparedTaskInWorktree(using
      ArrowChoice[-->],
      ArrowDefer[-->],
      ArrowAttempt[-->]
  ): PreparedTask --> ClaimedTask =
    executeTaskArrows.runAgent.runAgent >>>
      executeTaskArrows.runProjectValidation >>>
      executeTaskArrows.recordAgentOutput >>>
      commitIfChanged >>>
      executeTaskArrows.markTaskImplemented >>>
      executeTaskArrows.verifyRelatedPullRequestCi >>>
      executeTaskArrows.closeTaskIssue >>>
      executeTaskArrows.checkParentsForCompletion

  // Kleisli convenience
  private def Kleisli[F]: cats.data.Kleisli = cats.data.Kleisli
  private type Kleisli[F, A, B] = cats.data.Kleisli[F, A, B]

object BusinessLogic:
  given Functor2K[BusinessLogic] = Functor2K.derived
  given Apply2K[BusinessLogic] = Apply2K.derived
  given Monoid2K[BusinessLogic] = Monoid2K.derived
