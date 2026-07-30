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
    val candidates = arrow.lift(BusinessLogic.markCachePeers)
    val isolated = runCandidateIsolated
    programArrows.loadOpenIssues >>>
      programArrows.routeParallelExecution >>>
      ((candidates >>> traverse.parAll(isolated)) |||
        (candidates >>> traverse.all(isolated))) >>>
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

object BusinessLogic:
  /** Marks the candidates that should write their shared prefix with the extended (1-hour) cache TTL — T20.
    *
    * The TTL costs roughly 2x to write and pays back over about 3 reads, so it is set only where at least 3 candidates
    * share the same (agent, model): a peer group of 2 would pay the premium and never earn it back.
    *
    * Independent of `--parallel`. Peers do not have to run at the same time — an hour-long window is precisely what
    * decouples the readers from the writer, and three same-runner roots share the constant prompt segments (agent
    * boundary, answer contract, workflow) whether they run side by side or one after another. This used to be gated on
    * `--parallel`, which meant an ordinary sequential batch paid full price for a prefix it re-sent every time.
    *
    * Also groups same-`(agent, model)` candidates adjacently, which is the half of T20 that applies to everyone the TTL
    * does not: a group below `MinPeers` and a runner whose CLI exposes no TTL knob both still get the provider's default
    * 5-minute window, and that window survives `A A B B` but not `A B A B`. Ordering stays stable inside and between
    * groups, so this reorders the queue without reprioritising it.
    */
  def markCachePeers(selection: TaskSelection): List[TaskCandidate] =
    // Grouped before marking, not after, so the marks travel with the candidates
    // rather than being zipped onto a list that has since been reordered.
    val ordered =
      CachePeers.groupAdjacent(selection.candidates)(c => (c.runner.agent, c.runner.model))
    val qualifying =
      CachePeers.qualifying(ordered.map(c => (c.runner.agent, c.runner.model)))
    ordered.zip(qualifying).map { case (candidate, earnsTtl) =>
      candidate.copy(runner = candidate.runner.copy(extendedCacheTtl = earnsTtl))
    }

  given Functor2K[BusinessLogic] = Functor2K.derived
  given Apply2K[BusinessLogic] = Apply2K.derived
  given Monoid2K[BusinessLogic] = Monoid2K.derived
