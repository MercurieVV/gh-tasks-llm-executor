package com.github.mercurievv.ghllm

import com.github.mercurievv.ghllm.arrow.*

import arrowstep.core.ProgramSays
import cats.arrow.ArrowChoice
import cats.syntax.all.*

/** T23 runtime task-tree traversal mapping (Issue #72)
  *
  * The runtime traversals in this file are:
  *  1. `RecursiveArrows.executeRecursive` – dependency‑first recursion
  *     that short‑circuits on the first incomplete dependency, mutates
  *     `RunEnv.openIssues`, and discovers dependencies dynamically
  *     (including split children created between root passes).
  *  2. `UntilClosedArrows.runUntilClosed` – repeat‑until‑closed loop
  *     that repeats the whole root walk until no further progress is made.
  *  3. `TraversalArrows.runCandidate` – selects the single‑pass or
  *     repeat‑until‑closed path.
  *
  * None of these call sites can be ported to the shared `TaskF`/`TaskTree.Tree`
  * Droste algebra without breaking behaviour:
  *  - The dependency tree is not static; its members and order are computed
  *    at runtime and may change mid‑run when a task splits.
  *  - The short‑circuiting fold `ArrowTraverse.untilLeft` is list‑based
  *    and does not correspond to a pure fold over a statically‑known `Branch`.
  *  - The `runUntilClosed` loop is a temporal control loop, not a structural
  *    recursion over a tree node.
  *
  * Therefore all T23 call sites are classified as **intentionally outside
  * the task‑tree algebra**.  Future implementation leaves must not replace
  * them with Droste‑based walks against the existing `TaskF` without first
  * making the dependency computations static and removing the temporal repeat
  * — changes that would alter the observed behaviour and break existing tests.
  *
  * This conclusion preserves dependency order, first‑incomplete‑child
  * short‑circuiting, `RunEnv.openIssues` mutations, split‑parent replay,
  * and dynamic child discovery semantics.
  */
/** Top-level arrows that turn application input into a user-visible run summary.
  *
  * Leaves only. Every composition that spans more than this record lives on `BusinessLogic`, so that no arrow here has
  * to know how a candidate is actually executed.
  */
final case class ProgramArrows[-->[_, _]](
    resolveContext: AppInput --> RunContext,
    selectTask: RunContext --> TaskSelection,
    routeEmptySelection: TaskSelection --> Either[NoTask, TaskSelection],
    noTaskSummary: NoTask --> RunSummary,
    // Seeds RunEnv.openIssues for the run. Separate from selectTask because the
    // recursive walk mutates that map as issues close.
    loadOpenIssues: TaskSelection --> TaskSelection,
    // Left = run root candidates concurrently (--parallel), Right = one by one.
    routeParallelExecution: TaskSelection --> Either[TaskSelection, TaskSelection],
    // One candidate's uncaught failure must not abort the rest of the batch.
    recoverCandidateFailure: (TaskCandidate, Throwable) --> RunSummary,
    lastSummary: List[RunSummary] --> RunSummary,
    toProgramSays: RunSummary --> ProgramSays[ujson.Value]
)

/** Arrows for acquiring, evaluating, and routing one selected task. */
final case class TaskArrows[-->[_, _]](
    routeResumeOrRun: TaskCandidate --> Either[ClaimedTask, ClaimedTask],
    resumeExistingPullRequest: ExistingPullRequestResumeArrows[-->],
    announceTask: ClaimedTask --> ClaimedTask,
    fetchTaskContext: ClaimedTask --> PreparedTask,
    evaluateTask: PreparedTask --> Either[NeedsUserInput, Either[SplitTask, PreparedTask]],
    needsUserInputSummary: NeedsUserInput --> RunSummary,
    splitTaskSummary: SplitTask --> RunSummary,
    markTaskInProgress: PreparedTask --> PreparedTask,
    acquireWorktreeAndExecute: PreparedTask --> ClaimedTask,
    completedTaskSummary: ClaimedTask --> RunSummary
)

/** Arrows that decide whether agent output should become a commit.
  *
  * `publishChangedTask` is not a field here: publishing is `PublicationArrows`' job, and this group only knows how to
  * turn a changed task into a publish request and how to report a publication failure against the right issue. The
  * composition of the two is `BusinessLogic.publishChangedTask`.
  */
final case class ChangeArrows[-->[_, _]](
    classifyAgentResultForPublication: ExecutedTask -->
      Either[
        ChangedTask,
        UnchangedTask
      ],
    toPublishRequest: ChangedTask --> PublishRequest,
    reportPublicationFailure: (ChangedTask, Throwable) --> ExecutedTask,
    reportUnchangedTask: UnchangedTask --> ExecutedTask
)

/** Arrows that prepare changed or existing work and publish it locally or remotely.
  */
final case class PublicationArrows[-->[_, _]](
    classifyPublicationSource: PublishRequest -->
      Either[
        ChangedPublication,
        ExistingPublication
      ],
    prepareChangedPublication: ChangedPublication --> PublishRequest,
    prepareExistingPublication: ExistingPublication --> PublishRequest,
    choosePublicationTransport: PublishRequest -->
      Either[
        RemotePublication,
        LocalPublication
      ],
    publishRemote: PublishRemoteArrows[-->],
    publishLocal: LocalPublication --> Unit
):
  def publishChanges(using
      ArrowChoice[-->],
      ArrowDefer[-->],
      ArrowAttempt[-->]
  ): PublishRequest --> Unit =
    classifyPublicationSource >>>
      (prepareChangedPublication ||| prepareExistingPublication) >>>
      publishTransport

  def publishTransport(using
      ArrowChoice[-->],
      ArrowDefer[-->],
      ArrowAttempt[-->]
  ): PublishRequest --> Unit =
    choosePublicationTransport >>>
      (publishRemote.publishRemote ||| publishLocal)

/** Invoking the implementer agent for a prepared task. */
final case class AgentRunArrows[-->[_, _]](
    runTaskWithRunner: PreparedTask --> ExecutedTask
):
  def runAgent: PreparedTask --> ExecutedTask =
    runTaskWithRunner

/** Publishing a branch through the GitHub remote: push it, then open and merge its Pull Request. */
final case class PublishRemoteArrows[-->[_, _]](
    toPushRequest: RemotePublication --> PushRequest,
    pushBranch: PushRequest --> Unit,
    toPublishRequest: RemotePublication --> PublishRequest,
    createAndMergePullRequest: PublishRequest --> Unit
):
  def publishRemote(using
      arrow: ArrowChoice[-->]
  ): RemotePublication --> Unit =
    // `&&&` on this arrow is sequential, left before right (see ParallelArrows
    // for why, and for the concurrent variant): the push must land before the
    // Pull Request is opened against it.
    ((toPushRequest >>> pushBranch) &&& (toPublishRequest >>> createAndMergePullRequest)) >>> arrow.lift(_ => ())

/** Arrows for the already prepared task execution pipeline. */
final case class ExecuteTaskArrows[-->[_, _]](
    runAgent: AgentRunArrows[-->],
    runProjectValidation: ExecutedTask --> ExecutedTask,
    recordAgentOutput: ExecutedTask --> ExecutedTask,
    // Persist the durable "implemented" mark once the agent's output has been
    // committed and published, before CI verification and issue close. A crash
    // in that later window then resumes without re-invoking the implementer.
    markTaskImplemented: ExecutedTask --> ExecutedTask,
    verifyRelatedPullRequestCi: ExecutedTask --> ExecutedTask,
    closeTaskIssue: ExecutedTask --> ClaimedTask,
    checkParentsForCompletion: ClaimedTask --> ClaimedTask
)

/** Arrows for dependency-first recursive execution of GitHub task trees.
  *
  * All fields are plain arrows. The recursion is not a field: it is built here from `ArrowDefer.fix`, and the fold over
  * a task's dependencies from `ArrowTraverse.untilLeft`. Previously `runDependencies` was a field of type `(Issue -->
  * RunSummary) => (Issue --> Either[RunSummary, Issue])`, i.e. it took the recursive arrow as an argument - which put
  * `-->` in contravariant position and made the record unmappable by `Functor2K`, so it could be neither folded into
  * `BusinessLogic` nor covered by arrow logging.
  */
final case class RecursiveArrows[-->[_, _]](
    checkIfCompleted: TaskNode --> Either[RunSummary, TaskNode],
    // Dependencies and (for a split task) open children, in the order they
    // must close before the node itself may run.
    collectPendingDependencies: TaskNode --> DependencyPlan,
    // Left short-circuits the remaining dependencies: this one did not close.
    recordDependencyOutcome: (TaskNode, RunSummary) --> Either[RunSummary, Unit],
    routeDependencyOutcome: (DependencyPlan, Either[RunSummary, Unit]) --> Either[RunSummary, TaskNode],
    claimAndRun: TaskNode --> RunSummary
):
  def executeRecursive(using
      arrow: ArrowChoice[-->],
      defer: ArrowDefer[-->],
      traverse: ArrowTraverse[-->]
  ): TaskNode --> RunSummary =
    defer.fix { self =>
      checkIfCompleted >>>
        (arrow.id[RunSummary] |||
          (runDependencies(self) >>> (arrow.id[RunSummary] ||| claimAndRun)))
    }

  private def runDependencies(self: TaskNode --> RunSummary)(using
      arrow: ArrowChoice[-->],
      traverse: ArrowTraverse[-->]
  ): TaskNode --> Either[RunSummary, TaskNode] =
    val runOneDependency: TaskNode --> Either[RunSummary, Unit] =
      (arrow.id[TaskNode] &&& self) >>> recordDependencyOutcome
    val runPending: DependencyPlan --> Either[RunSummary, Unit] =
      arrow.lift((plan: DependencyPlan) => plan.pending) >>> traverse.untilLeft(runOneDependency)
    collectPendingDependencies >>>
      (arrow.id[DependencyPlan] &&& runPending) >>>
      routeDependencyOutcome

/** State threaded through the repeated root-closing walk: the candidate as last refreshed from GitHub, which iteration
  * this is, and the previous pass's summary (used to detect a pass that made no further progress). Carrying this in the
  * arrow's input/output, rather than in a closure over mutable state, is what lets `UntilClosedArrows.runUntilClosed`
  * express the repeat-until-closed loop as a self-referencing arrow.
  */
final case class RootWalk(
    candidate: TaskCandidate,
    iteration: Int,
    previous: Option[RunSummary]
)

/** Arrows for the repeated "walk a root's dependency tree until it closes" loop used under `--recursive`: a split
  * mid-tree creates new sub-issues a one-shot snapshot doesn't know about, so a single pass can short-circuit without
  * ever running them, and closing the tree means repeating the walk until a pass makes no further progress. Mirrors
  * `RecursiveArrows` above: `ArrowDefer.fix` breaks the eager `lazy val self` cycle, `refreshRoot` and
  * `routeContinuation` are the two decision points, fused with `|||`.
  */
final case class UntilClosedArrows[-->[_, _]](
    refreshRoot: RootWalk --> Either[RunSummary, RootWalk],
    runRootOnce: RootWalk --> (RootWalk, RunSummary),
    routeContinuation: (RootWalk, RunSummary) --> Either[RunSummary, RootWalk]
):
  def runUntilClosed(using
      arrow: ArrowChoice[-->],
      defer: ArrowDefer[-->]
  ): RootWalk --> RunSummary =
    defer.fix { self =>
      refreshRoot >>>
        (arrow.id[RunSummary] |||
          (runRootOnce >>> routeContinuation >>> (arrow.id[RunSummary] ||| self)))
    }

/** Arrows that drive one selected root candidate's dependency tree to completion: either a single dependency-first
  * pass, or (under `--recursive`) the repeated `UntilClosedArrows.runUntilClosed` walk. `routeRecursiveMode` is the
  * only place that decision is made; `runCandidate` fuses both branches with `|||` so callers never branch on
  * `Recursive` themselves.
  *
  * `BusinessLogic.executeSelectedCandidates` runs `runCandidate` over every selected root candidate; under `--parallel`
  * it does so concurrently via `ArrowTraverse.parAll` instead of the sequential `ArrowTraverse.all` - `runCandidate`
  * itself needs no change either way, since it is a plain `TaskCandidate --> RunSummary` arrow.
  */
final case class TraversalArrows[-->[_, _]](
    routeRecursiveMode: TaskCandidate --> Either[TaskCandidate, TaskCandidate],
    untilClosed: UntilClosedArrows[-->],
    runOnce: TaskCandidate --> RunSummary
):
  def runCandidate(using
      arrow: ArrowChoice[-->],
      defer: ArrowDefer[-->]
  ): TaskCandidate --> RunSummary =
    routeRecursiveMode >>>
      ((startWalk >>> untilClosed.runUntilClosed) ||| runOnce)

  private def startWalk(using arrow: ArrowChoice[-->]): TaskCandidate --> RootWalk =
    arrow.lift(candidate => RootWalk(candidate, iteration = 1, previous = None))
