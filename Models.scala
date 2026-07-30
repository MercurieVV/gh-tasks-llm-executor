package com.github.mercurievv.ghllm

import com.github.mercurievv.ghllm.agent.*
import com.github.mercurievv.ghllm.git.*

opaque type Questions = String
object Questions:
  def apply(value: String): Questions = value
  extension (self: Questions) def value: String = self

opaque type Status = String
object Status:
  def apply(value: String): Status = value
  extension (self: Status) def value: String = self

opaque type Message5 = String
object Message5:
  def apply(value: String): Message5 = value
  extension (self: Message5) def value: String = self

opaque type ResumePullRequest = Boolean
object ResumePullRequest:
  def apply(value: Boolean): ResumePullRequest = value
  extension (self: ResumePullRequest) def value: Boolean = self

/** Evaluator's execution verdict: what should happen to a task next. */
enum Execution:
  case Implement, Split, NeedsInput

  /** Canonical string persisted in issue "Execution:" metadata. */
  def wireValue: String = this match
    case Execution.Implement  => "implement"
    case Execution.Split      => "split"
    case Execution.NeedsInput => "needs-input"

object Execution:
  /** Parse loose evaluator/metadata text; unknown -> NeedsInput (safe block). */
  def fromString(value: String): Execution =
    value.trim.toLowerCase match
      case "ready" | "implement" => Execution.Implement
      case "split"               => Execution.Split
      case _                     => Execution.NeedsInput

/** CLI agent binary name, distinct from a runner's model and effort options. */
opaque type AgentBinary = String
object AgentBinary:
  def apply(value: String): AgentBinary = value
  extension (self: AgentBinary) def value: String = self

/** Git branch name used for task worktrees and publication targets. */
opaque type BranchName = String
object BranchName:
  def apply(value: String): BranchName = value.asInstanceOf[BranchName]
  extension (opaqueValue: BranchName) def value: String = opaqueValue.asInstanceOf[String]
  given cats.Eq[BranchName] = cats.Eq.by(_.value)

/** Whether dependency tasks should be claimed and executed before the task itself.
  */
opaque type Recursive = Boolean
object Recursive:
  def apply(value: Boolean): Recursive = value
  extension (self: Recursive) def value: Boolean = self

/** Whether independent root task candidates should run concurrently instead of one after another. See `ParallelArrows`
  * for the arrow-level `***`/`&&&` combinators this enables.
  */
opaque type ParallelExecution = Boolean
object ParallelExecution:
  def apply(value: Boolean): ParallelExecution = value
  extension (self: ParallelExecution) def value: Boolean = self

/** GitHub issue or sub-issue number selected for a run. */
opaque type TaskNumber = Int
object TaskNumber:
  def apply(value: Int): TaskNumber = value.asInstanceOf[TaskNumber]
  extension (opaqueValue: TaskNumber) def value: Int = opaqueValue.asInstanceOf[Int]
  given cats.Eq[TaskNumber] = cats.Eq.by(_.value)

/** Raw command-line input before configuration and runner inventory are loaded.
  */
final case class AppInput(
    root: os.Path,
    taskNumber: Option[TaskNumber],
    recursive: Recursive = Recursive(false),
    parallelExecution: ParallelExecution = ParallelExecution(false)
)

/** Resolved execution context shared by all tasks in the current invocation. */
final case class RunContext(
    root: os.Path,
    agentInventory: AgentInventory,
    taskNumber: Option[TaskNumber],
    recursive: Recursive = Recursive(false),
    parallelExecution: ParallelExecution = ParallelExecution(false)
)

/** Candidate issue paired with the runner selected to execute or resume it. */
final case class TaskCandidate(
    context: RunContext,
    issue: Issue,
    runner: TaskRunner,
    // An open Pull Request for this task's branch already exists (from a
    // prior run that was interrupted before merging): resume by verifying
    // and merging it instead of re-running the implementer.
    resumePullRequest: ResumePullRequest = ResumePullRequest(false)
)

/** Non-empty or potentially empty set of runnable task candidates. */
final case class TaskSelection(
    context: RunContext,
    candidates: List[TaskCandidate]
)

/** Marker used when no task candidate remains after selection and filtering. */
final case class NoTask(context: RunContext)

/** Claimed task with the local worktree and branch prepared for execution. */
final case class ClaimedTask(
    context: RunContext,
    task: Issue,
    runner: TaskRunner,
    worktreePath: os.Path,
    branchName: BranchName,
    // Base to branch off of / merge into. A subtask of a split task
    // (see GitHub.parentIds) integrates into its parent's shared branch
    // instead of the default branch, so sibling subtasks land together in
    // one final merge (GitHub.checkParentsForCompletion) rather than each
    // hitting the default branch independently.
    baseBranch: Option[BranchName]
)

/** Prepared task after prompt/context collection, before the agent is executed.
  */
final case class PreparedTask(
    claimedTask: ClaimedTask,
    parentConclusion: Option[String],
    replayContext: Option[String],
    // How many times this leaf has already been escalated to a stronger runner.
    // Bounded by BusinessLogicRetry.MaxEscalationDepth: past that, a task that
    // keeps failing is a task no runner in the ladder can do, and further
    // escalation only burns the most expensive tool available.
    escalationDepth: Int = 0
)

/** Task after agent execution has produced terminal output to inspect and publish.
  */
final case class ExecutedTask(run: ClaimedTask, output: AgentOutput)

/** Evaluation result for a task that cannot continue without a human answer. */
final case class NeedsUserInput(run: ClaimedTask, questions: Questions)

/** Evaluation result for a task that should be decomposed into smaller subtasks.
  */
final case class SplitTask(run: ClaimedTask, replayed: Boolean = false)

/** Parsed planning decision from the evaluator before execution routing. */
final case class TaskEvaluation(
    body: IssueBody,
    questions: Option[String],
    execution: Execution
)

/** Existing branch path for a prepared task that should reuse prior local work.
  */
final case class ExistingBranch(run: PreparedTask)

/** New branch path for a prepared task that needs a fresh worktree branch. */
final case class NewBranch(run: PreparedTask)

/** Agent output that changed the worktree and must be committed. */
final case class ChangedTask(run: ExecutedTask)

/** Agent output that left the worktree unchanged and should only be reported.
  */
final case class UnchangedTask(run: ExecutedTask)

/** Agent-provided publication metadata for commit and pull request text. */
final case class AgentFinalization(
    commitTitle: Option[String],
    pullRequestBody: Option[String]
)

/** All inputs needed to publish a completed task from its worktree branch. */
final case class PublishRequest(
    root: os.Path,
    worktreePath: os.Path,
    branchName: BranchName,
    baseBranch: Option[BranchName],
    task: Issue,
    finalization: AgentFinalization,
    runner: TaskRunner
)

/** Remote branch push request. */
final case class PushRequest(
    worktreePath: os.Path,
    branchName: BranchName,
    task: Issue,
    runner: TaskRunner
)

/** Publication path for worktree changes that still need commit preparation. */
final case class ChangedPublication(request: PublishRequest)

/** Publication path for an already prepared branch or pull request. */
final case class ExistingPublication(request: PublishRequest)

/** Publication transport that pushes, opens, or merges through the GitHub remote.
  */
final case class RemotePublication(request: PublishRequest)

/** Publication transport that stops after local branch preparation. */
final case class LocalPublication(request: PublishRequest)

/** Final machine-readable result emitted by the program. */
final case class RunSummary(
    status: Status,
    message: Message5,
    task: Option[Issue]
):
  def toJson: ujson.Value =
    ujson.Obj.from(
      Seq(
        "status" -> ujson.Str(status.value),
        "message" -> ujson.Str(message.value),
        "task" -> task.fold[ujson.Value](ujson.Null) { issue =>
          ujson.Obj.from(
            Seq(
              "number" -> ujson.Num(issue.number.value),
              "title" -> ujson.Str(issue.title.value),
              "state" -> ujson.Str(issue.state.value)
            )
          )
        }
      )
    )

/** A task issue together with the run context it was reached from.
  *
  * The recursive walk descends from issue to issue, so the context has to travel with it. It used to be captured in a
  * closure instead, which is what forced `RecursiveArrows` to be constructed per run rather than wired once.
  */
/** A task in the dependency walk.
  *
  * `extendedCacheTtl` is set when this node is one of >= 3 siblings in the same fan-out that will route to the same
  * runner, so the first of them writes the shared prompt prefix with the 1-hour TTL and the rest read it (T20). It
  * rides on the node rather than being recomputed at claim time because peer-ness is a property of the SET of
  * siblings, which a single node cannot see.
  */
final case class TaskNode(
    context: RunContext,
    issue: Issue,
    extendedCacheTtl: Boolean = false
)

/** What must close before a task node may run, plus whether any of it came from the node being an already split parent
  * (which means the node should be replayed on a later pass instead of implemented now).
  */
final case class DependencyPlan(
    node: TaskNode,
    pending: List[TaskNode],
    hasOpenChildren: Boolean
)
