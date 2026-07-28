package com.github.mercurievv.ghllm

import com.github.mercurievv.ghllm.*
import com.github.mercurievv.ghllm.agent.*
import com.github.mercurievv.ghllm.arrow.*
import com.github.mercurievv.ghllm.cli.*
import com.github.mercurievv.ghllm.git.*
import com.github.mercurievv.ghllm.metrics.*

import arrowstep.core.ProgramSays
import cats.arrow.ArrowChoice
import cats.syntax.all.*

import scala.util.Try

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

/** Concrete agent invocation choice, including optional model, effort, and version.
  */
final case class TaskRunner(
    agent: AgentBinary,
    model: Option[String],
    effort: Option[String],
    version: Option[String]
):
  def display: String =
    val modelPart = model.fold("")(value => s", model: $value")
    val effortPart = effort.fold("")(value => s", effort: $value")
    val versionPart = version.fold("")(value => s", version: $value")
    s"agent: $agent$modelPart$effortPart$versionPart"

  def command(
      prompt: AgentPrompt,
      allowedTools: Seq[String] = Nil,
      jsonSchema: Option[String] = None,
      cwd: Option[os.Path] = None,
      contextFiles: Seq[String] = Nil
  ): Seq[String] =
    val promptForRun = effectivePrompt(prompt, allowedTools, cwd)
    agent.value match
      case "claude" =>
        val mcpConfig = workspaceFile(cwd, os.rel / ".agents" / "mcp_config.json")
        val effectiveAllowedTools =
          allowedTools ++ mcpConfig.toList.flatMap(_ => ScalaSemanticClaudeTools)
        Seq(agent.value) ++ model.toList.flatMap(value => Seq("--model", value)) ++
          mcpConfig.toList.flatMap(path => Seq("--mcp-config", path.toString)) ++
          (if effectiveAllowedTools.isEmpty then Nil
           else Seq("--allowedTools") ++ effectiveAllowedTools) ++
          jsonSchema.toList.flatMap(schema => Seq("--json-schema", schema)) ++
          Seq("-p", promptForRun.value)
      case "codex" =>
        val mappedModel = (model, effort) match
          case (Some("gpt-5") | Some("gpt-5-codex"), Some("medium")) =>
            Some("gpt-5.6-terra")
          case (Some("gpt-5") | Some("gpt-5-codex"), Some("high")) =>
            Some("gpt-5.6-sol")
          case (Some("gpt-5") | Some("gpt-5-codex"), Some("low")) =>
            Some("gpt-5.6-luna")
          case _ => model
        Seq(agent.value, "exec") ++
          mappedModel.toList.flatMap(value => Seq("--model", value)) ++
          effort.toList.flatMap(value => Seq("--config", s"model_reasoning_effort=$value")) ++
          codexMcpConfigArgs(cwd) ++
          Seq(promptForRun.value)
      case "aider" =>
        // DeepSeek retired deepseek-chat/deepseek-reasoner in favor of
        // deepseek-v4-flash/deepseek-v4-pro. Runner ids/model fields keep the
        // old names so previously-persisted task metadata still resolves via
        // AgentInventory.selectRunner; only the invoked API model changes.
        val mappedModel = model match
          case Some("deepseek/deepseek-chat")     => Some("deepseek/deepseek-v4-flash")
          case Some("deepseek/deepseek-reasoner") => Some("deepseek/deepseek-v4-pro")
          case other                              => other
        Seq(agent.value) ++ mappedModel.toList.flatMap(value => Seq("--model", value)) ++
          Seq("--yes-always", "--no-auto-commits", "--message", promptForRun.value) ++
          contextFiles
      case "gemini" =>
        Seq(agent.value) ++ model.toList.flatMap(value => Seq("-m", value)) ++
          Seq("-p", promptForRun.value)
      case "agy" =>
        Seq(agent.value) ++ model.toList.flatMap(value => Seq("--model", value)) ++
          effort.toList.flatMap(value => Seq("--effort", value)) ++
          Seq("--print", promptForRun.value)
      case _ =>
        Seq(agent.value) ++ model.toList.flatMap(value => Seq("-m", value)) ++
          Seq("-p", promptForRun.value)

  def effectivePrompt(
      prompt: AgentPrompt,
      allowedTools: Seq[String] = Nil,
      cwd: Option[os.Path] = None
  ): AgentPrompt =
    if shouldInjectScalaSemanticInstruction(allowedTools, cwd) &&
      !prompt.value.contains(ScalaSemanticInstructionHeader)
    then AgentPrompt(s"$ScalaSemanticInstruction\n\n${prompt.value}")
    else prompt

  private def workspaceFile(cwd: Option[os.Path], path: os.RelPath): Option[os.Path] =
    cwd.map(_ / path).filter(os.exists(_))

  private def shouldInjectScalaSemanticInstruction(
      allowedTools: Seq[String],
      cwd: Option[os.Path]
  ): Boolean =
    Set("claude", "codex").contains(agent.value) &&
      allowedTools.nonEmpty &&
      workspaceFile(cwd, os.rel / ".agents" / "mcp_config.json").nonEmpty

  private def codexMcpConfigArgs(cwd: Option[os.Path]): Seq[String] =
    workspaceFile(cwd, os.rel / ".agents" / "mcp_config.json").toList.flatMap { path =>
      val servers =
        for
          json <- Try(ujson.read(os.read(path))).toOption
          servers <- json.obj.get("mcpServers").map(_.obj)
        yield servers.toSeq.flatMap { case (name, server) =>
          val obj = server.obj
          val command = obj.get("command").map(_.str).toSeq.flatMap { value =>
            Seq("--config", s"mcp_servers.$name.command=${tomlString(value)}")
          }
          val args = obj.get("args").map(_.arr.map(_.str).toSeq).toSeq.flatMap { values =>
            Seq("--config", s"mcp_servers.$name.args=${tomlStringArray(values)}")
          }
          command ++ args
        }
      servers.getOrElse(Nil)
    }

  private def tomlString(value: String): String = ujson.write(value)

  private def tomlStringArray(values: Seq[String]): String =
    values.map(tomlString).mkString("[", ",", "]")

  private val ScalaSemanticInstructionHeader =
    "ScalaSemantic MCP requirement:"

  private val ScalaSemanticInstruction =
    s"""$ScalaSemanticInstructionHeader
       |- Before inspecting or editing Scala source, call the ScalaSemantic MCP tools.
       |- Use `set_workspace_root` for the current worktree first.
       |- Use `annotated_source` to read `.scala` files and semantic tools such as `find_symbol`, `find_usages`, `type_at_position`, `method_signature`, `members`, `class_hierarchy`, `resolve_implicits`, or `call_path` for Scala code questions.
       |- Do not use shell text tools such as `cat`, `sed`, `rg`, or `grep` to inspect `.scala` source unless ScalaSemantic MCP is unavailable or failing; if that happens, state the failure in your final answer.
       |""".stripMargin

  private val ScalaSemanticClaudeTools = Seq(
    "mcp__scala-semantic__annotated_source",
    "mcp__scala-semantic__set_workspace_root",
    "mcp__scala-semantic__refresh_workspace",
    "mcp__scala-semantic__smart_code_duplications",
    "mcp__scala-semantic__batch_rename_plan",
    "mcp__scala-semantic__find_symbol",
    "mcp__scala-semantic__find_usages",
    "mcp__scala-semantic__class_hierarchy",
    "mcp__scala-semantic__method_signature",
    "mcp__scala-semantic__find_overloads",
    "mcp__scala-semantic__members",
    "mcp__scala-semantic__resolve_implicits",
    "mcp__scala-semantic__trace_implicit_chain",
    "mcp__scala-semantic__call_path",
    "mcp__scala-semantic__type_at_position",
    "mcp__scala-semantic__document_outline",
    "mcp__scala-semantic__rename_plan",
    "mcp__scala-semantic__move_plan",
    "mcp__scala-semantic__extract_method_plan",
    "mcp__scala-semantic__value_flow"
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
    replayContext: Option[String]
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

/** State of the push repair loop: what to push, and for whom to repair it. */
final case class PushRequest(
    worktreePath: os.Path,
    branchName: BranchName,
    task: Issue,
    runner: TaskRunner
)

/** State of the Pull Request resume loop.
  *
  * `checksRepairAttemptsRemaining` bounds it: an agent that cannot actually fix the failing check would otherwise retry
  * forever, burning an agent run on every resume of this task. Merge-conflict repairs do not consume the budget - they
  * make measurable progress or fail outright.
  */
final case class PullRequestResume(
    run: ClaimedTask,
    checksRepairAttemptsRemaining: Int
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
final case class TaskNode(context: RunContext, issue: Issue)

/** What must close before a task node may run, plus whether any of it came from the node being an already split parent
  * (which means the node should be replayed on a later pass instead of implemented now).
  */
final case class DependencyPlan(
    node: TaskNode,
    pending: List[TaskNode],
    hasOpenChildren: Boolean
)

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

/** Arrows for acquiring, evaluating, and running one selected task.
  *
  * Leaves and the resume group only. `executeCandidate` and friends moved to `BusinessLogic`: resuming a Pull Request
  * needs `closeTaskIssue`/`checkParentsForCompletion`, which live in `ExecuteTaskArrows`, and no record should reach
  * into another.
  */
final case class TaskArrows[-->[_, _]](
    routeResumeOrRun: TaskCandidate --> Either[ClaimedTask, ClaimedTask],
    resumeTask: ResumeTaskArrows[-->],
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

/** Invoking the implementer agent for a prepared task.
  *
  * Was a single `runAgent` leaf whose body held failure handling. Retry selection is now an `ArrowChoice` branch.
  */
final case class AgentRunArrows[-->[_, _]](
    runTaskWithRunner: PreparedTask --> ExecutedTask,
    // Left = no stronger runner left, give up. Right = the same task re-aimed
    // at a stronger implementer, to be attempted once more.
    routeRunnerFallback: (PreparedTask, Throwable) --> Either[Throwable, PreparedTask],
    raiseRunnerFailure: Throwable --> ExecutedTask
):
  def runAgent(using ArrowChoice[-->], ArrowAttempt[-->]): PreparedTask --> ExecutedTask =
    runImplementer

  def runImplementer(using
      arrow: ArrowChoice[-->],
      attempt: ArrowAttempt[-->]
  ): PreparedTask --> ExecutedTask =
    attempt.attempt(runTaskWithRunner) >>>
      (retryWithStrongerRunner ||| arrow.lift(_._2))

  // Exactly one escalation: the fallback runner is not itself retried.
  private def retryWithStrongerRunner(using
      arrow: ArrowChoice[-->]
  ): (PreparedTask, Throwable) --> ExecutedTask =
    routeRunnerFallback >>> (raiseRunnerFailure ||| runTaskWithRunner)

/** Driving an existing open Pull Request to merged, repairing what blocks it.
  *
  * Was `resumeOpenPullRequestWithConflictRepair`, a `def` that called itself from inside `handleErrorWith` with a
  * hand-carried attempt budget. The budget now lives in `PullRequestResume`, i.e. in the loop's state, the same way
  * `RootWalk` carries the root-walk's.
  */
final case class ResumePullRequestArrows[-->[_, _]](
    startResume: ClaimedTask --> PullRequestResume,
    resumePullRequest: PullRequestResume --> Unit,
    // Merge conflict -> resolve and retry with the budget intact; failing
    // checks -> run the repair agent, push, retry with one fewer attempt;
    // anything else (or budget exhausted) -> Left.
    routeResumeFailure: (PullRequestResume, Throwable) --> Either[Throwable, PullRequestResume],
    raiseResumeFailure: Throwable --> Unit
):
  def resumeUntilMerged(using
      ArrowChoice[-->],
      ArrowDefer[-->],
      ArrowAttempt[-->]
  ): ClaimedTask --> Unit =
    startResume >>> RepairLoop(resumePullRequest, routeResumeFailure, raiseResumeFailure)

/** Completing a task from a Pull Request an earlier, interrupted run left open. */
final case class ResumeTaskArrows[-->[_, _]](
    resume: ResumePullRequestArrows[-->],
    announceResume: ClaimedTask --> ClaimedTask,
    resumedExecution: ClaimedTask --> ExecutedTask,
    cleanupAndSummarize: ClaimedTask --> RunSummary,
    // Left = the Pull Request turned out to be gone; fall back to an ordinary
    // run. Right = a real failure to report and re-raise.
    routeResumeError: (ClaimedTask, Throwable) --> Either[ClaimedTask, (ClaimedTask, Throwable)],
    announceNoPullRequest: ClaimedTask --> ClaimedTask,
    reportResumeFailure: (ClaimedTask, Throwable) --> RunSummary
)

/** Publishing a branch through the GitHub remote: push it, then open and merge its Pull Request.
  *
  * Both steps are the same repair loop at different actions - a rejected `git push` (usually the repo's prePush hook)
  * is repaired by an agent and retried, a rejected merge by resolving the conflict against the base branch. They were
  * two separate self-recursive `def`s.
  */
final case class PublishRemoteArrows[-->[_, _]](
    toPushRequest: RemotePublication --> PushRequest,
    pushBranch: PushRequest --> Unit,
    routePushFailure: (PushRequest, Throwable) --> Either[Throwable, PushRequest],
    raisePushFailure: Throwable --> Unit,
    toPublishRequest: RemotePublication --> PublishRequest,
    createAndMergePullRequest: PublishRequest --> Unit,
    routeMergeFailure: (PublishRequest, Throwable) --> Either[Throwable, PublishRequest],
    raiseMergeFailure: Throwable --> Unit
):
  def publishRemote(using
      arrow: ArrowChoice[-->],
      defer: ArrowDefer[-->],
      attempt: ArrowAttempt[-->]
  ): RemotePublication --> Unit =
    // `&&&` on this arrow is sequential, left before right (see ParallelArrows
    // for why, and for the concurrent variant): the push must land before the
    // Pull Request is opened against it.
    (pushUntilAccepted &&& mergeUntilMerged) >>> arrow.lift(_ => ())

  def pushUntilAccepted(using
      ArrowChoice[-->],
      ArrowDefer[-->],
      ArrowAttempt[-->]
  ): RemotePublication --> Unit =
    toPushRequest >>> RepairLoop(pushBranch, routePushFailure, raisePushFailure)

  def mergeUntilMerged(using
      ArrowChoice[-->],
      ArrowDefer[-->],
      ArrowAttempt[-->]
  ): RemotePublication --> Unit =
    toPublishRequest >>> RepairLoop(createAndMergePullRequest, routeMergeFailure, raiseMergeFailure)

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
    val candidates = arrow.lift((selection: TaskSelection) => selection.candidates)
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
    val resume = taskArrows.resumeTask
    val recover =
      resume.routeResumeError >>>
        ((resume.announceNoPullRequest >>> executeClaimedTask) ||| resume.reportResumeFailure)
    attempt.attempt(resumeAndClose) >>> (recover ||| arrow.lift(_._2))

  private def resumeAndClose(using
      arrow: ArrowChoice[-->],
      defer: ArrowDefer[-->],
      attempt: ArrowAttempt[-->]
  ): ClaimedTask --> RunSummary =
    val resume = taskArrows.resumeTask
    val merged: ClaimedTask --> ClaimedTask =
      (resume.resume.resumeUntilMerged &&& arrow.id[ClaimedTask]) >>> arrow.lift(_._2)
    resume.announceResume >>>
      merged >>>
      resume.resumedExecution >>>
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
  given Functor2K[BusinessLogic] = Functor2K.derived
  given Monoid2K[BusinessLogic] = Monoid2K.derived
