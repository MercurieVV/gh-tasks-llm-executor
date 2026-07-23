import arrowstep.core.ProgramSays
import cats.arrow.ArrowChoice
import cats.syntax.all.*

/** CLI agent binary name, distinct from a runner's model and effort options. */
opaque type AgentBinary = String
object AgentBinary:
  def apply(value: String): AgentBinary = value
  extension (self: AgentBinary) def value: String = self

/** Git branch name used for task worktrees and publication targets. */
opaque type BranchName = String
object BranchName:
  def apply(value: String): BranchName = value.asInstanceOf[BranchName]
  extension (opaqueValue: BranchName)
    def value: String = opaqueValue.asInstanceOf[String]
  given cats.Eq[BranchName] = cats.Eq.by(_.value)

/** Whether dependency tasks should be claimed and executed before the task
  * itself.
  */
opaque type Recursive = Boolean
object Recursive:
  def apply(value: Boolean): Recursive = value
  extension (self: Recursive) def value: Boolean = self

/** GitHub issue or sub-issue number selected for a run. */
opaque type TaskNumber = Int
object TaskNumber:
  def apply(value: Int): TaskNumber = value.asInstanceOf[TaskNumber]
  extension (opaqueValue: TaskNumber)
    def value: Int = opaqueValue.asInstanceOf[Int]
  given cats.Eq[TaskNumber] = cats.Eq.by(_.value)

/** Raw command-line input before configuration and runner inventory are loaded.
  */
final case class AppInput(
    root: os.Path,
    taskNumber: Option[TaskNumber],
    recursive: Recursive = Recursive(false)
)

/** Resolved execution context shared by all tasks in the current invocation. */
final case class RunContext(
    root: os.Path,
    agentInventory: AgentInventory,
    taskNumber: Option[TaskNumber],
    recursive: Recursive = Recursive(false)
)

/** Concrete agent invocation choice, including optional model, effort, and
  * version.
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
      jsonSchema: Option[String] = None
  ): Seq[String] =
    agent.value match
      case "claude" =>
        Seq(agent.value) ++ model.toList.flatMap(value =>
          Seq("--model", value)
        ) ++
          (if allowedTools.isEmpty then Nil
           else Seq("--allowedTools") ++ allowedTools) ++
          jsonSchema.toList.flatMap(schema => Seq("--json-schema", schema)) ++
          Seq("-p", prompt.value)
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
          effort.toList.flatMap(value =>
            Seq("--config", s"model_reasoning_effort=$value")
          ) ++
          Seq(prompt.value)
      case "aider" =>
        Seq(agent.value) ++ model.toList.flatMap(value =>
          Seq("--model", value)
        ) ++
          Seq("--yes-always", "--no-auto-commits", "--message", prompt.value)
      case _ =>
        Seq(agent.value) ++ model.toList.flatMap(value => Seq("-m", value)) ++
          Seq("-p", prompt.value)

/** Candidate issue paired with the runner selected to execute or resume it. */
final case class TaskCandidate(
    context: RunContext,
    issue: Issue,
    runner: TaskRunner,
    // An open Pull Request for this task's branch already exists (from a
    // prior run that was interrupted before merging): resume by verifying
    // and merging it instead of re-running the implementer.
    resumePullRequest: Boolean = false
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
    run: ClaimedTask,
    parentConclusion: Option[String],
    replayContext: Option[String]
)

/** Task after agent execution has produced terminal output to inspect and
  * publish.
  */
final case class ExecutedTask(run: ClaimedTask, output: AgentOutput)

/** Evaluation result for a task that cannot continue without a human answer. */
final case class NeedsUserInput(run: ClaimedTask, questions: String)

/** Evaluation result for a task that should be decomposed into smaller
  * subtasks.
  */
final case class SplitTask(run: ClaimedTask, replayed: Boolean = false)

/** Parsed planning decision from the evaluator before execution routing. */
final case class TaskEvaluation(
    body: IssueBody,
    questions: Option[String],
    execution: String
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

/** Publication path for worktree changes that still need commit preparation. */
final case class ChangedPublication(request: PublishRequest)

/** Publication path for an already prepared branch or pull request. */
final case class ExistingPublication(request: PublishRequest)

/** Publication transport that pushes, opens, or merges through the GitHub
  * remote.
  */
final case class RemotePublication(request: PublishRequest)

/** Publication transport that stops after local branch preparation. */
final case class LocalPublication(request: PublishRequest)

/** Final machine-readable result emitted by the program. */
final case class RunSummary(
    status: String,
    message: String,
    task: Option[Issue]
):
  def toJson: ujson.Value =
    ujson.Obj.from(
      Seq(
        "status" -> ujson.Str(status),
        "message" -> ujson.Str(message),
        "task" -> task.fold[ujson.Value](ujson.Null) { issue =>
          ujson.Obj.from(
            Seq(
              "number" -> ujson.Num(issue.number.value),
              "title" -> ujson.Str(issue.title.value),
              "state" -> ujson.Str(issue.state)
            )
          )
        }
      )
    )

/** Top-level arrows that turn application input into a user-visible run
  * summary.
  */
final case class ProgramArrows[-->[_, _]](
    resolveContext: AppInput --> RunContext,
    selectTask: RunContext --> TaskSelection,
    routeEmptySelection: TaskSelection --> Either[NoTask, TaskSelection],
    noTaskSummary: NoTask --> RunSummary,
    executeSelectedCandidates: TaskSelection --> RunSummary,
    toProgramSays: RunSummary --> ProgramSays[ujson.Value]
):
  def program(using ArrowChoice[-->]): AppInput --> ProgramSays[ujson.Value] =
    taskFlow >>> toProgramSays

  def taskFlow(using ArrowChoice[-->]): AppInput --> RunSummary =
    resolveContext >>> selectTask >>> executeTask

  def executeTask(using ArrowChoice[-->]): TaskSelection --> RunSummary =
    routeEmptySelection >>> (noTaskSummary ||| executeSelectedCandidates)

/** Arrows for acquiring, evaluating, and running one selected task. */
final case class TaskArrows[-->[_, _]](
    routeResumeOrRun: TaskCandidate --> Either[ClaimedTask, ClaimedTask],
    resumeExistingPullRequest: ClaimedTask --> RunSummary,
    announceTask: ClaimedTask --> ClaimedTask,
    fetchTaskContext: ClaimedTask --> PreparedTask,
    evaluateTask: PreparedTask -->
      Either[NeedsUserInput, Either[SplitTask, PreparedTask]],
    needsUserInputSummary: NeedsUserInput --> RunSummary,
    splitTaskSummary: SplitTask --> RunSummary,
    markTaskInProgress: PreparedTask --> PreparedTask,
    acquireWorktreeAndExecute: PreparedTask --> ClaimedTask,
    completedTaskSummary: ClaimedTask --> RunSummary
):
  def executeCandidate(using ArrowChoice[-->]): TaskCandidate --> RunSummary =
    routeResumeOrRun >>>
      (resumeExistingPullRequest ||| executeClaimedTask)

  def executeClaimedTask(using ArrowChoice[-->]): ClaimedTask --> RunSummary =
    announceTask >>>
      fetchTaskContext >>>
      evaluateTask >>>
      (needsUserInputSummary ||| (splitTaskSummary ||| executePreparedTaskAndSummarize))

  def executePreparedTaskAndSummarize(using
      ArrowChoice[-->]
  ): PreparedTask --> RunSummary =
    markTaskInProgress >>> acquireWorktreeAndExecute >>> completedTaskSummary

/** Arrows that decide whether agent output should become a commit. */
final case class ChangeArrows[-->[_, _]](
    classifyAgentResultForPublication: ExecutedTask -->
      Either[
        ChangedTask,
        UnchangedTask
      ],
    publishChangedTask: ChangedTask --> ExecutedTask,
    reportUnchangedTask: UnchangedTask --> ExecutedTask
):
  def commitIfChanged(using ArrowChoice[-->]): ExecutedTask --> ExecutedTask =
    classifyAgentResultForPublication >>>
      (publishChangedTask ||| reportUnchangedTask)

/** Arrows that prepare changed or existing work and publish it locally or
  * remotely.
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
    publishRemote: RemotePublication --> Unit,
    publishLocal: LocalPublication --> Unit
):
  def publishChanges(using ArrowChoice[-->]): PublishRequest --> Unit =
    classifyPublicationSource >>>
      (prepareChangedPublication ||| prepareExistingPublication) >>>
      publishTransport

  def publishTransport(using ArrowChoice[-->]): PublishRequest --> Unit =
    choosePublicationTransport >>>
      (publishRemote ||| publishLocal)

/** Arrows for the already prepared task execution pipeline. */
final case class PreparedTaskArrows[-->[_, _]](
    runAgent: PreparedTask --> ExecutedTask,
    runProjectValidation: ExecutedTask --> ExecutedTask,
    recordAgentOutput: ExecutedTask --> ExecutedTask,
    verifyReplayCi: ExecutedTask --> ExecutedTask,
    closeTaskIssue: ExecutedTask --> ClaimedTask
)

/** Arrows for dependency-first recursive execution of GitHub task trees. */
final case class RecursiveArrows[-->[_, _]](
    checkIfCompleted: Issue --> Either[RunSummary, Issue],
    runDependencies: (Issue --> RunSummary) => (Issue -->
      Either[RunSummary, Issue]),
    claimAndRun: Issue --> RunSummary,
    defer: (=> Issue --> RunSummary) => (Issue --> RunSummary)
):
  def executeRecursive(using ArrowChoice[-->]): Issue --> RunSummary =
    lazy val self: Issue --> RunSummary =
      checkIfCompleted >>> (summon[ArrowChoice[-->]]
        .id[RunSummary] ||| (runDependencies(
        defer(self)
      ) >>> (summon[ArrowChoice[-->]].id[RunSummary] ||| claimAndRun)))
    self

/** Complete business workflow assembled from independently testable arrow
  * groups.
  */
final case class BusinessLogic[-->[_, _]](
    programArrows: ProgramArrows[-->],
    taskArrows: TaskArrows[-->],
    changeArrows: ChangeArrows[-->],
    publicationArrows: PublicationArrows[-->],
    preparedTaskArrows: PreparedTaskArrows[-->]
):
  def executePreparedTaskInWorktree(using ArrowChoice[-->]): PreparedTask -->
    ClaimedTask =
    preparedTaskArrows.runAgent >>>
      preparedTaskArrows.runProjectValidation >>>
      preparedTaskArrows.recordAgentOutput >>>
      changeArrows.commitIfChanged >>>
      preparedTaskArrows.verifyReplayCi >>>
      preparedTaskArrows.closeTaskIssue

object BusinessLogic:
  given Functor2K[BusinessLogic] = Functor2K.derived
  given Semigroup2K[BusinessLogic] = Semigroup2K.derived
