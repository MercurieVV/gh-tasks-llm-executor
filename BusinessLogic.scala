import arrowstep.core.ProgramSays
import cats.arrow.ArrowChoice
import cats.syntax.all.*

final case class AppInput(root: os.Path, taskNumber: Option[Int])

final case class RunContext(
    root: os.Path,
    agentInventory: AgentInventory,
    taskNumber: Option[Int]
)

final case class TaskRunner(
    agent: String,
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
      prompt: String,
      allowedTools: Seq[String] = Nil,
      jsonSchema: Option[String] = None
  ): Seq[String] =
    agent match
      case "claude" =>
        Seq(agent) ++ model.toList.flatMap(value => Seq("--model", value)) ++
          (if allowedTools.isEmpty then Nil
           else Seq("--allowedTools") ++ allowedTools) ++
          jsonSchema.toList.flatMap(schema => Seq("--json-schema", schema)) ++
          Seq("-p", prompt)
      case "codex" =>
        val mappedModel = (model, effort) match
          case (Some("gpt-5") | Some("gpt-5-codex"), Some("medium")) =>
            Some("gpt-5.6-terra")
          case (Some("gpt-5") | Some("gpt-5-codex"), Some("high")) =>
            Some("gpt-5.6-sol")
          case (Some("gpt-5") | Some("gpt-5-codex"), Some("low")) =>
            Some("gpt-5.6-luna")
          case _ => model
        Seq(agent, "exec") ++
          mappedModel.toList.flatMap(value => Seq("--model", value)) ++
          effort.toList.flatMap(value =>
            Seq("--config", s"model_reasoning_effort=$value")
          ) ++
          Seq(prompt)
      case "aider" =>
        Seq(agent) ++ model.toList.flatMap(value => Seq("--model", value)) ++
          Seq("--yes-always", "--no-auto-commits", "--message", prompt)
      case _ =>
        Seq(agent) ++ model.toList.flatMap(value => Seq("-m", value)) ++
          Seq("-p", prompt)

final case class RunnableTask(
    context: RunContext,
    issue: Issue,
    runner: TaskRunner,
    // An open Pull Request for this task's branch already exists (from a
    // prior run that was interrupted before merging): resume by verifying
    // and merging it instead of re-running the implementer.
    resumePullRequest: Boolean = false
)

final case class TaskSelection(
    context: RunContext,
    candidates: List[RunnableTask]
)

final case class NoTask(context: RunContext)

final case class TaskRun(
    context: RunContext,
    task: Issue,
    runner: TaskRunner,
    worktreePath: os.Path,
    branchName: String,
    // Base to branch off of / merge into. A subtask of a split task
    // (see GitHub.parentIds) integrates into its parent's shared branch
    // instead of the default branch, so sibling subtasks land together in
    // one final merge (GitHub.checkParentsForCompletion) rather than each
    // hitting the default branch independently.
    baseBranch: Option[String]
)

final case class TaskWithPrompt(
    run: TaskRun,
    parentConclusion: Option[String],
    replayContext: Option[String]
)

final case class TaskWithOutput(run: TaskRun, output: String)

final case class NeedsUserInput(run: TaskRun, questions: String)

final case class SplitTask(run: TaskRun)

final case class TaskEvaluation(
    body: String,
    questions: Option[String],
    execution: String
)

final case class ExistingBranch(run: TaskWithPrompt)

final case class NewBranch(run: TaskWithPrompt)

final case class ChangedTask(run: TaskWithOutput)

final case class UnchangedTask(run: TaskWithOutput)

final case class AgentFinalization(
    commitTitle: Option[String],
    pullRequestBody: Option[String]
)

final case class PublishRequest(
    root: os.Path,
    worktreePath: os.Path,
    branchName: String,
    baseBranch: Option[String],
    task: Issue,
    finalization: AgentFinalization,
    runner: TaskRunner
)

final case class ChangedPublication(request: PublishRequest)

final case class ExistingPublication(request: PublishRequest)

final case class RemotePublication(request: PublishRequest)

final case class LocalPublication(request: PublishRequest)

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
              "number" -> ujson.Num(issue.number),
              "title" -> ujson.Str(issue.title),
              "state" -> ujson.Str(issue.state)
            )
          )
        }
      )
    )

final case class ProgramArrows[-->[_, _]](
    resolveContext: AppInput --> RunContext,
    selectTask: RunContext --> TaskSelection,
    partitionCandidates: TaskSelection --> Either[NoTask, TaskSelection],
    noTaskSummary: NoTask --> RunSummary,
    executeNonEmptySelection: TaskSelection --> RunSummary,
    toProgramSays: RunSummary --> ProgramSays[ujson.Value]
):
  def program(using ArrowChoice[-->]): AppInput --> ProgramSays[ujson.Value] =
    taskFlow >>> toProgramSays

  def taskFlow(using ArrowChoice[-->]): AppInput --> RunSummary =
    resolveContext >>> selectTask >>> executeTask

  def executeTask(using ArrowChoice[-->]): TaskSelection --> RunSummary =
    partitionCandidates >>> (noTaskSummary ||| executeNonEmptySelection)

final case class TaskArrows[-->[_, _]](
    resumePlan: RunnableTask --> Either[TaskRun, TaskRun],
    resumeExistingPullRequest: TaskRun --> RunSummary,
    announceTask: TaskRun --> TaskRun,
    fetchTaskContext: TaskRun --> TaskWithPrompt,
    evaluateTask: TaskWithPrompt -->
      Either[NeedsUserInput, Either[SplitTask, TaskWithPrompt]],
    needsUserInputSummary: NeedsUserInput --> RunSummary,
    splitTaskSummary: SplitTask --> RunSummary,
    markInProgress: TaskWithPrompt --> TaskWithPrompt,
    runPreparedTask: TaskWithPrompt --> TaskRun,
    completedTaskSummary: TaskRun --> RunSummary
):
  def resumeChoice(using ArrowChoice[-->]): RunnableTask --> RunSummary =
    resumePlan >>>
      (resumeExistingPullRequest ||| taskExecution)

  def taskExecution(using ArrowChoice[-->]): TaskRun --> RunSummary =
    announceTask >>>
      fetchTaskContext >>>
      evaluateTask >>>
      (needsUserInputSummary ||| (splitTaskSummary ||| executePreparedTask))

  def executePreparedTask(using
      ArrowChoice[-->]
  ): TaskWithPrompt --> RunSummary =
    markInProgress >>> runPreparedTask >>> completedTaskSummary

final case class ChangeArrows[-->[_, _]](
    changedPlan: TaskWithOutput --> Either[ChangedTask, UnchangedTask],
    commitChangedTask: ChangedTask --> TaskWithOutput,
    reportUnchangedTask: UnchangedTask --> TaskWithOutput
):
  def commitIfChanged(using ArrowChoice[-->]): TaskWithOutput -->
    TaskWithOutput =
    changedPlan >>>
      (commitChangedTask ||| reportUnchangedTask)

final case class PublicationArrows[-->[_, _]](
    publicationPlan: PublishRequest -->
      Either[
        ChangedPublication,
        ExistingPublication
      ],
    prepareChangedPublication: ChangedPublication --> PublishRequest,
    prepareExistingPublication: ExistingPublication --> PublishRequest,
    publishTransportPlan: PublishRequest -->
      Either[
        RemotePublication,
        LocalPublication
      ],
    publishRemote: RemotePublication --> Unit,
    publishLocal: LocalPublication --> Unit
):
  def publishChanges(using ArrowChoice[-->]): PublishRequest --> Unit =
    publicationPlan >>>
      (prepareChangedPublication ||| prepareExistingPublication) >>>
      publishTransport

  def publishTransport(using ArrowChoice[-->]): PublishRequest --> Unit =
    publishTransportPlan >>>
      (publishRemote ||| publishLocal)

final case class PreparedTaskArrows[-->[_, _]](
    runExecutor: TaskWithPrompt --> TaskWithOutput,
    runProjectValidation: TaskWithOutput --> TaskWithOutput,
    commentOutput: TaskWithOutput --> TaskWithOutput,
    verifyReplayCi: TaskWithOutput --> TaskWithOutput,
    closeTask: TaskWithOutput --> TaskRun
)

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

final case class BusinessLogic[-->[_, _]](
    programArrows: ProgramArrows[-->],
    taskArrows: TaskArrows[-->],
    changeArrows: ChangeArrows[-->],
    publicationArrows: PublicationArrows[-->],
    preparedTaskArrows: PreparedTaskArrows[-->]
):
  def executeAcquiredTask(using ArrowChoice[-->]): TaskWithPrompt --> TaskRun =
    preparedTaskArrows.runExecutor >>>
      preparedTaskArrows.runProjectValidation >>>
      preparedTaskArrows.commentOutput >>>
      changeArrows.commitIfChanged >>>
      preparedTaskArrows.verifyReplayCi >>>
      preparedTaskArrows.closeTask

object BusinessLogic:
  given Functor2K[BusinessLogic] = Functor2K.derived
  given Semigroup2K[BusinessLogic] = Semigroup2K.derived
