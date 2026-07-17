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
        Seq(agent, "exec") ++
          model.toList.flatMap(value => Seq("--model", value)) ++
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
    finalization: AgentFinalization
)

final case class ChangedPublication(request: PublishRequest)

final case class ExistingPublication(request: PublishRequest)

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

final case class BusinessLogic[ArrowK[_, _]](
    resolveContext: ArrowK[AppInput, RunContext],
    selectTask: ArrowK[RunContext, TaskSelection],
    executeTask: ArrowK[TaskSelection, RunSummary],
    resumeExistingPullRequest: ArrowK[TaskRun, RunSummary],
    announceTask: ArrowK[TaskRun, TaskRun],
    fetchTaskContext: ArrowK[TaskRun, TaskWithPrompt],
    evaluateTask: ArrowK[
      TaskWithPrompt,
      Either[NeedsUserInput, Either[SplitTask, TaskWithPrompt]]
    ],
    needsUserInputSummary: ArrowK[NeedsUserInput, RunSummary],
    splitTaskSummary: ArrowK[SplitTask, RunSummary],
    runPreparedTask: ArrowK[TaskWithPrompt, TaskRun],
    completedTaskSummary: ArrowK[TaskRun, RunSummary],
    changedPlan: ArrowK[TaskWithOutput, Either[ChangedTask, UnchangedTask]],
    commitChangedTask: ArrowK[ChangedTask, TaskWithOutput],
    reportUnchangedTask: ArrowK[UnchangedTask, TaskWithOutput],
    publicationPlan: ArrowK[
      PublishRequest,
      Either[ChangedPublication, ExistingPublication]
    ],
    commitChangedPublication: ArrowK[ChangedPublication, Unit],
    publishExistingPublication: ArrowK[ExistingPublication, Unit],
    toProgramSays: ArrowK[RunSummary, ProgramSays[ujson.Value]]
)(using arrowChoice: ArrowChoice[ArrowK]):

  def program: ArrowK[AppInput, ProgramSays[ujson.Value]] =
    taskFlow >>> toProgramSays

  def taskFlow: ArrowK[AppInput, RunSummary] =
    resolveContext >>> selectTask >>> executeTask

  def resumeChoice: ArrowK[Either[TaskRun, TaskRun], RunSummary] =
    arrowChoice.choice(resumeExistingPullRequest, taskExecution)

  def taskExecution: ArrowK[TaskRun, RunSummary] =
    announceTask >>>
      fetchTaskContext >>>
      evaluateTask >>>
      arrowChoice.choice(
        needsUserInputSummary,
        arrowChoice.choice(splitTaskSummary, executePreparedTask)
      )

  def executePreparedTask: ArrowK[TaskWithPrompt, RunSummary] =
    runPreparedTask >>> completedTaskSummary

  def commitIfChanged: ArrowK[TaskWithOutput, TaskWithOutput] =
    changedPlan >>>
      arrowChoice.choice(commitChangedTask, reportUnchangedTask)

  def publishChanges: ArrowK[PublishRequest, Unit] =
    publicationPlan >>>
      arrowChoice.choice(commitChangedPublication, publishExistingPublication)
