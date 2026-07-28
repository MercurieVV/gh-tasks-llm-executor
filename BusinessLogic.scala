package com.github.mercurievv.ghllm

import com.github.mercurievv.ghllm.*
import com.github.mercurievv.ghllm.agent.*
import com.github.mercurievv.ghllm.arrow.*

import arrowstep.core.ProgramSays
import cats.arrow.ArrowChoice
import cats.syntax.all.*

import scala.util.Try

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
  given Functor2K[BusinessLogic] = Functor2K.derived
  given Apply2K[BusinessLogic] = Apply2K.derived
  given Monoid2K[BusinessLogic] = Monoid2K.derived
