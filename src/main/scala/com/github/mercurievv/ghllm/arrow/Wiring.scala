package com.github.mercurievv.ghllm.arrow

import com.github.mercurievv.ghllm.*
import com.github.mercurievv.ghllm.arrow.*
import com.github.mercurievv.ghllm.cli.*
import com.github.mercurievv.ghllm.git.*
import com.github.mercurievv.ghllm.metrics.*

import cats.data.Kleisli
import cats.effect.kernel.Async
import cats.effect.kernel.Sync
import cats.effect.kernel.implicits.parallelForGenSpawn
import cats.syntax.all.*
import io.github.mercurievv.minuscles.fieldsnames.derivation.semiauto.FieldNamesDerivation.fieldsNames

import ArrowLogging.*

/** Assembly. The only place that knows both the shape of the program (`BusinessLogic.scala`) and its leaves
  * (`Implementations.scala`).
  *
  * There is no logic here beyond naming which leaf fills which slot: no `if`, no effect, no composition. Composition
  * belongs to the arrow records; this is the wire loom.
  *
  * A few slots are cyclic - a leaf that has to re-enter the program it is part of: claim a task then run the whole
  * candidate pipeline, acquire a worktree then run the whole execution pipeline inside it, walk a dependency tree that
  * descends into itself, wait for a user's answer and then evaluate again. Those leaves take the continuation as a
  * parameter and get it here as an `ArrowDefer.defer` reference to the fully assembled, fully logged program. That
  * replaces the old arrangement, where such leaves called `businessLogic[F].something.run(x)` in their own body -
  * rebuilding and re-instrumenting the entire program on every invocation.
  */
object Wiring:

  def businessLogic[F[_]: Async]: BusinessLogic[Flow[RunF[F]]] =
    type RealArr = Flow[RunF[F]]
    type ReplayArr = Replayability.ReplayFlow[RunF[F]]

    def cycle[A, B](arrow: => Kleisli[RunF[F], A, B]): Kleisli[RunF[F], A, B] =
      ArrowDefer[RealArr].defer(arrow)

    lazy val replayed: BusinessLogic[ReplayArr] =
      Replayability.combine(replayWired, realWired)
    lazy val realAssembled: BusinessLogic[RealArr] =
      Retryability.combine(retryWired, Replayability.lowerOrRaise(replayed))
    lazy val logged: BusinessLogic[RealArr] =
      realAssembled.withArrowLogging(arrowLogger[RunF[F]])

    // Evaluation is its own arrow group with its own factory; the cycle here
    // is that waiting for a user's answer ends by evaluating again.
    lazy val evaluationArrows: EvaluationArrows[RealArr] = EvaluationArrows[RunF[F]](
      progress = Impl.progress,
      evaluatorRunner = Impl.evaluatorRunner,
      waitForUserInput = Impl.waitForUserInput(cycle(logged.taskArrows.evaluateTask))
    )

    lazy val realWired: BusinessLogic[RealArr] = BusinessLogic[RealArr](
      programArrows = ProgramArrows[RealArr](
        resolveContext = Impl.resolveContext,
        selectTask = Impl.selectTask,
        routeEmptySelection = Impl.routeEmptySelection,
        noTaskSummary = Impl.noTaskSummary,
        loadOpenIssues = Impl.loadOpenIssues,
        routeParallelExecution = Impl.routeParallelExecution,
        recoverCandidateFailure = Impl.recoverCandidateFailure,
        lastSummary = Impl.lastSummary,
        toProgramSays = Impl.toProgramSays
      ),
      taskArrows = TaskArrows[RealArr](
        routeResumeOrRun = Impl.routeRun,
        resumeTask = replayOnlyResumeTask,
        announceTask = Impl.announceTask,
        fetchTaskContext = Impl.fetchTaskContext,
        evaluateTask = evaluationArrows.evaluateTask,
        needsUserInputSummary = Impl.needsUserInputSummary,
        splitTaskSummary = Impl.splitTaskSummary,
        markTaskInProgress = Impl.markTaskInProgress,
        acquireWorktreeAndExecute = Impl.acquireWorktreeAndExecute(cycle(logged.executePreparedTaskInWorktree)),
        completedTaskSummary = Impl.completedTaskSummary
      ),
      changeArrows = ChangeArrows[RealArr](
        classifyAgentResultForPublication = Impl.classifyAgentResultForPublication,
        toPublishRequest = Impl.toPublishRequestOfChanged,
        reportPublicationFailure = Impl.reportPublicationFailure,
        reportUnchangedTask = Impl.reportUnchangedTask
      ),
      publicationArrows = PublicationArrows[RealArr](
        classifyPublicationSource = Impl.classifyPublicationSource,
        prepareChangedPublication = Impl.prepareChangedPublication,
        prepareExistingPublication = Impl.prepareExistingPublication,
        choosePublicationTransport = Impl.choosePublicationTransport,
        publishRemote = PublishRemoteArrows[RealArr](
          toPushRequest = Impl.toPushRequest,
          pushBranch = Impl.pushBranch,
          toPublishRequest = Impl.toPublishRequest,
          createAndMergePullRequest = Impl.createAndMergePullRequest
        ),
        publishLocal = Impl.publishLocal
      ),
      executeTaskArrows = ExecuteTaskArrows[RealArr](
        runAgent = AgentRunArrows[RealArr](
          runTaskWithRunner = Impl.runTaskWithRunner
        ),
        runProjectValidation = Impl.runProjectValidation,
        recordAgentOutput = Impl.recordAgentOutput,
        markTaskImplemented = Impl.markTaskImplemented,
        verifyRelatedPullRequestCi = Impl.verifyRelatedPullRequestCi,
        closeTaskIssue = Impl.closeTaskIssue,
        checkParentsForCompletion = Impl.checkParentsForCompletion
      ),
      recursiveArrows = RecursiveArrows[RealArr](
        checkIfCompleted = Impl.checkIfCompleted,
        collectPendingDependencies = Impl.collectPendingDependencies,
        recordDependencyOutcome = Impl.recordDependencyOutcome,
        routeDependencyOutcome = Impl.routeDependencyOutcome,
        claimAndRun = Impl.claimAndRun(cycle(logged.executeCandidate))
      ),
      traversalArrows = TraversalArrows[RealArr](
        routeRecursiveMode = Impl.routeRecursiveMode,
        untilClosed = UntilClosedArrows[RealArr](
          refreshRoot = Impl.refreshRoot,
          runRootOnce = Impl.runRootOnce(cycle(logged.executeRecursive)),
          routeContinuation = Impl.routeContinuation
        ),
        runOnce = Impl.runOnce(cycle(logged.executeRecursive))
      )
    )

    def replayOnlyResumeTask: ResumeTaskArrows[RealArr] =
      ResumeTaskArrows[RealArr](
        resume = ResumePullRequestArrows[RealArr](
          resumePullRequest = replayOnly("resumePullRequest")
        ),
        announceResume = replayOnly("announceResume"),
        resumedExecution = replayOnly("resumedExecution"),
        cleanupAndSummarize = replayOnly("cleanupAndSummarize"),
        routeResumeError = replayOnly("routeResumeError"),
        announceNoPullRequest = replayOnly("announceNoPullRequest"),
        reportResumeFailure = replayOnly("reportResumeFailure")
      )

    def replayOnly[A, B](name: String): RealArr[A, B] =
      Kleisli(_ =>
        Sync[RunF[F]].raiseError(RuntimeException(s"Unexpected replay-only arrow in real business logic: $name"))
      )

    lazy val replayWired: BusinessLogic[ReplayArr] =
      BusinessLogicReplay[RunF[F]](
        progress = Impl.progress,
        evaluatorRunner = Impl.evaluatorRunner,
        waitForUserInput = Impl.waitForUserInput(cycle(logged.taskArrows.evaluateTask)),
        hasOriginBranch = Impl.git[RunF[F]].hasOriginBranch
      )

    lazy val retryWired: BusinessLogic[Retryability.RetryFlow[RealArr]] =
      BusinessLogicRetry[RunF[F]](progress = Impl.progress)

    logged

  def arrowLogger[F[_]: Sync]: ArrowLogger[Flow[F]] =
    new ArrowLogger[Flow[F]]:
      def apply[A, B](name: String, flow: -->[F, A, B]): -->[F, A, B] =
        traced(name, flow)

  def traced[F[_]: Sync, A, B](
      name: String,
      flow: -->[F, A, B]
  ): -->[F, A, B] =
    val logEnter =
      TaskLogger.trace[F, A](input => s"enter $name input=${summarize(input)}")
    val logExit =
      TaskLogger.trace[F, B](output => s"exit $name output=${summarize(output)}")
    val logError = TaskLogger.trace[F, Throwable](error =>
      s"fail $name error=${error.getClass.getSimpleName}: ${error.getMessage}"
    ) >>> Kleisli(Sync[F].raiseError[B])

    (logEnter >>> flow).attempt >>> (logError ||| logExit)

  def summarize(value: Any): String =
    value match
      case null => "null"
      case AppInput(root, taskNumber, recursive, parallelExecution) =>
        s"AppInput(root=$root,task=${taskNumber.fold("auto")(_.toString)},recursive=$recursive,parallel=$parallelExecution)"
      case RunContext(root, agentInventory, taskNumber, recursive, parallelExecution) =>
        s"RunContext(root=$root,task=${taskNumber.fold("auto")(_.toString)},recursive=$recursive,parallel=$parallelExecution,availableAgents=${agentInventory.availableTools.size})"
      case Issue(number, title, body, state, _) =>
        s"Issue(#$number,title=${quote(title.value)},state=$state,bodyChars=${body.value.length})"
      case TaskRunner(agent, model, effort, version) =>
        s"TaskRunner(agent=$agent,model=${model.getOrElse("")},effort=${effort
            .getOrElse("")},version=${version.getOrElse("")})"
      case TaskCandidate(_, issue, runner, resumePullRequest) =>
        s"TaskCandidate(issue=#${issue.number},runner=${runner.display},resumePullRequest=$resumePullRequest)"
      case TaskSelection(_, candidates) =>
        s"TaskSelection(candidates=${candidates.map(t => s"#${t.issue.number}").mkString(",")})"
      case NoTask(_) =>
        "NoTask"
      case ClaimedTask(_, task, runner, worktreePath, branchName, baseBranch) =>
        s"ClaimedTask(issue=#${task.number},runner=${runner.display},worktree=$worktreePath,branch=$branchName,base=${baseBranch
            .getOrElse("default")})"
      case PreparedTask(run, parentConclusion, replayContext) =>
        s"PreparedTask(issue=#${run.task.number},hasDependencyConclusion=${parentConclusion.nonEmpty},hasReplayContext=${replayContext.nonEmpty})"
      case ExecutedTask(run, output) =>
        s"ExecutedTask(issue=#${run.task.number},outputChars=${output.value.length})"
      case NeedsUserInput(run, questions) =>
        s"NeedsUserInput(issue=#${run.task.number},questionChars=${questions.value.length})"
      case SplitTask(run, _) =>
        s"SplitTask(issue=#${run.task.number})"
      case TaskEvaluation(body, questions, execution) =>
        s"TaskEvaluation(execution=$execution,bodyChars=${body.value.length},hasQuestions=${questions
            .exists(_.trim.nonEmpty)})"
      case ExistingBranch(run) =>
        s"ExistingBranch(issue=#${run.claimedTask.task.number},branch=${run.claimedTask.branchName})"
      case NewBranch(run) =>
        s"NewBranch(issue=#${run.claimedTask.task.number},branch=${run.claimedTask.branchName})"
      case ChangedTask(run) =>
        s"ChangedTask(issue=#${run.run.task.number})"
      case UnchangedTask(run) =>
        s"UnchangedTask(issue=#${run.run.task.number})"
      case AgentFinalization(commitTitle, pullRequestBody) =>
        s"AgentFinalization(hasCommitTitle=${commitTitle.nonEmpty},hasPullRequestBody=${pullRequestBody.nonEmpty})"
      case PublishRequest(
            _,
            _,
            branchName,
            baseBranch,
            task,
            finalization,
            runner
          ) =>
        s"PublishRequest(issue=#${task.number},branch=$branchName,base=${baseBranch
            .getOrElse("default")},runner=${runner.display},${summarize(finalization)})"
      case ChangedPublication(request) =>
        s"ChangedPublication(${summarize(request)})"
      case ExistingPublication(request) =>
        s"ExistingPublication(${summarize(request)})"
      case RunSummary(status, message, task) =>
        s"RunSummary(status=$status,task=${task
            .map(_.number)
            .fold("none")(number => s"#$number")},message=${quote(message.value)})"
      case Left(value) =>
        s"Left(${summarize(value)})"
      case Right(value) =>
        s"Right(${summarize(value)})"
      case Some(value) =>
        s"Some(${summarize(value)})"
      case None =>
        "None"
      case list: List[?] =>
        s"List(size=${list.size})"
      case seq: Seq[?] =>
        s"Seq(size=${seq.size})"
      case other =>
        truncate(other.toString, 240)

  def quote(value: String): String =
    "\"" + truncate(value.linesIterator.mkString("\\n"), 120) + "\""

  def truncate(value: String, maxLength: Int): String =
    if value.length <= maxLength then value
    else value.take(maxLength) + "...[truncated]"
