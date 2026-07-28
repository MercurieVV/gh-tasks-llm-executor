package com.github.mercurievv.ghllm

import com.github.mercurievv.ghllm.*
import com.github.mercurievv.ghllm.agent.*
import com.github.mercurievv.ghllm.arrow.*
import com.github.mercurievv.ghllm.cli.*
import com.github.mercurievv.ghllm.git.*
import com.github.mercurievv.ghllm.metrics.*

import cats.data.Kleisli
import cats.effect.IO

/** A whole `BusinessLogic` whose every leaf fails if it is called.
  *
  * Tests override only the leaves the composition under test is supposed to reach, so reaching anything else is itself
  * the failure. That is what makes these tests about the SHAPE of the program - which arrow runs, in what order, on
  * which branch - rather than about any implementation.
  */
object BusinessLogicFixture:
  type TestFlow[A, B] = Kleisli[IO, A, B]

  def unexpected[A, B](name: String): TestFlow[A, B] =
    Kleisli(input => IO.raiseError(AssertionError(s"unexpected call: $name($input)")))

  val context: RunContext = RunContext(os.pwd, AgentInventory(Nil), None)

  def issue(number: Int, state: String = "open"): Issue =
    Issue(TaskNumber(number), IssueTitle(s"Task $number"), IssueBody(""), State(state))

  val runner: TaskRunner = TaskRunner(AgentBinary("claude"), None, None, None)

  def candidate(number: Int, parallel: Boolean = false, recursive: Boolean = false): TaskCandidate =
    TaskCandidate(
      context.copy(
        parallelExecution = ParallelExecution(parallel),
        recursive = Recursive(recursive)
      ),
      issue(number),
      runner
    )

  def claimed(number: Int): ClaimedTask =
    ClaimedTask(context, issue(number), runner, os.pwd, BranchName(s"task-$number"), None)

  def prepared(number: Int): PreparedTask = PreparedTask(claimed(number), None, None)

  def executed(number: Int): ExecutedTask = ExecutedTask(claimed(number), AgentOutput(""))

  def summary(message: String, status: String = "completed"): RunSummary =
    RunSummary(Status(status), Message5(message), None)

  val logic: BusinessLogic[TestFlow] = BusinessLogic[TestFlow](
    programArrows = ProgramArrows[TestFlow](
      resolveContext = unexpected("resolveContext"),
      selectTask = unexpected("selectTask"),
      routeEmptySelection = unexpected("routeEmptySelection"),
      noTaskSummary = unexpected("noTaskSummary"),
      loadOpenIssues = unexpected("loadOpenIssues"),
      routeParallelExecution = unexpected("routeParallelExecution"),
      recoverCandidateFailure = unexpected("recoverCandidateFailure"),
      lastSummary = unexpected("lastSummary"),
      toProgramSays = unexpected("toProgramSays")
    ),
    taskArrows = TaskArrows[TestFlow](
      routeResumeOrRun = unexpected("routeResumeOrRun"),
      resumeExistingPullRequest = ExistingPullRequestResumeArrows[TestFlow](
        pullRequest = PullRequestResumeArrows[TestFlow](
          resumeOpenPullRequest = unexpected("resumeOpenPullRequest")
        ),
        announceResume = unexpected("announceResume"),
        toResumedExecution = unexpected("toResumedExecution"),
        cleanupAndSummarize = unexpected("cleanupAndSummarize"),
        routeResumeError = unexpected("routeResumeError"),
        announceNoPullRequest = unexpected("announceNoPullRequest"),
        reportResumeFailure = unexpected("reportResumeFailure")
      ),
      announceTask = unexpected("announceTask"),
      fetchTaskContext = unexpected("fetchTaskContext"),
      evaluateTask = unexpected("evaluateTask"),
      needsUserInputSummary = unexpected("needsUserInputSummary"),
      splitTaskSummary = unexpected("splitTaskSummary"),
      markTaskInProgress = unexpected("markTaskInProgress"),
      acquireWorktreeAndExecute = unexpected("acquireWorktreeAndExecute"),
      completedTaskSummary = unexpected("completedTaskSummary")
    ),
    changeArrows = ChangeArrows[TestFlow](
      classifyAgentResultForPublication = unexpected("classifyAgentResultForPublication"),
      toPublishRequest = unexpected("toPublishRequest"),
      reportPublicationFailure = unexpected("reportPublicationFailure"),
      reportUnchangedTask = unexpected("reportUnchangedTask")
    ),
    publicationArrows = PublicationArrows[TestFlow](
      classifyPublicationSource = unexpected("classifyPublicationSource"),
      prepareChangedPublication = unexpected("prepareChangedPublication"),
      prepareExistingPublication = unexpected("prepareExistingPublication"),
      choosePublicationTransport = unexpected("choosePublicationTransport"),
      publishRemote = PublishRemoteArrows[TestFlow](
        toPushRequest = unexpected("toPushRequest"),
        pushBranch = unexpected("pushBranch"),
        toPublishRequest = unexpected("toPublishRequestRemote"),
        createAndMergePullRequest = unexpected("createAndMergePullRequest")
      ),
      publishLocal = unexpected("publishLocal")
    ),
    executeTaskArrows = ExecuteTaskArrows[TestFlow](
      runAgent = AgentRunArrows[TestFlow](
        runTaskWithRunner = unexpected("runTaskWithRunner")
      ),
      runProjectValidation = unexpected("runProjectValidation"),
      recordAgentOutput = unexpected("recordAgentOutput"),
      markTaskImplemented = unexpected("markTaskImplemented"),
      verifyRelatedPullRequestCi = unexpected("verifyRelatedPullRequestCi"),
      closeTaskIssue = unexpected("closeTaskIssue"),
      checkParentsForCompletion = unexpected("checkParentsForCompletion")
    ),
    recursiveArrows = RecursiveArrows[TestFlow](
      checkIfCompleted = unexpected("checkIfCompleted"),
      collectPendingDependencies = unexpected("collectPendingDependencies"),
      recordDependencyOutcome = unexpected("recordDependencyOutcome"),
      routeDependencyOutcome = unexpected("routeDependencyOutcome"),
      claimAndRun = unexpected("claimAndRun")
    ),
    traversalArrows = TraversalArrows[TestFlow](
      routeRecursiveMode = unexpected("routeRecursiveMode"),
      untilClosed = UntilClosedArrows[TestFlow](
        refreshRoot = unexpected("refreshRoot"),
        runRootOnce = unexpected("runRootOnce"),
        routeContinuation = unexpected("routeContinuation")
      ),
      runOnce = unexpected("runOnce")
    )
  )
