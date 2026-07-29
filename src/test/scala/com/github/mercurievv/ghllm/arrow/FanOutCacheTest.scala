package com.github.mercurievv.ghllm.arrow

import com.github.mercurievv.ghllm.*
import com.github.mercurievv.ghllm.cli.ParallelExecution
import com.github.mercurievv.ghllm.agent.*

class FanOutCacheTest extends munit.CatsEffectSuite:

  val minimalIssue = Issue(
    TaskNumber(1),
    IssueTitle("Test task"),
    IssueBody(""),
    State("open")
  )

  val minimalRunner = TaskRunner(AgentBinary("test"), None, None, None)

  /** Build a PreparedTask with the given parallelExecution flag. */
  def prepared(parallel: Boolean): PreparedTask =
    val context = RunContext(
      root = os.pwd,
      inventory = AgentInventory(Nil),
      taskNumber = None,
      recursive = Recursive(false),
      parallelExecution = ParallelExecution(parallel)
    )
    val claimed = ClaimedTask(
      context = context,
      task = minimalIssue,
      runner = minimalRunner,
      worktreePath = os.pwd / ".worktrees" / "test-1",
      branchName = BranchName("task-1"),
      baseBranch = None
    )
    PreparedTask(claimed, dependencyConclusion = None, replayContext = None)

  override def afterEach(context: AfterEach): Unit =
    // reset the shared store so tests do not leak state
    Impl.setCurrentBatchSize(0)

  test("cache TTL marker is present when batch size >= 3") {
    Impl.setCurrentBatchSize(3)
    val task = prepared(parallel = true)
    val basePrompt = Impl.taskPrompt(
      task.claimedTask.task,
      task.claimedTask.runner,
      task.parentConclusion,
      task.replayContext
    )
    val hinted = Impl.maybePrependCacheHint(basePrompt, extendedCache = true)
    assert(clue(hinted.value).contains(Impl.CacheTtlExtendedMarker))
  }

  test("cache TTL marker is absent when batch size < 3") {
    Impl.setCurrentBatchSize(2)
    val task = prepared(parallel = true)
    val basePrompt = Impl.taskPrompt(
      task.claimedTask.task,
      task.claimedTask.runner,
      task.parentConclusion,
      task.replayContext
    )
    // extendedCache=false because batch <3, so the marker must NOT be added
    val hinted = Impl.maybePrependCacheHint(basePrompt, extendedCache = false)
    assert(!clue(hinted.value).contains(Impl.CacheTtlExtendedMarker))
  }
