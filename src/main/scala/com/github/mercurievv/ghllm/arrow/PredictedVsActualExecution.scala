package com.github.mercurievv.ghllm.arrow

import com.github.mercurievv.ghllm.*
import com.github.mercurievv.ghllm.cli.*
import com.github.mercurievv.ghllm.metrics.TokenMetrics
import com.github.mercurievv.ghllm.metrics.TokenMetrics.TokenMetricsBackend
import com.github.mercurievv.ghllm.metrics.TokenMetrics.TokenMetricsQuery
import com.github.mercurievv.ghllm.task.NodeProfiles
import com.github.mercurievv.ghllm.task.TaskGraph

import cats.data.Kleisli
import cats.effect.Ref
import cats.effect.kernel.Async
import cats.syntax.all.*

/** Answers the question this traversal work started from: since `HyloExecutionSpike` now drives both cost estimation
  * (`TaskTree.estimate`) and real execution (`HyloExecutionSpike.executeRecursive`, wired in `Wiring.scala` via
  * `HyloExecutionSpike.wire`) off the exact same `collectPendingDependencies` field, a prediction taken immediately
  * before a run and the run's own set of visited task numbers describe the same tree - so the predicted `$` and the `$`
  * actually billed for those same task numbers over the run's wall-clock window are a fair diff, not two numbers
  * computed by different code at different times.
  *
  * Deliberately takes `recursiveArrows: RecursiveArrows[Flow[RunF[F]]]` rather than calling `Impl.*`/`Wiring` directly:
  * both the prediction's tree-unfold and the execution's fold read `collectPendingDependencies`/
  * `checkIfCompleted`/`claimAndRun` off the *same* field values, so passing the production `logged.recursiveArrows`
  * (already logged, already replay-resolved) makes this a real report, and passing a test double (as
  * `PredictedVsActualExecution.test.scala` does) makes it a fast, offline one - no separate code path to keep in sync.
  */
object PredictedVsActualExecution:

  final case class Report(
      predictedUsd: Double,
      actualUsd: Double,
      visited: List[TaskNumber],
      result: RunSummary
  )

  def runWithPrediction[F[_]: Async](
      root: TaskNode,
      recursiveArrows: RecursiveArrows[Flow[RunF[F]]],
      costModel: TaskTree.CostModel,
      metricsBackend: TokenMetricsBackend,
      worktree: os.Path
  ): RunF[F][Report] =
    Kleisli { env =>
      val profileFor = NodeProfiles.fromMetrics(metricsBackend, worktree)
      val pendingOf: TaskNode => RunF[F][List[TaskNode]] =
        node => recursiveArrows.collectPendingDependencies.run(node).map(_.pending)

      for
        startedAtMillis <- Async[F].realTime.map(_.toMillis)
        tree <- TaskGraph.unfold[RunF[F]](pendingOf)(TaskGraph.seed(root)).run(env)
        predicted = TaskTree.estimate(TaskTree.ofTaskGraph(tree), costModel, profileFor)
        visited <- Ref.of[F, List[TaskNumber]](Nil)
        recordingClaimAndRun = (node: TaskNode) =>
          visited.update(_ :+ node.issue.number) *> recursiveArrows.claimAndRun.run(node).run(env)
        result <- HyloExecutionSpike.executeRecursive[F](
          checkIfCompleted = node => recursiveArrows.checkIfCompleted.run(node).run(env),
          collectPendingDependencies = node => recursiveArrows.collectPendingDependencies.run(node).run(env),
          claimAndRun = recordingClaimAndRun
        )(root)
        visitedNumbers <- visited.get
        actualEvents = visitedNumbers.flatMap { number =>
          metricsBackend.query(
            TokenMetricsQuery(taskNumber = Some(number), sinceMillis = Some(startedAtMillis), limit = None)
          )
        }
        // Sum, not NodeProfiles.coefficientsOf's per-run mean: predicted.subtreeUsd
        // is additive over the whole subtree (TaskTree.Cost's own contract), so the
        // actual side of the diff has to be a total too, not an average node.
        actualUsd = costModel.estimate(
          TaskTree.Stage0Coefficients(
            inputTokens = actualEvents.map(_.usage.input.toDouble).sum,
            cachedInputTokens = actualEvents.map(_.usage.cacheRead.toDouble).sum,
            cacheWriteTokens = actualEvents.map(_.usage.cacheWrite.toDouble).sum,
            outputTokens = actualEvents.map(_.usage.output.toDouble).sum,
            turnCount = actualEvents.flatMap(_.turnCount).map(_.toDouble).sum
          )
        )
      yield Report(predicted.subtreeUsd, actualUsd, visitedNumbers, result)
    }
