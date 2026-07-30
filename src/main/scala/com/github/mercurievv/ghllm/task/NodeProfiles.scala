package com.github.mercurievv.ghllm.task

import com.github.mercurievv.ghllm.*
import com.github.mercurievv.ghllm.TaskTree.NodeProfile
import com.github.mercurievv.ghllm.TaskTree.NodeRef
import com.github.mercurievv.ghllm.TaskTree.Stage0Coefficients
import com.github.mercurievv.ghllm.arrow.PrefixKey
import com.github.mercurievv.ghllm.metrics.TokenMetrics
import com.github.mercurievv.ghllm.metrics.TokenMetrics.TokenMetricsEvent

/** Builds the cost fold's `profileFor` out of recorded Stage 0 measurements.
  *
  * Until now `profileFor` only ever came from a test's hand-written `Map`, so
  * the cost model computed correct arithmetic over invented numbers. These are
  * the real coefficients.
  *
  * Lookup falls back in three steps, because the useful question is what a plan
  * will cost and most nodes in a plan have never run:
  *
  *   1. this exact task's own history — the only true measurement;
  *   2. the mean over its phase, for a task not yet run;
  *   3. the mean over everything, for a task whose phase is unset or unseen.
  *
  * An empty history yields zero coefficients, not a guess. A zero estimate is
  * visibly useless; a fabricated one is not, and would be read as a forecast.
  */
object NodeProfiles:

  val UnknownTier = "unknown"

  def fromMetrics(
      backend: TokenMetrics.TokenMetricsBackend,
      worktree: os.Path
  ): NodeRef => NodeProfile =
    fromEvents(backend.query(TokenMetrics.TokenMetricsQuery(limit = None)), worktree)

  def fromEvents(
      events: List[TokenMetricsEvent],
      worktree: os.Path
  ): NodeRef => NodeProfile =
    val byTask: Map[Int, List[TokenMetricsEvent]] =
      events.groupBy(_.taskNumber.map(_.value)).collect { case (Some(number), es) => number -> es }
    val byPhase: Map[String, List[TokenMetricsEvent]] =
      events.groupBy(_.phase.map(normalise)).collect { case (Some(phase), es) => phase -> es }

    ref =>
      val (tier, sample) = ref match
        case NodeRef.Task(number, phase) =>
          val phaseKey = phase.map(normalise)
          val own = byTask.getOrElse(number, Nil)
          val fromPhase = phaseKey.flatMap(byPhase.get).getOrElse(Nil)
          val chosen =
            if own.nonEmpty then own
            else if fromPhase.nonEmpty then fromPhase
            else events
          (phaseKey.orElse(chosen.flatMap(_.phase).headOption).getOrElse(UnknownTier), chosen)
        case _ =>
          (UnknownTier, events)
      profileOf(tier, sample, worktree)

  /** Mean per-run cost of a sample.
    *
    * The token counts on an event are already cumulative over that run's turns,
    * so the mean over events is a mean over runs and `turnCount` stays a
    * diagnostic — multiplying it in would count the same tokens twice.
    */
  def coefficientsOf(events: List[TokenMetricsEvent]): Stage0Coefficients =
    if events.isEmpty then Zero
    else
      val n = events.size.toDouble
      Stage0Coefficients(
        inputTokens = events.map(_.usage.input.toDouble).sum / n,
        cachedInputTokens = events.map(_.usage.cacheRead.toDouble).sum / n,
        cacheWriteTokens = events.map(_.usage.cacheWrite.toDouble).sum / n,
        outputTokens = events.map(_.usage.output.toDouble).sum / n,
        turnCount = events.flatMap(_.turnCount).map(_.toDouble).sum / n
      )

  val Zero: Stage0Coefficients =
    Stage0Coefficients(0.0, 0.0, 0.0, 0.0, 0.0)

  private def profileOf(
      tier: String,
      sample: List[TokenMetricsEvent],
      worktree: os.Path
  ): NodeProfile =
    NodeProfile(
      PrefixKey(
        runner = mostCommon(sample.flatMap(_.runner)).getOrElse(UnknownTier),
        model = mostCommon(sample.flatMap(_.model)),
        worktree = worktree
      ),
      tier,
      coefficientsOf(sample)
    )

  private def mostCommon(values: List[String]): Option[String] =
    values.groupBy(identity).toList.sortBy { case (value, occurrences) =>
      (-occurrences.size, value)
    }.headOption.map(_._1)

  private def normalise(value: String): String = value.trim.toLowerCase
