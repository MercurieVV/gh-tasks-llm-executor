package com.github.mercurievv.ghllm.agent

/** A leaf that ran past its turn budget.
  *
  * Deliberately a distinct type rather than a plain `RuntimeException`: the
  * escalation router pattern-matches on it to produce a `Red` verdict instead of
  * a `Failed` one, which is the difference between "this cheap runner could not
  * finish the leaf" and "the tool is broken".
  */
final case class TurnCapExceeded(turnCount: Int, cap: Int)
    extends RuntimeException(
      s"Agent exceeded the per-leaf turn cap of $cap turns (reported $turnCount)."
    )

object TurnCap:
  val Default = 25

  val RelativePath: os.RelPath =
    os.rel / ".gh-tasks-llm-executor" / "execution-limits.json"

  def load(root: os.Path): Int =
    val path = root / RelativePath
    if !os.exists(path) then Default
    else
      scala.util
        .Try(ujson.read(os.read(path)).obj.get("turnCap").flatMap(_.numOpt).map(_.toInt))
        .toOption
        .flatten
        // A non-positive cap would kill every run on its first turn, so treat a
        // malformed value as "no opinion" rather than as a hard stop.
        .filter(_ > 0)
        .getOrElse(Default)

  /** The breach, if the reported turn count exceeded `cap`.
    *
    * Detection is after the fact by necessity: `num_turns` only arrives in the
    * runner's terminal JSON, so there is no mid-flight counter to trip. It still
    * pays off — the leaf is re-run on a stronger runner instead of the same weak
    * one grinding through another repair round at the same price.
    */
  def exceeded(turnCount: Option[Int], cap: Int): Option[TurnCapExceeded] =
    turnCount.filter(_ > cap).map(TurnCapExceeded(_, cap))
