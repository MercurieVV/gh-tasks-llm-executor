package com.github.mercurievv.ghllm.agent

/** A leaf that ran past its turn budget.
  *
  * Deliberately a distinct type rather than a plain `RuntimeException`: the escalation router pattern-matches on it to
  * produce a `Red` verdict instead of a `Failed` one, which is the difference between "this cheap runner could not
  * finish the leaf" and "the tool is broken".
  */
final case class TurnCapExceeded(turnCount: Int, cap: Int)
    extends RuntimeException(
      s"Agent exceeded the per-leaf turn cap of $cap turns (reported $turnCount)."
    )

object TurnCap:
  val Default = 25

  /** Repairs are not leaves and must not be measured as if they were.
    *
    * A leaf implements one specified change; a repair resolves whatever the conflict or the failing check turns out to
    * involve, across as many files as it spans. On 2026-08-01 a four-file merge repair took 75 turns against the leaf
    * cap of 25, and because the count only arrives at the end, the cap bounded nothing — it discarded fifteen minutes
    * of finished work and re-ran the same job on a pricier runner.
    */
  val RepairMultiplier = 3

  val RelativePath: os.RelPath =
    os.rel / ".gh-tasks-llm-executor" / "execution-limits.json"

  def load(root: os.Path): Int =
    loadKey(root, "turnCap").getOrElse(Default)

  /** The cap for a repair: `repairTurnCap` if the repository states one, else `RepairMultiplier` times the leaf cap. */
  def loadRepair(root: os.Path): Int =
    loadKey(root, "repairTurnCap").getOrElse(load(root) * RepairMultiplier)

  private def loadKey(root: os.Path, key: String): Option[Int] =
    val path = root / RelativePath
    if !os.exists(path) then None
    else
      scala.util
        .Try(ujson.read(os.read(path)).obj.get(key).flatMap(_.numOpt).map(_.toInt))
        .toOption
        .flatten
        // A non-positive cap would kill every run on its first turn, so treat a
        // malformed value as "no opinion" rather than as a hard stop.
        .filter(_ > 0)

  /** The breach, if the reported turn count exceeded `cap`.
    *
    * Detection is after the fact by necessity: `num_turns` only arrives in the runner's terminal JSON, so there is no
    * mid-flight counter to trip. Which is why, since 2026-08-01, a breach is reported and recorded but no longer
    * raised: by the time it is knowable the turns are bought and the edits are on disk, so failing on it discarded
    * finished work before the verifier had looked at it. See `AgentExecutor.runMonitored`. A run that burned its turns
    * without finishing still fails, on `runProjectValidation` — which is the check that can tell the two apart.
    */
  def exceeded(turnCount: Option[Int], cap: Int): Option[TurnCapExceeded] =
    turnCount.filter(_ > cap).map(TurnCapExceeded(_, cap))
