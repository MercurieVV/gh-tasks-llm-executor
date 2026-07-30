package com.github.mercurievv.ghllm.arrow

import os.*

/** Cache attribution for a node: who ran it, on what model, in which worktree.
  *
  * This carried a `stablePrefixHash` (SHA-256 over three prompt layers) until it
  * was removed. Nothing cached on it, and it could not usefully be made to: its
  * only caller fed it the constants `"system" | "repo" | tier`, so the hash was
  * a restatement of `tier`, which sits beside it in `TaskTree.Attr`. What
  * actually decides whether two runs share a cached prefix is `(agent, model)` —
  * see [[CachePeers]], which is what T20 groups on.
  */
final case class PrefixKey(
    runner: String,
    model: Option[String],
    worktree: os.Path
)

/** Who is worth paying the extended (1-hour) cache TTL for — T20.
  *
  * The premium is roughly 2x on the write and pays back over about 3 reads, so a
  * group is marked only once it has [[MinPeers]] members sharing the same
  * `(agent, model)`. Peers do not have to run concurrently: an hour-long window
  * is exactly what lets a later sibling read a prefix an earlier one wrote.
  */
object CachePeers:
  val MinPeers: Int = 3

  /** For each key in `keys` (input order preserved), whether its group is large
    * enough to earn the TTL.
    */
  def qualifying[K](keys: List[K]): List[Boolean] =
    val counts = keys.groupMapReduce(identity)(_ => 1)(_ + _)
    keys.map(key => counts.getOrElse(key, 0) >= MinPeers)
