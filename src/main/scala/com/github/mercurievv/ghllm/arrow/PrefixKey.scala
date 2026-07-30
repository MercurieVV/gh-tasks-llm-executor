package com.github.mercurievv.ghllm.arrow

import os.*

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

final case class PrefixKey(
    runner: String,
    model: Option[String],
    worktree: os.Path,
    stablePrefixHash: String
)

object PrefixKey:
  def of(
      runner: String,
      model: Option[String],
      worktree: os.Path,
      layer0: String,
      layer1: String,
      layer2: String
  ): PrefixKey =
    // The stable prefix hash is computed only from the three layers so that
    // identical layers produce the same hash regardless of runner/model/worktree.
    val input =
      List(layer0, layer1, layer2)
        .mkString("|")
    val digest = MessageDigest
      .getInstance("SHA-256")
      .digest(input.getBytes(StandardCharsets.UTF_8))
    val hash = digest.map(b => f"$b%02x").mkString
    PrefixKey(runner, model, worktree, hash)

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
