package com.github.mercurievv.ghllm.arrow

import os.Path
import java.security.MessageDigest

final case class PrefixKey(
    runner: String,
    model: Option[String],
    worktree: os.Path,
    stablePrefixHash: String
)

object PrefixKey:

  /** Compute a stable hash from the three stable prompt layers (0,1,2). */
  def of(
      runner: String,
      model: Option[String],
      worktree: os.Path,
      layer0: String,
      layer1: String,
      layer2: String
  ): PrefixKey =
    val hash = sha256(s"${layer0}\n${layer1}\n${layer2}")
    PrefixKey(runner, model, worktree, hash)

  private def sha256(s: String): String =
    val md = MessageDigest.getInstance("SHA-256")
    val digestBytes = md.digest(s.getBytes("UTF-8"))
    digestBytes.map(b => f"$b%02x").mkString
