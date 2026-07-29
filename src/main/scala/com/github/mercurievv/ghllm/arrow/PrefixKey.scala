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
