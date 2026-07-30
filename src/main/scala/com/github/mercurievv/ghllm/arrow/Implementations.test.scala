package com.github.mercurievv.ghllm.arrow

import com.github.mercurievv.ghllm.*
import com.github.mercurievv.ghllm.agent.*
import com.github.mercurievv.ghllm.arrow.*
import com.github.mercurievv.ghllm.cli.*
import com.github.mercurievv.ghllm.git.*
import com.github.mercurievv.ghllm.metrics.*

import cats.effect.IO
import cats.effect.Ref
import munit.CatsEffectSuite

class SemanticDbMemoizationSuite extends CatsEffectSuite:
  test("refresh runs once across two dispatches with unchanged source hash"):
    for
      issues <- Ref[IO].of(Map.empty[TaskNumber, Issue])
      env <- RunEnv.create[IO](issues)
      count <- Ref[IO].of(0)
      one = SemanticDbSource(os.pwd, "one")
      two = SemanticDbSource(os.pwd, "two")
      _ <- Impl.refreshSemanticDbIfNeeded(env, one)(count.update(_ + 1))
      _ <- Impl.refreshSemanticDbIfNeeded(env, one)(count.update(_ + 1))
      unchanged <- count.get
    yield assert(unchanged == 1, s"expected 1 refresh, got $unchanged")

  test("refresh runs twice when source hash changes"):
    for
      issues <- Ref[IO].of(Map.empty[TaskNumber, Issue])
      env <- RunEnv.create[IO](issues)
      count <- Ref[IO].of(0)
      one = SemanticDbSource(os.pwd, "one")
      two = SemanticDbSource(os.pwd, "two")
      _ <- Impl.refreshSemanticDbIfNeeded(env, one)(count.update(_ + 1))
      _ <- Impl.refreshSemanticDbIfNeeded(env, one)(count.update(_ + 1))
      unchanged <- count.get
      _ <- Impl.refreshSemanticDbIfNeeded(env, two)(count.update(_ + 1))
      changed <- count.get
    yield assert(changed == 2, s"expected 2 refreshes, got $changed")
