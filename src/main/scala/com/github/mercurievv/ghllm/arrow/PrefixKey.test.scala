package com.github.mercurievv.ghllm.arrow

import munit.*
import os.Path

class PrefixKeySuite extends munit.FunSuite:

  val worktree = os.pwd

  test("identical inputs produce identical hash"):
    val key = PrefixKey.of("runner", None, worktree, "layer0", "layer1", "layer2")
    val key2 = PrefixKey.of("runner", None, worktree, "layer0", "layer1", "layer2")
    assertEquals(key.stablePrefixHash, key2.stablePrefixHash)

  test("identical layers produce identical hash regardless of runner/worktree"):
    val kA = PrefixKey.of("runnerA", Some("modelA"), os.pwd / "one", "a", "b", "c")
    val kB = PrefixKey.of("runnerB", None,           os.pwd / "two", "a", "b", "c")
    assertEquals(kA.stablePrefixHash, kB.stablePrefixHash)

  test("changing layer0 alters the hash"):
    val keyBase = PrefixKey.of("runner", None, worktree, "l0", "l1", "l2")
    val keyDiff = PrefixKey.of("runner", None, worktree, "l0_changed", "l1", "l2")
    assertNotEquals(keyBase.stablePrefixHash, keyDiff.stablePrefixHash)

  test("changing layer1 alters the hash"):
    val keyBase = PrefixKey.of("runner", None, worktree, "l0", "l1", "l2")
    val keyDiff = PrefixKey.of("runner", None, worktree, "l0", "l1_changed", "l2")
    assertNotEquals(keyBase.stablePrefixHash, keyDiff.stablePrefixHash)

  test("changing layer2 alters the hash"):
    val keyBase = PrefixKey.of("runner", None, worktree, "l0", "l1", "l2")
    val keyDiff = PrefixKey.of("runner", None, worktree, "l0", "l1", "l2_changed")
    assertNotEquals(keyBase.stablePrefixHash, keyDiff.stablePrefixHash)
