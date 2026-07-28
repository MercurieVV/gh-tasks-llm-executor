package com.github.mercurievv.ghllm

import munit.FunSuite
import higherkindness.droste.data.Mu
import com.github.mercurievv.ghllm.TaskTree.*

class TaskTreeTestSuite extends FunSuite:
  test("branch and leaf construct valid trees"):
    val t = branch("root", List(leaf(Some(1)), branch("sub", List(leaf(None)))))
    assert(t ne null)
