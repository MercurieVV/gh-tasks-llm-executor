package com.github.mercurievv.ghllm

import higherkindness.droste.{Algebra, scheme}
import higherkindness.droste.data.Fix
import TaskTree.{TaskF, given}

class TaskTreeTest extends munit.FunSuite {

  test("3-node tree folds to node count with cata") {
    // Two leaf nodes.
    val leafA = Fix(TaskF("a", Nil))
    val leafB = Fix(TaskF("b", Nil))
    // Root with two children → 3 nodes total.
    val root = Fix(TaskF("root", List(leafA, leafB)))

    // Algebra: each node contributes 1 + sum of children counts.
    val countAlgebra: Algebra[TaskF, Int] = Algebra { case TaskF(_, children) =>
      children.sum + 1
    }

    assertEquals(scheme.cata(countAlgebra)(root), 3)
  }
}
