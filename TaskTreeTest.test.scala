package com.github.mercurievv.ghllm

import higherkindness.droste.{Algebra, scheme}
import higherkindness.droste.data.Fix
import higherkindness.droste.syntax.all._
import com.github.mercurievv.ghllm.TaskTree.TaskF

class TaskTreeTest extends munit.FunSuite {

  test("3-node tree folds to node count with cata") {
    // Two leaf nodes.
    val leafA = Fix(TaskF.Branch("a", Nil))
    val leafB = Fix(TaskF.Branch("b", Nil))
    // Root with two children → 3 nodes total.
    val root  = Fix(TaskF.Branch("root", List(leafA, leafB)))

    // Algebra: each node contributes 1 + sum of children counts.
    val countAlgebra: Algebra[TaskF, Int] = Algebra {
      case TaskF.Branch(_, children) =>
        children.sum + 1
      case TaskF.Leaf(_) => 1
    }

    val total = root.cata(countAlgebra)
    assertEquals(total, 3)
  }
}
