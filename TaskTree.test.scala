package com.github.mercurievv.ghllm

import higherkindness.droste._
import higherkindness.droste.data._
import higherkindness.droste.syntax.all._

class TaskTreeSuite extends munit.FunSuite {

  test("build a 3-node tree and fold it to a node count with cata") {
    import TaskTree._

    val leaf: FixTaskTree = Fix(TaskF("leaf", Nil))
    val left: FixTaskTree = Fix(TaskF("left", List(leaf)))
    val tree: FixTaskTree = Fix(TaskF("root", List(left)))

    val countAlg: Algebra[TaskF, Int] = Algebra {
      case TaskF(_, kids) => 1 + kids.sum
    }
    val total = tree.cata(countAlg)
    assertEquals(total, 3)
  }
}
