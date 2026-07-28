package com.github.mercurievv.ghllm

import higherkindness.droste._
import higherkindness.droste.data._
import higherkindness.droste.syntax.all._

class TaskTreeSuite extends munit.FunSuite {

  test("build a 3-node tree and fold it to a node count with cata") {
    import com.github.mercurievv.ghllm.TaskTree.{given, _}

    val leaf: Fix[TaskF] = Fix(TaskF.Branch("leaf", Nil))
    val left: Fix[TaskF] = Fix(TaskF.Branch("left", List(leaf)))
    val tree: Fix[TaskF] = Fix(TaskF.Branch("root", List(left)))

    val countAlg: Algebra[TaskF, Int] = Algebra {
      case TaskF.Branch(_, kids) => kids.sum + 1
      case TaskF.Leaf(_)         => 1
    }

    val total = tree.cata(countAlg)
    assertEquals(total, 3)
  }
}
