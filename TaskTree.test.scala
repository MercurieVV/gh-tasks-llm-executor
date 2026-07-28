package com.github.mercurievv.ghllm

import higherkindness.droste.{scheme, Algebra}
import higherkindness.droste.data.Mu
import com.github.mercurievv.ghllm.task.TaskF
import com.github.mercurievv.ghllm.TaskTree
import munit.FunSuite

class TaskTreeSuite extends FunSuite {

  test("cata over a simple tree") {
    val countAlg: Algebra[TaskF, Int] = Algebra {
      case TaskF.Leaf(_)           => 1
      case TaskF.Branch(_, children) => children.sum + 1
    }

    val tree = TaskTree.branch(
      "root",
      List(
        Mu(TaskF.Leaf(Some(5))),
        TaskTree.branch(
          "inner",
          List(Mu(TaskF.Leaf(Some(10))))
        )
      )
    )

    val result = scheme.cata(countAlg).apply(tree)
    assertEquals(result, 4) // root + inner branch + 2 leaves = 4 nodes
  }
}
