package com.github.mercurievv.ghllm

import higherkindness.droste.data.Mu
import higherkindness.droste.syntax.all._
import higherkindness.droste.Algebra
import com.github.mercurievv.ghllm.task.TaskF
import com.github.mercurievv.ghllm.task.TaskF._

class TaskTreeSuite extends munit.FunSuite {

  test("build a 3-node tree and fold it to a node count with cata") {
    val leaf = Mu[TaskF](Branch("leaf", Nil))
    val left = Mu[TaskF](Branch("left", List(leaf)))
    val tree = Mu[TaskF](Branch("root", List(left)))

    val countAlg: Algebra[TaskF, Int] = Algebra {
      case Branch(_, kids) => kids.sum + 1
      case Leaf(_)         => 1
    }

    val total = tree.cata(countAlg)
    assertEquals(total, 3)
  }
}
