package com.github.mercurievv.ghllm

import higherkindness.droste.data.Mu
import higherkindness.droste.syntax.mu._
import higherkindness.droste.Algebra
import com.github.mercurievv.ghllm.task.TaskF
import com.github.mercurievv.ghllm.task.TaskF._

class TaskTreeTest extends munit.FunSuite {

  test("3-node tree folds to node count with cata") {
    val leafA = Mu[TaskF](Branch("a", Nil))
    val leafB = Mu[TaskF](Branch("b", Nil))
    val root  = Mu[TaskF](Branch("root", List(leafA, leafB)))

    val countAlgebra: Algebra[TaskF, Int] = Algebra {
      case Branch(_, children) => children.sum + 1
      case Leaf(_)             => 1
    }

    val total = root.cata(countAlgebra)
    assertEquals(total, 3)
  }
}
