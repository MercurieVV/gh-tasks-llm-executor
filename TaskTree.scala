package com.github.mercurievv.ghllm

import higherkindness.droste.{scheme, Algebra}
import higherkindness.droste.data.Mu
import com.github.mercurievv.ghllm.task.TaskF

object TaskTree:

  export higherkindness.droste.{ scheme, Algebra }
  export higherkindness.droste.data.Mu
  export com.github.mercurievv.ghllm.task.TaskF

  type Tree = Mu[TaskF]

  def branch(name: String, children: List[Tree]): Tree =
    Mu(TaskF.Branch(name, children))

  def leaf(task: Option[Int]): Tree =
    Mu(TaskF.Leaf(task))
