package com.github.mercurievv.ghllm

import higherkindness.droste._
import higherkindness.droste.data._

object TaskTree {

  /** Pattern functor for a task tree node.
    * @tparam A placeholder for child subtrees.
    */
  final case class TaskF[A](label: String, children: List[A])

  /** Functor instance for map over children. */
  given taskFFunctor: Functor[TaskF] = new Functor[TaskF] {
    def map[A, B](fa: TaskF[A])(f: A => B): TaskF[B] =
      fa.copy(children = fa.children.map(f))
  }

  /** Fixed-point carrier. */
  type FixTaskTree = Fix[TaskF]

  /** Cofree carrier – annotated node. */
  type CofreeTask[A] = Cofree[TaskF, A]
}
