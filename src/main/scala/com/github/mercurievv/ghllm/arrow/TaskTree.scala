package com.github.mercurievv.ghllm.arrow

import cats.Functor

object TaskTree {

  /** Pattern functor for a task tree node.
    *
    * @tparam A placeholder for child subtrees.
    */
  final case class TaskTreeF[A](label: String, children: List[A])

  /** Functor instance for `TaskTreeF`. */
  given taskTreeFFunctor: Functor[TaskTreeF] = new Functor[TaskTreeF] {
    def map[A, B](fa: TaskTreeF[A])(f: A => B): TaskTreeF[B] =
      fa.copy(children = fa.children.map(f))
  }
}
