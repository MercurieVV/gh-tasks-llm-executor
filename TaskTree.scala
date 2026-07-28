package com.github.mercurievv.ghllm.arrow

import cats.Functor

object TaskTree {

  final case class TaskTreeF[A](label: String, children: List[A])

  given Functor[TaskTreeF] = new Functor[TaskTreeF] {
    def map[A, B](fa: TaskTreeF[A])(f: A => B): TaskTreeF[B] =
      fa.copy(children = fa.children.map(f))
  }

}
