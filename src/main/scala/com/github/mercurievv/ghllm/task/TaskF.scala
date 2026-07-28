package com.github.mercurievv.ghllm.task

import higherkindness.droste.Functor

/** Task‑tree pattern functor for recursive task decomposition.
  * Use with `higherkindness.droste.Fix` to obtain a recursive tree type.
  */
sealed trait TaskF[+A]

object TaskF:

  /** A node with a name and a list of child sub‑tasks. */
  final case class Branch[A](name: String, children: List[A]) extends TaskF[A]

  /** A leaf containing an optional task number. */
  final case class Leaf[A](task: Option[Int]) extends TaskF[A]

  /** Functor instance required by Droste recursion schemes. */
  given Functor[TaskF] with
    def map[A, B](fa: TaskF[A])(f: A => B): TaskF[B] = fa match
      case Branch(name, children) => Branch(name, children.map(f))
      case Leaf(task)             => Leaf(task)
