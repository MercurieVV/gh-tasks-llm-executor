package com.github.mercurievv.ghllm.task

import cats.Functor

sealed trait TaskF[+A]

object TaskF:
  final case class Branch[A](name: String, children: List[A]) extends TaskF[A]
  final case class Leaf[A](task: Option[Int]) extends TaskF[A]

  given Functor[TaskF] with
    def map[A, B](fa: TaskF[A])(f: A => B): TaskF[B] = fa match
      case Branch(name, children) => Branch(name, children.map(f))
      case Leaf(task)             => Leaf[B](task)
