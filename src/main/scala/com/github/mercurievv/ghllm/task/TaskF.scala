package com.github.mercurievv.ghllm.task

import cats.Applicative
import cats.Eval
import cats.Traverse
import cats.syntax.all.*

/** Pattern functor for a task tree.
  *
  * `P` is the node's own payload, `A` the placeholder for child subtrees.
  *
  * The payload is a parameter rather than a fixed type because the same shape
  * has to carry different things depending on what is being folded: cost
  * estimation wants a node's measured identity, orchestration wants the live
  * `TaskNode` (the issue plus the run context that travels down the walk).
  * Pinning it to one of those, as the original `Leaf(task: Option[Int])` did,
  * left the functor unable to carry a real task at all.
  *
  * `Traverse` rather than plain `Functor`: droste's effectful schemes (`anaM`,
  * `hyloM`) require it, and unfolding a task tree means asking GitHub for each
  * node's dependencies, which is an effect.
  */
sealed trait TaskF[+P, +A]

object TaskF:
  final case class Branch[+P, +A](payload: P, children: List[A]) extends TaskF[P, A]
  final case class Leaf[+P](payload: P) extends TaskF[P, Nothing]

  def payloadOf[P, A](node: TaskF[P, A]): P = node match
    case Branch(payload, _) => payload
    case Leaf(payload)      => payload

  def childrenOf[P, A](node: TaskF[P, A]): List[A] = node match
    case Branch(_, children) => children
    case Leaf(_)             => Nil

  given [P]: Traverse[[A] =>> TaskF[P, A]] with
    def traverse[G[_]: Applicative, A, B](fa: TaskF[P, A])(f: A => G[B]): G[TaskF[P, B]] =
      fa match
        case Branch(payload, children) =>
          children.traverse(f).map(Branch(payload, _))
        case leaf @ Leaf(_) =>
          Applicative[G].pure(leaf)

    def foldLeft[A, B](fa: TaskF[P, A], b: B)(f: (B, A) => B): B =
      childrenOf(fa).foldLeft(b)(f)

    def foldRight[A, B](fa: TaskF[P, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      Traverse[List].foldRight(childrenOf(fa), lb)(f)
