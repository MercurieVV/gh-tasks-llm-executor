package com.github.mercurievv.ghllm.arrow

import com.github.mercurievv.ghllm.*
import com.github.mercurievv.ghllm.agent.*
import com.github.mercurievv.ghllm.arrow.*
import com.github.mercurievv.ghllm.cli.*
import com.github.mercurievv.ghllm.git.*
import com.github.mercurievv.ghllm.metrics.*

import cats.Parallel
import cats.data.Kleisli
import cats.effect.kernel.Concurrent
import cats.syntax.parallel.*
import cats.effect.syntax.all.*

/** Parallel analogs of `ArrowChoice`'s `***`/`&&&` for Kleisli arrows backed by an effect `F` with a `Parallel[F]`
  * instance (e.g. `cats.effect.IO`).
  *
  * Kleisli's default `ArrowChoice` instance implements `***`/`&&&` via `first >>> second`, i.e. sequentially through
  * `F`'s `Monad`. These variants run both sides concurrently through `F`'s `Parallel` instance instead.
  * `ArrowTraverse.parAll` is backed by `parAll` here, and `BusinessLogic.executeSelectedCandidates` selects it - as one
  * branch of an `ArrowChoice`, against the sequential `ArrowTraverse.all` - to run independent root task candidates
  * side by side when `--parallel` is set.
  */
object ParallelArrows:

  /** Parallel `***`: runs two independent arrows on two independent inputs concurrently. */
  def parSplit[F[_]: Parallel, A, B, C, D](
      left: Kleisli[F, A, B],
      right: Kleisli[F, C, D]
  ): Kleisli[F, (A, C), (B, D)] =
    Kleisli { case (a, c) => (left.run(a), right.run(c)).parTupled }

  /** Parallel `&&&`: runs two independent arrows on the same input concurrently. */
  def parMerge[F[_]: Parallel, A, B, C](
      left: Kleisli[F, A, B],
      right: Kleisli[F, A, C]
  ): Kleisli[F, A, (B, C)] =
    Kleisli(a => (left.run(a), right.run(a)).parTupled)

  /** Root candidates commonly hit the same shared resources (issue tracker rate limits, vendor budgets) - running
    * every candidate side by side risks a thundering herd, so this caps how many run at once rather than letting
    * `parAll` fan out unbounded.
    */
  val MaxParallelism: Int = 2

  /** `List`-shaped generalization of `parSplit`/`parMerge` past fixed arity: lifts one arrow over an independent-input
    * list so every element runs concurrently (up to `MaxParallelism` at a time), through the same `Concurrent[F]`
    * instance.
    */
  def parAll[F[_]: Concurrent, A, B](one: Kleisli[F, A, B]): Kleisli[F, List[A], List[B]] =
    Kleisli(inputs => inputs.parTraverseN(MaxParallelism)(one.run))
