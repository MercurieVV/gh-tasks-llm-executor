package com.github.mercurievv.ghllm.arrow

import com.github.mercurievv.ghllm.*
import com.github.mercurievv.ghllm.agent.*
import com.github.mercurievv.ghllm.arrow.*
import com.github.mercurievv.ghllm.cli.*
import com.github.mercurievv.ghllm.git.*
import com.github.mercurievv.ghllm.metrics.*

import cats.Defer
import cats.Monad
import cats.MonadError
import cats.Parallel
import cats.data.Kleisli
import cats.syntax.all.*

/** Arrow-level fixed points.
  *
  * A self-referencing arrow (`RecursiveArrows.executeRecursive`, `UntilClosedArrows.runUntilClosed`) cannot be written
  * as a plain `lazy val self = ... self ...`: composing the cycle forces the arrow value while it is still being
  * initialized. `defer` pushes that force behind the arrow's input, which breaks the knot.
  *
  * This used to be a `defer: (=> A --> B) => (A --> B)` FIELD on every recursive arrow record, supplied at each wiring
  * site. As a field it is not of the shape `A --> B`, so `Functor2K`/`Semigroup2K` (and therefore `withArrowLogging`)
  * could not map over those records at all, which is why they had to stay outside `BusinessLogic`. As a capability of
  * the arrow type it costs one given and the records stay mappable.
  */
trait ArrowDefer[-->[_, _]]:
  def defer[A, B](arrow: => A --> B): A --> B

  /** Least fixed point: `build` receives a deferred reference to the arrow being defined.
    */
  def fix[A, B](build: (A --> B) => (A --> B)): A --> B =
    lazy val self: A --> B = build(defer(self))
    self

object ArrowDefer:
  def apply[-->[_, _]](using instance: ArrowDefer[-->]): ArrowDefer[-->] =
    instance

  given kleisli[F[_]: Defer: Monad]: ArrowDefer[[A, B] =>> Kleisli[F, A, B]] with
    def defer[A, B](arrow: => Kleisli[F, A, B]): Kleisli[F, A, B] =
      Kleisli(input => Defer[F].defer(arrow.run(input)))

/** Lifting a single arrow over a list of independent inputs.
  *
  * `all` and `parAll` are the same arrow at two execution strategies, which is what lets the `--parallel` decision be
  * an `ArrowChoice` branch (`BusinessLogic.executeSelectedCandidates`) instead of an `if` inside a leaf's effect.
  * `untilLeft` is the short-circuiting sequential fold used to run a task's dependencies, stopping at the first one
  * that did not close.
  */
trait ArrowTraverse[-->[_, _]]:
  /** Sequential: every input in order, all of them. */
  def all[A, B](one: A --> B): List[A] --> List[B]

  /** Concurrent: every input side by side. */
  def parAll[A, B](one: A --> B): List[A] --> List[B]

  /** Sequential until the first `Left`, which is returned as the result. */
  def untilLeft[A, E](step: A --> Either[E, Unit]): List[A] --> Either[E, Unit]

object ArrowTraverse:
  def apply[-->[_, _]](using instance: ArrowTraverse[-->]): ArrowTraverse[-->] =
    instance

  given kleisli[F[_]: Monad: Parallel: cats.effect.kernel.Concurrent]: ArrowTraverse[[A, B] =>> Kleisli[F, A, B]] with
    def all[A, B](one: Kleisli[F, A, B]): Kleisli[F, List[A], List[B]] =
      Kleisli(inputs => inputs.traverse(one.run))

    def parAll[A, B](one: Kleisli[F, A, B]): Kleisli[F, List[A], List[B]] =
      ParallelArrows.parAll(one)

    def untilLeft[A, E](
        step: Kleisli[F, A, Either[E, Unit]]
    ): Kleisli[F, List[A], Either[E, Unit]] =
      Kleisli { inputs =>
        inputs.foldLeftM[F, Either[E, Unit]](Right(())) {
          case (stopped @ Left(_), _) => stopped.pure[F]
          case (_, input)             => step.run(input)
        }
      }

/** Running an arrow and routing on whether it failed, keeping the input either way.
  *
  * The input has to survive a failure because every recovery in this program needs it: to retry with a stronger runner,
  * to repair a merge conflict and try the merge again, to comment the failure on the right issue. With this, "attempt,
  * then branch" is `attempt(action) >>> (recover ||| continue)` - ordinary `ArrowChoice` - instead of a
  * `handleErrorWith` buried inside a leaf.
  */
trait ArrowAttempt[-->[_, _]]:
  def attempt[A, B](arrow: A --> B): A --> Either[(A, Throwable), (A, B)]

object ArrowAttempt:
  def apply[-->[_, _]](using instance: ArrowAttempt[-->]): ArrowAttempt[-->] =
    instance

  given kleisli[F[_]](using F: MonadError[F, Throwable]): ArrowAttempt[[A, B] =>> Kleisli[F, A, B]] with
    def attempt[A, B](arrow: Kleisli[F, A, B]): Kleisli[F, A, Either[(A, Throwable), (A, B)]] =
      (Kleisli.ask[F, A] &&& arrow.attempt).map {
        case (input, Left(error))  => Left((input, error))
        case (input, Right(value)) => Right((input, value))
      }

/** The shape every repair loop in this program has: attempt an action; on failure decide whether it is repairable; if
  * so repair and attempt again, otherwise re-raise.
  *
  * `routeFailure` is the effectful decision AND the repair - it returns `Right(state)` to go round again (with whatever
  * budget or state the next attempt should carry) and `Left(error)` to give up. The loop itself is `ArrowDefer.fix`,
  * the same mechanism as the recursive task walk, rather than a `def`-recursive method calling itself inside
  * `handleErrorWith`.
  */
object RepairLoop:
  def apply[-->[_, _], A](
      action: A --> Unit,
      routeFailure: (A, Throwable) --> Either[Throwable, A],
      raiseFailure: Throwable --> Unit
  )(using
      arrow: cats.arrow.ArrowChoice[-->],
      defer: ArrowDefer[-->],
      attempt: ArrowAttempt[-->]
  ): A --> Unit =
    defer.fix { self =>
      attempt.attempt(action) >>>
        ((routeFailure >>> (raiseFailure ||| self)) ||| arrow.lift(_ => ()))
    }
