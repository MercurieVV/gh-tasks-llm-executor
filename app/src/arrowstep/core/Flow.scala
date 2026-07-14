package arrowstep.core

import cats.{Applicative, Monad}
import cats.arrow.ArrowChoice
import cats.data.Kleisli

/** One typed step of a dialogue pipeline (DESIGN §3).
  *
  * Opaque `Kleisli` so the arrow laws are cats' laws, not ours: the lawful [[Flow.arrowChoice]]
  * instance is inherited wholesale from `Kleisli`, and every combinator below delegates to it.
  *
  *   - `-A` contravariant input, `B` covariant output.
  *   - No `IO`; `F[_]` stays abstract (scala-rules §4, abstraction-first).
  */
opaque type Flow[F[_], -A, B] = Kleisli[F, A, B]

object Flow:

  /** Effectful step: the only door through which `F` may enter a pipeline. */
  def apply[F[_], A, B](run: A => F[B]): Flow[F, A, B] = Kleisli(run)

  /** Pure step — the compile-time purity fence (DESIGN §6). `A => B` admits no effect, so a
    * decision step lifted here provably cannot touch the world.
    */
  def lift[F[_], A, B](f: A => B)(using F: Applicative[F]): Flow[F, A, B] =
    Kleisli(a => F.pure(f(a)))

  /** Identity arrow. */
  def id[F[_], A](using F: Applicative[F]): Flow[F, A, A] =
    Kleisli(F.pure)

  /** The lawful `ArrowChoice`, inherited from `Kleisli` (D8). Provides `+++`, `|||`,
    * `left`/`right` and backs every combinator below.
    */
  given arrowChoice[F[_]](using Monad[F]): ArrowChoice[[a, b] =>> Flow[F, a, b]] =
    Kleisli.catsDataArrowChoiceForKleisli[F]

  extension [F[_], A, B](self: Flow[F, A, B])

    /** Run the step. */
    def run(a: A): F[B] = self.apply(a)

    /** Sequence: `self` then `next`. */
    def >>>[C](next: Flow[F, B, C])(using F: Monad[F]): Flow[F, A, C] =
      arrowChoice[F].andThen(self, next)

    /** Fan-out: feed the same input to both, pair the outputs. */
    def &&&[C](other: Flow[F, A, C])(using F: Monad[F]): Flow[F, A, (B, C)] =
      arrowChoice[F].merge(self, other)

    /** Parallel pairs: run both sides of a tuple independently. */
    def ***[C, D](other: Flow[F, C, D])(using F: Monad[F]): Flow[F, (A, C), (B, D)] =
      arrowChoice[F].split(self, other)

    /** Act on the first component of a pair, carrying the second untouched. */
    def first[C](using F: Monad[F]): Flow[F, (A, C), (B, C)] =
      arrowChoice[F].first(self)

    /** Act on the second component of a pair, carrying the first untouched. */
    def second[C](using F: Monad[F]): Flow[F, (C, A), (C, B)] =
      arrowChoice[F].second(self)
