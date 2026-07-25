import cats.Parallel
import cats.data.Kleisli
import cats.syntax.parallel.*

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

  /** `List`-shaped generalization of `parSplit`/`parMerge` past fixed arity: lifts one arrow over an independent-input
    * list so every element runs concurrently, through the same `Parallel[F]` instance.
    */
  def parAll[F[_]: Parallel, A, B](one: Kleisli[F, A, B]): Kleisli[F, List[A], List[B]] =
    Kleisli(inputs => inputs.parTraverse(one.run))
