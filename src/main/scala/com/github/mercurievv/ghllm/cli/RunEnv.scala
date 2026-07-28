package com.github.mercurievv.ghllm.cli

import com.github.mercurievv.ghllm.*
import com.github.mercurievv.ghllm.agent.*
import com.github.mercurievv.ghllm.arrow.*
import com.github.mercurievv.ghllm.cli.*
import com.github.mercurievv.ghllm.git.*
import com.github.mercurievv.ghllm.metrics.*

import cats.data.Kleisli
import cats.effect.Ref
import cats.effect.kernel.Sync

/** Ambient state every arrow in a run may read, supplied once when the assembled program is finally applied to its
  * input.
  *
  * `openIssues` is the shared, mutable view of which task issues are still open. It is genuinely run-scoped: the
  * recursive walk removes an issue as it closes, and `UntilClosedArrows.refreshRoot` reseeds the whole map after a
  * split creates issues no earlier snapshot knew about.
  *
  * Before, this `Ref` was allocated in the middle of `executeSelectedCandidates`, which forced the traversal/recursion
  * arrow records to be CONSTRUCTED there too - composition happening at run time, inside a leaf's effect, invisible to
  * arrow logging. Carrying it in the arrow type instead means the whole program is a value assembled once, with nothing
  * to build at run time.
  */
final case class RunEnv[F[_]](
    openIssues: Ref[F, Map[TaskNumber, Issue]]
)

object RunEnv:
  /** Leaf that needs the ambient env in addition to its own input. */
  def read[F[_]: Sync, A, B](use: (RunEnv[F], A) => F[B]): -->[RunF[F], A, B] =
    Kleisli(input => Kleisli(env => use(env, input)))

  /** Leaf that only needs the ambient env. */
  def reads[F[_]: Sync, A, B](use: RunEnv[F] => F[B]): -->[RunF[F], A, B] =
    read((env, _) => use(env))

/** Effect of a running program: an effect `F` that can also read the `RunEnv`.
  *
  * `Kleisli[F, RunEnv[F], *]` has `Sync`/`Async`/`Parallel`/`Defer` instances whenever `F` does, so every existing
  * `[F[_]: Sync]` leaf compiles unchanged at `RunF[F]`; only the handful of leaves that actually touch the env are
  * written against it.
  */
type RunF[F[_]] = [A] =>> Kleisli[F, RunEnv[F], A]

/** The arrow: a Kleisli in whatever effect the program runs in. */
type Flow[F[_]] = [A, B] =>> Kleisli[F, A, B]

/** Infix spelling of `Flow`, for leaf signatures. */
type -->[F[_], A, B] = Kleisli[F, A, B]
