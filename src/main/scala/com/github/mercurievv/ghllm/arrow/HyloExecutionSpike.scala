package com.github.mercurievv.ghllm.arrow

import com.github.mercurievv.ghllm.*
import com.github.mercurievv.ghllm.cli.*
import com.github.mercurievv.ghllm.task.TaskF
import com.github.mercurievv.ghllm.task.TaskGraph

import cats.Monad
import cats.data.EitherT
import cats.data.Kleisli
import cats.effect.kernel.Async
import cats.syntax.all.*
import higherkindness.droste.AlgebraM
import higherkindness.droste.CoalgebraM
import higherkindness.droste.scheme

/** `RecursiveArrows.executeRecursive` (`BusinessArrows.scala`) re-expressed as a single `scheme.hyloM` over
  * `TaskGraph.Node` instead of hand-rolled `ArrowDefer.fix` + `ArrowTraverse.untilLeft`. Wired in at `Wiring.scala`
  * via [[wire]], replacing `logged.executeRecursive` at both call sites (`runRootOnce`, `runOnce`) - see
  * `BusinessArrows.scala`'s T23 comment for the full history of why this was safe to adopt.
  *
  * Behavioural equivalence to the arrow-composed original is covered by `HyloExecutionSpike.test.scala`: the
  * synthetic-double suite adapted from `RecursiveArrowsSuite`, an integration suite against the real
  * `Impl.checkIfCompleted`/`Impl.collectPendingDependencies`, and a suite proving the traversal is unchanged under
  * `Replayability.ReplayFlow`'s `OptionT` layer.
  *
  * Design:
  *   - `M[A] = EitherT[F, RunSummary, A]`. `Left` always means "the walk is done, this is the final summary" - both
  *     the ordinary success case and every short-circuit case use it, so the outermost caller just needs `.merge`.
  *   - `checkIfCompleted`'s `Left` becomes the coalgebra's own `Left`: droste never invokes the algebra for a node
  *     whose coalgebra step already failed, so an already-closed node is never queried for dependencies or run.
  *   - A child's non-"completed" result stops the fold immediately, the same way `EitherT`'s flatMap-based
  *     `Applicative` skips constructing (never mind running) any later sibling's effect once one child is `Left`.
  *     That is what reproduces `ArrowTraverse.untilLeft`'s "stop at the first incomplete dependency, don't touch the
  *     rest" without writing the stop condition by hand.
  *   - `hasOpenChildren` rides on the node payload (`TaskF`'s `P` slot) because the split-replay decision
  *     (`routeDependencyOutcome`) needs it after the children have already collapsed to a bare `RunSummary`.
  */
object HyloExecutionSpike:

  private final case class NodeInfo(node: TaskNode, hasOpenChildren: Boolean)

  private type Node[A] = TaskF[NodeInfo, A]

  private def splitSummary(node: TaskNode): RunSummary =
    RunSummary(
      status = Status("split"),
      message = Message5(
        s"Task #${node.issue.number} is already split; processed open child tasks before parent replay."
      ),
      task = Some(node.issue)
    )

  private def coalgebra[F[_]: Monad](
      checkIfCompleted: TaskNode => F[Either[RunSummary, TaskNode]],
      collectPendingDependencies: TaskNode => F[DependencyPlan]
  ): CoalgebraM[[A] =>> EitherT[F, RunSummary, A], Node, TaskNode] =
    CoalgebraM { node =>
      EitherT(checkIfCompleted(node)).flatMap { checked =>
        EitherT.liftF[F, RunSummary, DependencyPlan](collectPendingDependencies(checked)).map { plan =>
          val payload = NodeInfo(checked, plan.hasOpenChildren)
          if plan.pending.isEmpty then TaskF.Leaf(payload)
          else TaskF.Branch(payload, plan.pending)
        }
      }
    }

  private def runOrSplit[F[_]: Monad](
      info: NodeInfo,
      claimAndRun: TaskNode => F[RunSummary]
  ): EitherT[F, RunSummary, RunSummary] =
    if info.hasOpenChildren then EitherT.leftT(splitSummary(info.node))
    else
      EitherT.liftF[F, RunSummary, RunSummary](claimAndRun(info.node)).flatMap { summary =>
        if summary.status.value === "completed" then EitherT.rightT(summary)
        else EitherT.leftT(summary)
      }

  private def algebra[F[_]: Monad](
      claimAndRun: TaskNode => F[RunSummary]
  ): AlgebraM[[A] =>> EitherT[F, RunSummary, A], Node, RunSummary] =
    AlgebraM {
      case TaskF.Leaf(info)      => runOrSplit(info, claimAndRun)
      case TaskF.Branch(info, _) => runOrSplit(info, claimAndRun)
    }

  /** Same contract as `RecursiveArrows.executeRecursive`: dependencies (and, for an already-split task, open
    * children) run before the node itself, in order, stopping at the first one that does not close.
    */
  def executeRecursive[F[_]: Monad](
      checkIfCompleted: TaskNode => F[Either[RunSummary, TaskNode]],
      collectPendingDependencies: TaskNode => F[DependencyPlan],
      claimAndRun: TaskNode => F[RunSummary]
  ): TaskNode => F[RunSummary] =
    node =>
      scheme
        .hyloM(algebra[F](claimAndRun), coalgebra[F](checkIfCompleted, collectPendingDependencies))
        .apply(node)
        .value
        .map(_.merge)

  /** Wires this traversal in place of `RecursiveArrows.executeRecursive`'s `ArrowDefer.fix` + `ArrowTraverse.untilLeft`
    * composition, reading `checkIfCompleted`/`collectPendingDependencies`/`claimAndRun` off the given record.
    *
    * Takes the record rather than three loose functions so the caller supplies the already-logged, already
    * replay-resolved `RealArr` fields (e.g. `logged.recursiveArrows` in `Wiring.scala`) - by the time those fields
    * exist, `Replayability.lowerOrRaise` has already folded replay-or-real into them and `withArrowLogging` has
    * already wrapped each one, so this fold inherits both for free. `recordDependencyOutcome`/`routeDependencyOutcome`
    * are intentionally unused here: their logic (stop on a non-"completed" summary; block on `hasOpenChildren`) is
    * folded into this module's algebra directly (see the class doc), so those two fields' own enter/exit trace lines
    * no longer fire for this traversal - the record keeps them for `RecursiveArrows.executeRecursive`'s own tests and
    * for `Functor2K`/`Replayability` derivation, which still walk every field of the record regardless of which
    * method actually calls them.
    */
  def wire[F[_]: Async](
      recursiveArrows: RecursiveArrows[Flow[RunF[F]]]
  ): Flow[RunF[F]][TaskNode, RunSummary] =
    Kleisli(
      executeRecursive[RunF[F]](
        checkIfCompleted = recursiveArrows.checkIfCompleted.run,
        collectPendingDependencies = recursiveArrows.collectPendingDependencies.run,
        claimAndRun = recursiveArrows.claimAndRun.run
      )
    )
