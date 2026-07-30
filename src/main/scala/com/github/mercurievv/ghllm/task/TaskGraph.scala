package com.github.mercurievv.ghllm.task

import com.github.mercurievv.ghllm.*

import cats.Monad
import cats.syntax.all.*
import higherkindness.droste.CoalgebraM
import higherkindness.droste.data.Mu
import higherkindness.droste.scheme

/** Unfolds the live dependency graph into a task tree.
  *
  * This is the piece that was missing: `TaskF` and the cost algebra existed but
  * nothing ever built a tree from real data, so the whole apparatus was
  * reachable only from tests. With a coalgebra, the shape becomes ordinary
  * data, and which scheme consumes it (`cata`, `para`, `histo`, a fused
  * `hylo`) is a separate, swappable decision.
  *
  * Discovery stays a parameter. Production passes the same GitHub-backed lookup
  * `RecursiveArrows.collectPendingDependencies` uses, so there is one definition
  * of "what must close before this node"; tests pass a pure map.
  */
object TaskGraph:

  type Node[A] = TaskF[TaskNode, A]
  type Tree = Mu[Node]

  /** A node plus the ancestors already expanded on this path.
    *
    * The visited set is not bookkeeping — it is what makes the unfold total. A
    * dependency graph is not guaranteed acyclic (nothing stops two issues
    * listing each other), and `anaM` on a cycle does not terminate. The walking
    * traversal in `RecursiveArrows` never had to care, because it short-circuits
    * on the first dependency that fails to close rather than materialising the
    * structure.
    */
  final case class Seed(node: TaskNode, expanded: Set[TaskNumber])

  def seed(node: TaskNode): Seed = Seed(node, Set.empty)

  def coalgebra[F[_]: Monad](
      pendingOf: TaskNode => F[List[TaskNode]]
  ): CoalgebraM[F, Node, Seed] =
    CoalgebraM { case Seed(node, expanded) =>
      val number = node.issue.number
      if expanded.contains(number) then TaskF.Leaf(node).pure[F].widen[Node[Seed]]
      else
        val nextExpanded = expanded + number
        pendingOf(node).map {
          case Nil => TaskF.Leaf(node)
          case children =>
            TaskF.Branch(node, children.map(Seed(_, nextExpanded)))
        }
    }

  /** Materialises the tree. Prefer [[fold]] when the tree itself is not wanted:
    * `hyloM` fuses unfold and fold and never allocates the intermediate.
    */
  def unfold[F[_]: Monad](
      pendingOf: TaskNode => F[List[TaskNode]]
  ): Seed => F[Tree] =
    scheme.anaM[F, Node, Seed, Tree](coalgebra(pendingOf))
