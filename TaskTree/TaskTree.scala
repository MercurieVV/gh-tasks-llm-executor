package com.github.mercurievv.ghllm.tasktree

import higherkindness.droste._
import higherkindness.droste.syntax.all._

/** Pattern functor for the task tree (one node’s data plus `List[A]` children). */
sealed trait TaskF[+A]

object TaskF {
  final case class Node[+A](work: Double, tier: String, children: List[A]) extends TaskF[A]

  // Functor evidence required by Droste
  implicit val functor: Functor[TaskF] = new Functor[TaskF] {
    def map[A, B](fa: TaskF[A])(f: A => B): TaskF[B] =
      fa match {
        case Node(w, t, cs) => Node(w, t, cs.map(f))
      }
  }
}

/** Coefficients taken from Stage 0 metrics (placeholder values). */
object CostCoefficients {
  /** Maps a runner tier to its relative cost multiplier.
    * Lower tiers cost less; the numbers are illustrative and will
    * be replaced with actual metric‑driven values. */
  def tierMultiplier(tier: String): Double = tier.toLowerCase match {
    case "plan"      => 1.0
    case "implement" => 0.3
    case "test"      => 0.1
    case other       => 0.5 // unknown tier
  }
}

/** Data attached to each node in an annotated tree (Cofree). */
final case class Attr(prefixKey: String, tier: String, estCost: Double)

object CostAlgebra {

  import TaskF.Node

  /** Algebra that computes a node’s own estimated cost,
    * ignoring children (stable under fan‑out changes). */
  val ownCost: Algebra[TaskF, Double] = Algebra {
    case Node(w, t, _) => w * CostCoefficients.tierMultiplier(t)
  }

  /** Algebra that sums the own cost of a node plus the costs of all children,
    * giving the total estimated cost of a subtree. */
  val totalCost: Algebra[TaskF, Double] = Algebra {
    case Node(w, t, childCosts) =>
      w * CostCoefficients.tierMultiplier(t) + childCosts.sum
  }

  /** Annotate each node with its `PrefixKey`, tier, and estimated own cost,
    * yielding a Cofree tree.  The cost is the same value as `ownCost`. */
  def annotate(tree: Fix[TaskF]): Cofree[TaskF, Attr] = {
    def go(t: Fix[TaskF]): Cofree[TaskF, Attr] = {
      val Node(w, tier, childrenFix) = t.unFix
      val childAnn = childrenFix.map(go)
      val cost = w * CostCoefficients.tierMultiplier(tier)
      val pk   = s"key-$w-$tier"
      Cofree(
        head = Attr(pk, tier, cost),
        tail = Eval.now(Node(cost, tier, childAnn))
      )
    }
    go(tree)
  }
}
