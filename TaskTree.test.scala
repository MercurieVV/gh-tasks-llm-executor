package com.github.mercurievv.ghllm.tasktree

import munit.FunSuite
import higherkindness.droste.scheme.cata
import higherkindness.droste.Fix
import higherkindness.droste.Cofree

import TaskF.Node

class CostAlgebraSuite extends FunSuite {

  // Helper to construct a Fix[TaskF] quickly
  private def leaf(work: Double, tier: String): Fix[TaskF] =
    Fix(Node(work, tier, Nil))

  private def branch(work: Double, tier: String, cs: Fix[TaskF]*): Fix[TaskF] =
    Fix(Node(work, tier, cs.toList))

  test("hand‑computed total cost matches algebra fold") {
    // tree:
    //          root (plan, work=10)
    //          /        \
    //  impl(5)          test(3)
    //  work=5            work=3
    val root = branch(10.0, "plan",
      leaf(5.0, "implement"),
      leaf(3.0, "test")
    )
    val expected = 10.0 * 1.0 + 5.0 * 0.3 + 3.0 * 0.1 // 11.8
    val total = cata(CostAlgebra.totalCost).apply(root)
    assertEqualsDouble(total, expected, 0.001)
  }

  test("property: adding a sibling does NOT change a node's own cost") {
    val baseWork  = 10.0
    val tier      = "plan"
    val leafA     = leaf(5.0, "implement")
    val leafB     = leaf(3.0, "test")

    val before    = branch(baseWork, tier, leafA, leafB)

    def rootOwnCost(t: Fix[TaskF]): Double = t.unFix match {
      case Node(w, tr, _) => w * CostCoefficients.tierMultiplier(tr)
    }

    val ownBefore = rootOwnCost(before)

    // add an extra sibling (fan‑out grows)
    val leafC     = leaf(4.0, "test")
    val after     = branch(baseWork, tier, leafA, leafB, leafC)
    val ownAfter  = rootOwnCost(after)

    assertEqualsDouble(ownBefore, ownAfter, 0.0,
      "Root's own cost must be identical after adding a sibling")
  }

  test("annotate tree yields correct per‑node cost estimates") {
    val root = branch(10.0, "plan",
      leaf(5.0, "implement"),
      leaf(3.0, "test")
    )

    val annotated = CostAlgebra.annotate(root)

    // root cost (own)
    assertEqualsDouble(annotated.head.estCost, 10.0 * 1.0, 0.001)

    // children
    val childrenCofree: List[Cofree[TaskF, Attr]] =
      annotated.tailForced match {
        case Node(_, _, kids) => kids
      }

    assertEquals(childrenCofree.size, 2)

    val Vector(c1, c2) = childrenCofree.toVector
    assertEqualsDouble(c1.head.estCost, 5.0 * 0.3, 0.001)   // implement
    assertEqualsDouble(c2.head.estCost, 3.0 * 0.1, 0.001)   // test
  }
}
