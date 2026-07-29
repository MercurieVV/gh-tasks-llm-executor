//> using test.dependency org.scalameta::munit-scalacheck:1.3.0

package com.github.mercurievv.ghllm

import com.github.mercurievv.ghllm.TaskTree.*
import com.github.mercurievv.ghllm.TaskTree.NodeRef
import com.github.mercurievv.ghllm.arrow.PrefixKey
import higherkindness.droste.data.Fix
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

class TaskTreeSuite extends ScalaCheckSuite:

  private val model =
    CostModel(
      inputUsdPerMillionTokens = 1.0,
      outputUsdPerMillionTokens = 10.0
    )

  private val zeroCoefficients =
    Stage0Coefficients(
      inputTokens = 0.0,
      cachedInputTokens = 0.0,
      cacheWriteTokens = 0.0,
      outputTokens = 0.0,
      turnCount = 1.0
    )

  private def prefix(value: String): PrefixKey =
    PrefixKey.of(
      runner = "test-runner",
      model = Some("test-model"),
      worktree = os.pwd,
      layer0 = "system",
      layer1 = "repo",
      layer2 = value
    )

  private def profile(
      node: NodeRef,
      tier: String,
      coefficients: Stage0Coefficients
  ): (NodeRef, NodeProfile) =
    node -> NodeProfile(prefix(node.toString), tier, coefficients)

  test("a known small tree folds to the hand-computed cost"):
    val root = branch(
      "root",
      List(
        leaf(Some(1)),
        leaf(Some(2))
      )
    )
    val profiles = Map(
      profile(
        NodeRef.Branch("root"),
        "plan",
        zeroCoefficients.copy(inputTokens = 1000000.0)
      ),
      profile(
        NodeRef.Leaf(Some(1)),
        "implement",
        zeroCoefficients.copy(outputTokens = 100000.0)
      ),
      profile(
        NodeRef.Leaf(Some(2)),
        "test",
        zeroCoefficients.copy(cachedInputTokens = 1000000.0)
      )
    )
    val fallback = NodeProfile(prefix("fallback"), "unknown", zeroCoefficients)
    val result = estimate(root, model, node => profiles.getOrElse(node, fallback))

    assertEqualsDouble(result.ownUsd, 1.0, 1e-12)
    assertEqualsDouble(result.subtreeUsd, 2.1, 1e-12)
    assertEquals(result.nodeCount, 3)
    assertEqualsDouble(result.estimatedPerNodeUsd, 0.5, 1e-12)

  property("adding a sibling to a fan-out never increases estimated per-node cost"):
    forAll(Gen.choose(0.0, 10000000.0), Gen.choose(1, 100)) { (inputTokens, siblingCount) =>
      val rootProfile =
        NodeProfile(
          prefix("fan-out"),
          "implement",
          zeroCoefficients.copy(inputTokens = inputTokens)
        )
      val leafProfile = NodeProfile(prefix("leaf"), "test", zeroCoefficients)
      val profileFor: NodeRef => NodeProfile =
        case NodeRef.Branch("fan-out") => rootProfile
        case _                         => leafProfile
      val siblings = List.tabulate(siblingCount)(index => leaf(Some(index)))
      val before = branch("fan-out", siblings)
      val after = branch("fan-out", siblings :+ leaf(Some(Int.MaxValue)))
      val beforeCost = estimate(before, model, profileFor)
      val afterCost = estimate(after, model, profileFor)

      assert(
        afterCost.estimatedPerNodeUsd <= beforeCost.estimatedPerNodeUsd,
        s"${afterCost.estimatedPerNodeUsd} was greater than ${beforeCost.estimatedPerNodeUsd}"
      )
    }

  test("cata annotation carries each node's PrefixKey, tier, and cost"):
    val child = leaf(Some(7))
    val root = branch("root", List(child))
    val rootProfile =
      NodeProfile(
        prefix("root"),
        "plan",
        zeroCoefficients.copy(inputTokens = 500000.0)
      )
    val childProfile =
      NodeProfile(
        prefix("child"),
        "test",
        zeroCoefficients.copy(outputTokens = 50000.0)
      )
    val profileFor: NodeRef => NodeProfile =
      case NodeRef.Branch("root") => rootProfile
      case _                      => childProfile
    val annotated = annotate(root, model, profileFor)

    assertEquals(annotated.head.prefixKey, rootProfile.prefixKey)
    assertEquals(annotated.head.tier, "plan")
    assertEqualsDouble(annotated.head.cost.subtreeUsd, 1.0, 1e-12)
    annotated.tailForced match
      case TaskF.Branch(_, onlyChild :: Nil) =>
        assertEquals(onlyChild.head.prefixKey, childProfile.prefixKey)
        assertEquals(onlyChild.head.tier, "test")
        assertEqualsDouble(onlyChild.head.cost.ownUsd, 0.5, 1e-12)
      case other => fail(s"expected one annotated child, got $other")

  test("cata over a simple tree"):
    val countAlg: Algebra[TaskF, Int] = Algebra {
      case TaskF.Leaf(_)             => 1
      case TaskF.Branch(_, children) => children.sum + 1
    }

    val tree: Fix[TaskF] = Fix(
      TaskF.Branch(
        "root",
        List(
          Fix(TaskF.Leaf(Some(5))),
          Fix(
            TaskF.Branch(
              "inner",
              List(Fix(TaskF.Leaf(Some(10))))
            )
          )
        )
      )
    )

    val result = scheme.cata(countAlg).apply(tree)
    assertEquals(result, 4) // root + inner branch + 2 leaves = 4 nodes
