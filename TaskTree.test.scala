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
    PrefixKey(
      runner = s"test-runner-$value",
      model = Some("test-model"),
      worktree = os.pwd
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
    // `profile` gives every node a PrefixKey of its own, so neither child shares
    // the root's cached prefix and the root amortises across nobody.
    assertEqualsDouble(result.estimatedPerNodeUsd, 1.0, 1e-12)

  test("only the children sharing the prefix divide the parent's cost"):
    val root = branch("root", List(leaf(Some(1)), leaf(Some(2)), leaf(Some(3))))
    val shared = prefix("shared")
    val rootProfile =
      NodeProfile(shared, "plan", zeroCoefficients.copy(inputTokens = 1000000.0))
    val profileFor: NodeRef => NodeProfile =
      case NodeRef.Branch("root") => rootProfile
      // Two of the three leaves route to the root's runner; the third does not,
      // so it re-sends the prefix and cannot be counted as amortising it.
      case NodeRef.Leaf(Some(3)) => NodeProfile(prefix("other"), "test", zeroCoefficients)
      case _                     => NodeProfile(shared, "implement", zeroCoefficients)

    val cost = estimate(root, model, profileFor)
    assertEqualsDouble(cost.ownUsd, 1.0, 1e-12)
    assertEqualsDouble(cost.estimatedPerNodeUsd, 0.5, 1e-12)

  test("a fan-out that shares nothing is priced as if it were serial"):
    // The defect this replaced: dividing by the raw fan-out made a plan look
    // cheaper for spreading work across runners, which is when it shares least.
    val root = branch("root", List(leaf(Some(1)), leaf(Some(2))))
    val rootProfile =
      NodeProfile(prefix("root"), "plan", zeroCoefficients.copy(inputTokens = 1000000.0))
    val profileFor: NodeRef => NodeProfile =
      case NodeRef.Branch("root") => rootProfile
      case ref                    => NodeProfile(prefix(ref.toString), "implement", zeroCoefficients)

    val cost = estimate(root, model, profileFor)
    assertEqualsDouble(cost.estimatedPerNodeUsd, cost.ownUsd, 1e-12)

  // The property this replaces asserted that adding a sibling never increases
  // estimated per-node cost. That holds by construction and cannot fail:
  // estimatedPerNodeUsd is ownUsd / the sharing count, and ownUsd does not read
  // children at all, so the numerator is fixed while the denominator only grows.
  // The properties below can fail, and each one names a specific way the fold
  // could be wrong.

  /** Random trees, so the properties are not tested only against fan-outs. */
  private def genTree(depth: Int): Gen[Tree] =
    if depth <= 0 then Gen.choose(0, 50).map(index => leaf(Some(index)))
    else
      Gen.oneOf(
        Gen.choose(0, 50).map(index => leaf(Some(index))),
        for
          width <- Gen.choose(1, 4)
          name <- Gen.choose(0, 50).map(index => s"branch-$index")
          children <- Gen.listOfN(width, genTree(depth - 1))
        yield branch(name, children)
      )

  private def genCoefficients: Gen[Stage0Coefficients] =
    for
      input <- Gen.choose(0.0, 1000000.0)
      cached <- Gen.choose(0.0, 1000000.0)
      written <- Gen.choose(0.0, 1000000.0)
      output <- Gen.choose(0.0, 100000.0)
    yield Stage0Coefficients(input, cached, written, output, 1.0)

  /** Every node priced independently of the tree, keyed by its own NodeRef. */
  private def pricing(seed: Int): NodeRef => NodeProfile =
    ref =>
      val magnitude = math.abs(ref.hashCode % 1000).toDouble * seed.toDouble
      NodeProfile(
        prefix(ref.toString),
        "implement",
        zeroCoefficients.copy(inputTokens = magnitude, outputTokens = magnitude / 2.0)
      )

  property("subtreeUsd is the sum of every node's own cost, counted exactly once"):
    // Cross-checked against an independent fold rather than restating the
    // recursion: this fails on double-counting and on a dropped subtree, which
    // the previous property could not detect.
    forAll(genTree(3), Gen.choose(1, 5)) { (tree, seed) =>
      val profileFor = pricing(seed)
      val total = estimate(tree, model, profileFor).subtreeUsd
      val perNode =
        scheme
          .cata[Node, Tree, List[Double]](
            Algebra(node =>
              model.estimate(profileFor(TaskF.payloadOf(node)).coefficients) ::
                TaskF.childrenOf(node).flatten
            )
          )
          .apply(tree)

      assertEqualsDouble(total, perNode.sum, 1e-9)
      assertEquals(estimate(tree, model, profileFor).nodeCount, perNode.size)
    }

  property("attaching a subtree never lowers the plan's total cost"):
    forAll(genTree(2), genTree(2), Gen.choose(1, 5)) { (tree, extra, seed) =>
      val profileFor = pricing(seed)
      val before = branch("root", List(tree))
      val after = branch("root", List(tree, extra))

      assert(
        estimate(after, model, profileFor).subtreeUsd >=
          estimate(before, model, profileFor).subtreeUsd,
        "adding work reduced the estimate"
      )
    }

  property("per-node allocation neither invents nor loses cost"):
    // The real content of estimatedPerNodeUsd: it splits *this node's* cost
    // across the children that share its prefix. Fails if allocation ever starts
    // folding in children's own cost, and fails if it goes back to dividing by
    // the raw fan-out - `pricing` keys every node differently, so nothing shares.
    forAll(genTree(3), Gen.choose(1, 5)) { (tree, seed) =>
      val profileFor = pricing(seed)
      val cost = estimate(tree, model, profileFor)
      val rootKey = profileFor(TaskF.payloadOf(Mu.un(tree))).prefixKey
      val sharing =
        TaskF
          .childrenOf(Mu.un(tree))
          .count(child => profileFor(TaskF.payloadOf(Mu.un(child))).prefixKey == rootKey)
          .max(1)

      assertEqualsDouble(cost.estimatedPerNodeUsd * sharing, cost.ownUsd, 1e-9)
    }

  property("garbage coefficients cannot produce a garbage estimate"):
    // NaN and negatives are reachable: coefficients are averages over recorded
    // events, and an empty or malformed sample divides by zero.
    val poison = List(Double.NaN, Double.PositiveInfinity, Double.NegativeInfinity, -1.0e9)
    forAll(genTree(2), Gen.oneOf(poison), genCoefficients) { (tree, bad, good) =>
      val profileFor: NodeRef => NodeProfile =
        ref =>
          val coefficients =
            if ref.hashCode % 2 == 0 then good.copy(inputTokens = bad, outputTokens = bad)
            else good
          NodeProfile(prefix(ref.toString), "implement", coefficients)
      val cost = estimate(tree, model, profileFor)

      assert(cost.subtreeUsd.isFinite && cost.subtreeUsd >= 0.0, s"got ${cost.subtreeUsd}")
      assert(cost.ownUsd.isFinite && cost.ownUsd >= 0.0, s"got ${cost.ownUsd}")
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

    assertEquals(annotated.head.cost.prefixKey, rootProfile.prefixKey)
    assertEquals(annotated.head.tier, "plan")
    assertEqualsDouble(annotated.head.cost.subtreeUsd, 1.0, 1e-12)
    annotated.tailForced match
      case TaskF.Branch(_, onlyChild :: Nil) =>
        assertEquals(onlyChild.head.cost.prefixKey, childProfile.prefixKey)
        assertEquals(onlyChild.head.tier, "test")
        assertEqualsDouble(onlyChild.head.cost.ownUsd, 0.5, 1e-12)
      case other => fail(s"expected one annotated child, got $other")

  test("cata over a simple tree"):
    // Deliberately instantiated at a payload of its own rather than at
    // TaskTree.Node: the functor is payload-generic, and a fold that only ever
    // typechecks at one payload would not have shown that.
    type Labelled[A] = TaskF[String, A]

    val countAlg: Algebra[Labelled, Int] = Algebra {
      case TaskF.Leaf(_)             => 1
      case TaskF.Branch(_, children) => children.sum + 1
    }

    val tree: Fix[Labelled] = Fix[Labelled](
      TaskF.Branch(
        "root",
        List(
          Fix[Labelled](TaskF.Leaf("first")),
          Fix[Labelled](
            TaskF.Branch(
              "inner",
              List(Fix[Labelled](TaskF.Leaf("second")))
            )
          )
        )
      )
    )

    val result = scheme.cata(countAlg).apply(tree)
    assertEquals(result, 4) // root + inner branch + 2 leaves = 4 nodes
