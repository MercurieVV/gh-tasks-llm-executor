package com.github.mercurievv.ghllm

import cats.{Eval, Functor}
import cats.free.Cofree
import com.github.mercurievv.ghllm.arrow.PrefixKey
import com.github.mercurievv.ghllm.cli.TaskMetadata
import com.github.mercurievv.ghllm.task.TaskF
import com.github.mercurievv.ghllm.task.TaskGraph
import higherkindness.droste.data.Mu

object TaskTree:

  export higherkindness.droste.{Algebra, scheme}
  export higherkindness.droste.data.Mu
  export com.github.mercurievv.ghllm.task.TaskF

  // The payload is this module's own NodeRef: cost estimation folds a node's
  // measured identity, nothing more. Orchestration instantiates the same
  // functor at TaskNode instead - see TaskGraph.
  type Node[A] = TaskF[NodeRef, A]
  type Tree = Mu[Node]
  type AnnotatedTree = Cofree[Node, Attr]

  /** Numeric inputs observed by Stage 0, averaged for a `(tier, runner, model)`.
    *
    * The token counts are already cumulative across the observed turns, so `turnCount` remains a diagnostic coefficient
    * and is not multiplied into the cost a second time.
    */
  final case class Stage0Coefficients(
      inputTokens: Double,
      cachedInputTokens: Double,
      cacheWriteTokens: Double,
      outputTokens: Double,
      turnCount: Double
  )

  /** Prices and cache ratios that turn Stage 0 token coefficients into USD. */
  final case class CostModel(
      inputUsdPerMillionTokens: Double,
      outputUsdPerMillionTokens: Double,
      cacheReadPriceRatio: Double = 0.1,
      cacheWritePriceRatio: Double = 1.25
  ):
    def estimate(coefficients: Stage0Coefficients): Double =
      val inputCost =
        nonNegative(coefficients.inputTokens) * nonNegative(inputUsdPerMillionTokens)
      val cachedInputCost =
        nonNegative(coefficients.cachedInputTokens) *
          nonNegative(inputUsdPerMillionTokens) *
          nonNegative(cacheReadPriceRatio)
      val cacheWriteCost =
        nonNegative(coefficients.cacheWriteTokens) *
          nonNegative(inputUsdPerMillionTokens) *
          nonNegative(cacheWritePriceRatio)
      val outputCost =
        nonNegative(coefficients.outputTokens) * nonNegative(outputUsdPerMillionTokens)
      (inputCost + cachedInputCost + cacheWriteCost + outputCost) / TokensPerMillion

  /** Stable identity of the raw node used to look up its measured profile.
    *
    * `Task` is deliberately shape-blind: a real issue is a branch when it still
    * has open dependencies and a leaf when it does not, and that changes as work
    * lands. Keying the profile on shape would make the same task price
    * differently depending on when it was estimated. `phase` rides along because
    * it is the fallback key — a task nobody has run yet has no measurement of its
    * own, only its phase's.
    */
  enum NodeRef:
    case Branch(name: String)
    case Leaf(task: Option[Int])
    case Task(number: Int, phase: Option[String])

  /** Inputs that are inherited from measurement and prompt assembly, not invented by the fold.
    */
  final case class NodeProfile(
      prefixKey: PrefixKey,
      tier: String,
      coefficients: Stage0Coefficients
  )

  /** Result of folding one subtree.
    *
    * `subtreeUsd` is additive and is used for whole-plan comparisons. `estimatedPerNodeUsd` allocates only this node's
    * shared-prefix cost over its immediate fan-out. Consequently, adding a sibling cannot make the estimate for any
    * node more expensive.
    */
  final case class Cost(
      ownUsd: Double,
      subtreeUsd: Double,
      nodeCount: Int,
      estimatedPerNodeUsd: Double
  )

  /** Attributes attached to every node by [[annotate]]. */
  final case class Attr(prefixKey: PrefixKey, tier: String, cost: Cost)

  def branch(name: String, children: List[Tree]): Tree =
    Mu[Node](TaskF.Branch(NodeRef.Branch(name), children))

  def leaf(task: Option[Int]): Tree =
    Mu[Node](TaskF.Leaf(NodeRef.Leaf(task)))

  /** Retags a tree unfolded from live issues into the identity the cost fold
    * reads. The shape is preserved exactly — including the fact that a diamond
    * dependency appears on both paths, so `subtreeUsd` is work-if-run rather
    * than a deduplicated total.
    */
  def ofTaskGraph(tree: TaskGraph.Tree): Tree =
    TaskF.retag(tree)(nodeRefOf)

  def nodeRefOf(node: TaskNode): NodeRef =
    NodeRef.Task(
      node.issue.number.value,
      TaskMetadata.parse(node.issue.body.value).phase.map(_.trim.toLowerCase).filter(_.nonEmpty)
    )

  /** The plan's decision algebra: `TaskF[Cost] => Cost`. */
  def costAlgebra(
      costModel: CostModel,
      profileFor: NodeRef => NodeProfile
  ): Algebra[Node, Cost] =
    Algebra(node => foldNode(node, costModel, profileFor))

  def estimate(
      tree: Tree,
      costModel: CostModel,
      profileFor: NodeRef => NodeProfile
  ): Cost =
    scheme.cata[Node, Tree, Cost](costAlgebra(costModel, profileFor)).apply(tree)

  /** A second algebra builds the `Cofree` annotation with the exact same cost rule as [[costAlgebra]]. The traversal
    * itself remains Droste's `cata`.
    */
  def annotationAlgebra(
      costModel: CostModel,
      profileFor: NodeRef => NodeProfile
  ): Algebra[Node, AnnotatedTree] =
    Algebra { node =>
      val costNode = summon[Functor[Node]].map(node)(_.head.cost)
      val profile = profileFor(TaskF.payloadOf(node))
      Cofree(
        Attr(profile.prefixKey, profile.tier, foldNode(costNode, costModel, profileFor)),
        Eval.now(node)
      )
    }

  def annotate(
      tree: Tree,
      costModel: CostModel,
      profileFor: NodeRef => NodeProfile
  ): AnnotatedTree =
    scheme.cata[Node, Tree, AnnotatedTree](annotationAlgebra(costModel, profileFor)).apply(tree)

  private val TokensPerMillion = 1000000.0

  private def foldNode(
      node: Node[Cost],
      costModel: CostModel,
      profileFor: NodeRef => NodeProfile
  ): Cost =
    val children = TaskF.childrenOf(node)
    val ownUsd = costModel.estimate(profileFor(TaskF.payloadOf(node)).coefficients)
    val fanOut = children.size.max(1)
    Cost(
      ownUsd = ownUsd,
      subtreeUsd = ownUsd + children.map(_.subtreeUsd).sum,
      nodeCount = 1 + children.map(_.nodeCount).sum,
      estimatedPerNodeUsd = ownUsd / fanOut
    )

  private def nonNegative(value: Double): Double =
    if !value.isFinite || value < 0.0 then 0.0 else value
