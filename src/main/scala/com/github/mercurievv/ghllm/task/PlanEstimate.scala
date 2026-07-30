package com.github.mercurievv.ghllm.task

import com.github.mercurievv.ghllm.*
import com.github.mercurievv.ghllm.TaskTree.AnnotatedTree
import com.github.mercurievv.ghllm.TaskTree.CostModel
import com.github.mercurievv.ghllm.TaskTree.NodeProfile
import com.github.mercurievv.ghllm.TaskTree.NodeRef
import com.github.mercurievv.ghllm.arrow.Impl
import com.github.mercurievv.ghllm.cli.RunF

import cats.effect.kernel.Sync
import cats.syntax.all.*

/** Prices a real plan: discover the dependency tree, retag it to cost
  * identities, fold it with measured coefficients, and print the annotation.
  *
  * This is the whole point of the recursion-scheme apparatus being reachable.
  * Which scheme produces the annotation is a one-line change here — `cata` today,
  * `para` if a node's cost ever needs its own children's shape, `histo` if it
  * needs their history — and nothing about discovery or orchestration moves.
  */
object PlanEstimate:

  def annotate[F[_]: Sync](
      node: TaskNode,
      costModel: CostModel,
      profileFor: NodeRef => NodeProfile
  ): RunF[F][AnnotatedTree] =
    Impl
      .taskTree[F](node)
      .map(graph => TaskTree.annotate(TaskTree.ofTaskGraph(graph), costModel, profileFor))

  def render(tree: AnnotatedTree, costModel: CostModel): String =
    val header =
      f"Plan estimate at $$${costModel.inputUsdPerMillionTokens}%.2f/Mtok in, " +
        f"$$${costModel.outputUsdPerMillionTokens}%.2f/Mtok out"
    val total = tree.head.cost
    val summary =
      f"${total.nodeCount} nodes, subtree $$${total.subtreeUsd}%.4f " +
        "(work-if-run: a dependency reached twice is counted twice)"
    (header :: summary :: "" :: lines(tree, 0)).mkString("\n")

  private def lines(tree: AnnotatedTree, depth: Int): List[String] =
    val node = tree.tailForced
    val attr = tree.head
    val label = TaskF.payloadOf(node) match
      case NodeRef.Task(number, phase) => s"#$number${phase.fold("")(p => s" [$p]")}"
      case other                       => other.toString
    val own = f"$$${attr.cost.ownUsd}%.4f"
    val subtree = f"$$${attr.cost.subtreeUsd}%.4f"
    // Printed because it is the only number that shows whether the plan's fan-out
    // actually shares a cached prefix: when it equals `own`, no child does.
    val perNode = f"$$${attr.cost.estimatedPerNodeUsd}%.4f"
    val line =
      s"${"  " * depth}$label  own=$own per-node=$perNode subtree=$subtree tier=${attr.tier}"
    line :: TaskF.childrenOf(node).flatMap(child => lines(child, depth + 1))
