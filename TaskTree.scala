package com.github.mercurievv.ghllm

import higherkindness.droste._
import higherkindness.droste.data._
import higherkindness.droste.syntax.all._

object TaskTree {

  /** Pattern functor for a task tree node.
    * @tparam A placeholder for child subtrees.
    */
  final case class TaskF[A](label: String, children: List[A])

  /** Functor instance for map over children. */
  given taskFFunctor: Functor[TaskF] = new Functor[TaskF] {
    def map[A, B](fa: TaskF[A])(f: A => B): TaskF[B] =
      fa.copy(children = fa.children.map(f))
  }

  /** Fixed-point carrier. */
  type FixTaskTree = Fix[TaskF]

  /** Cofree carrier – annotated node. */
  type CofreeTask[A] = Cofree[TaskF, A]
}

class TaskTreeSuite extends munit.FunSuite {

  // Helper to create a leaf node.
  private def leaf(label: String): TaskTree.FixTaskTree =
    Fix(TaskTree.TaskF(label, Nil))

  // Helper to create a branch node.
  private def branch(label: String, children: List[TaskTree.FixTaskTree]): TaskTree.FixTaskTree =
    Fix(TaskTree.TaskF(label, children))

  test("build a 3-node tree and fold it to a node count with cata") {
    // Construct a tree: root with two leaf children (total 3 nodes).
    val tree: TaskTree.FixTaskTree = branch("root", List(leaf("a"), leaf("b")))

    // Algebra that counts number of nodes.
    val countAlg: Algebra[TaskTree.TaskF, Int] = Algebra {
      case TaskTree.TaskF(_, children) => 1 + children.sum
    }

    val totalNodes: Int = tree.cata(countAlg)

    assertEquals(totalNodes, 3)
  }
}
