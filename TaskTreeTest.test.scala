package com.github.mercurievv.ghllm

import munit.FunSuite
import com.github.mercurievv.ghllm.TaskTree.*

// The previous test here asserted `t ne null` on a freshly constructed tree.
// That cannot fail: `Mu(...)` either returns a value or throws, so the
// assertion held for every possible definition of `branch` and `leaf` —
// including ones that dropped the children or swapped the payload. These
// read the tree back out instead.
class TaskTreeTestSuite extends FunSuite:

  private def refs(tree: Tree): List[NodeRef] =
    scheme
      .cata[Node, Tree, List[NodeRef]](
        Algebra(node => TaskF.payloadOf(node) :: TaskF.childrenOf(node).flatten)
      )
      .apply(tree)

  test("branch keeps its name, its children, and their order"):
    val tree = branch("root", List(leaf(Some(1)), branch("sub", List(leaf(None)))))

    assertEquals(
      refs(tree),
      List(
        NodeRef.Branch("root"),
        NodeRef.Leaf(Some(1)),
        NodeRef.Branch("sub"),
        NodeRef.Leaf(None)
      )
    )

  test("a childless branch is not silently a leaf"):
    val empty = branch("root", Nil)

    assertEquals(refs(empty), List(NodeRef.Branch("root")))
    assertEquals(refs(leaf(None)), List(NodeRef.Leaf(None)))
    assertNotEquals(refs(empty), refs(leaf(None)))

  test("leaf carries the task number it was given"):
    assertEquals(refs(leaf(Some(42))), List(NodeRef.Leaf(Some(42))))
    assertEquals(refs(leaf(None)), List(NodeRef.Leaf(None)))

  test("depth is preserved, not flattened"):
    val deep = (1 to 5).foldLeft(leaf(Some(0)))((acc, level) => branch(s"level-$level", List(acc)))

    assertEquals(
      refs(deep),
      List(
        NodeRef.Branch("level-5"),
        NodeRef.Branch("level-4"),
        NodeRef.Branch("level-3"),
        NodeRef.Branch("level-2"),
        NodeRef.Branch("level-1"),
        NodeRef.Leaf(Some(0))
      )
    )
