package com.github.mercurievv.ghllm.task

import com.github.mercurievv.ghllm.*
import com.github.mercurievv.ghllm.agent.*
import com.github.mercurievv.ghllm.arrow.*
import com.github.mercurievv.ghllm.cli.*
import com.github.mercurievv.ghllm.git.*
import com.github.mercurievv.ghllm.metrics.*

import cats.effect.IO
import cats.effect.Ref
import higherkindness.droste.Algebra
import higherkindness.droste.data.Mu
import higherkindness.droste.scheme
import munit.CatsEffectSuite

class TaskGraphSuite extends CatsEffectSuite:

  private val context = RunContext(os.pwd, AgentInventory(Nil), None)

  private def issue(number: Int, body: String = "") =
    Issue(TaskNumber(number), IssueTitle(s"Task $number"), IssueBody(body), State("open"))

  private def node(number: Int) = TaskNode(context, issue(number))

  /** Children by issue number, as a pure stand-in for the GitHub lookup. */
  private def pendingFrom(edges: Map[Int, List[Int]]): TaskNode => IO[List[TaskNode]] =
    task => IO.pure(edges.getOrElse(task.issue.number.value, Nil).map(node))

  private val countNodes: Algebra[TaskGraph.Node, Int] =
    Algebra(n => 1 + TaskF.childrenOf(n).sum)

  private def numbersOf(tree: TaskGraph.Tree): List[Int] =
    scheme
      .cata[TaskGraph.Node, TaskGraph.Tree, List[Int]](
        Algebra(n => TaskF.payloadOf(n).issue.number.value :: TaskF.childrenOf(n).flatten)
      )
      .apply(tree)

  private def unfold(edges: Map[Int, List[Int]], root: Int = 1): IO[TaskGraph.Tree] =
    TaskGraph.unfold[IO](pendingFrom(edges))(TaskGraph.seed(node(root)))

  test("a node with no pending dependencies unfolds to a leaf"):
    unfold(Map.empty).map { tree =>
      Mu.un(tree) match
        case TaskF.Leaf(payload) => assertEquals(payload.issue.number.value, 1)
        case other               => fail(s"expected a leaf, got $other")
    }

  test("dependencies become children, in the order discovery returned them"):
    unfold(Map(1 -> List(2, 3))).map { tree =>
      Mu.un(tree) match
        case TaskF.Branch(payload, children) =>
          assertEquals(payload.issue.number.value, 1)
          assertEquals(children.map(child => TaskF.payloadOf(Mu.un(child)).issue.number.value), List(2, 3))
        case other => fail(s"expected a branch, got $other")
    }

  test("the unfold descends transitively"):
    unfold(Map(1 -> List(2), 2 -> List(3), 3 -> List(4))).map { tree =>
      assertEquals(scheme.cata[TaskGraph.Node, TaskGraph.Tree, Int](countNodes).apply(tree), 4)
      assertEquals(numbersOf(tree), List(1, 2, 3, 4))
    }

  test("a cycle terminates instead of unfolding forever"):
    // Nothing stops two issues listing each other as dependencies. The walking
    // traversal never had to care because it short-circuits; materialising the
    // shape does, and anaM on a cycle would not terminate.
    unfold(Map(1 -> List(2), 2 -> List(1))).map { tree =>
      assertEquals(numbersOf(tree), List(1, 2, 1))
      // The repeat is a leaf: the walk stopped there rather than re-expanding.
      assertEquals(scheme.cata[TaskGraph.Node, TaskGraph.Tree, Int](countNodes).apply(tree), 3)
    }

  test("a self-dependency terminates too"):
    unfold(Map(1 -> List(1))).map { tree =>
      assertEquals(numbersOf(tree), List(1, 1))
    }

  test("a diamond expands both paths — this is a tree, not a DAG"):
    // Recorded deliberately: 4 is reached via both 2 and 3 and is unfolded
    // twice. Cost folds must therefore treat subtreeUsd as work-if-run, not as
    // a deduplicated total.
    unfold(Map(1 -> List(2, 3), 2 -> List(4), 3 -> List(4))).map { tree =>
      assertEquals(numbersOf(tree), List(1, 2, 4, 3, 4))
    }

  test("discovery is only asked once per node on a path"):
    Ref[IO].of(List.empty[Int]).flatMap { asked =>
      val edges = Map(1 -> List(2), 2 -> List(1))
      val recording: TaskNode => IO[List[TaskNode]] =
        task => asked.update(_ :+ task.issue.number.value) *> pendingFrom(edges)(task)
      TaskGraph
        .unfold[IO](recording)(TaskGraph.seed(node(1)))
        .flatMap(_ => asked.get)
        .map(seen => assertEquals(seen, List(1, 2)))
    }

class TaskGraphProductionSourceSuite extends CatsEffectSuite:

  private val context = RunContext(os.pwd, AgentInventory(Nil), None)

  test("pendingDependencies is the same definition the walking traversal uses"):
    // The point of deriving it rather than restating it: if these two ever
    // disagree, the tree describes a different graph than the one that runs.
    val root = Issue(TaskNumber(1), IssueTitle("Root"), IssueBody("Depends on #2"), State("open"))
    val dependency = Issue(TaskNumber(2), IssueTitle("Dep"), IssueBody(""), State("open"))
    val node = TaskNode(context, root)

    for
      issues <- Ref[IO].of(Map(TaskNumber(2) -> dependency))
      env <- RunEnv.create[IO](issues)
      viaPlan <- Impl.collectPendingDependencies[IO].run(node).run(env)
      viaAdapter <- Impl.pendingDependencies[IO](node).run(env)
    yield
      assertEquals(viaAdapter.map(_.issue.number), List(TaskNumber(2)))
      assertEquals(viaAdapter.map(_.issue.number), viaPlan.pending.map(_.issue.number))
