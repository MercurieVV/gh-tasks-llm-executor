package com.github.mercurievv.ghllm.arrow

import com.github.mercurievv.ghllm.*
import com.github.mercurievv.ghllm.agent.*
import com.github.mercurievv.ghllm.cli.*
import com.github.mercurievv.ghllm.git.*

import cats.data.Kleisli
import cats.data.OptionT
import cats.effect.IO
import cats.effect.Ref
import cats.syntax.all.*
import munit.CatsEffectSuite

/** Same two cases as `RecursiveArrowsSuite` in `BusinessLogic.test.scala`, run against `HyloExecutionSpike` instead of
  * `RecursiveArrows.executeRecursive`, to prove the `hyloM`-based traversal is behaviourally equivalent before anything
  * real switches over.
  */
class HyloExecutionSpikeSuite extends CatsEffectSuite:
  private val context = RunContext(os.pwd, AgentInventory(Nil), None)
  private def issue(number: Int) =
    Issue(TaskNumber(number), IssueTitle(s"Task $number"), IssueBody(""), State("open"))
  private def summary(message: String, task: Issue) =
    RunSummary(status = Status("completed"), message = Message5(message), task = Some(task))

  test("executeRecursive runs a node only after its dependencies have run"):
    val root = issue(1)
    val dependency = issue(2)

    Ref[IO].of(List.empty[TaskNumber]).flatMap { claimed =>
      val run = HyloExecutionSpike.executeRecursive[IO](
        checkIfCompleted = node => IO.pure(Right(node)),
        collectPendingDependencies = node =>
          val pending =
            if node.issue.number === root.number then List(TaskNode(context, dependency))
            else Nil
          IO.pure(DependencyPlan(node, pending, hasOpenChildren = false))
        ,
        claimAndRun = node => claimed.update(_ :+ node.issue.number).as(summary("ran", node.issue))
      )

      run(TaskNode(context, root)).flatMap { result =>
        claimed.get.map { order =>
          assertEquals(result, summary("ran", root))
          assertEquals(order, List(dependency.number, root.number))
        }
      }
    }

  test("executeRecursive stops at the first dependency that did not close"):
    val root = issue(1)
    val blocked =
      RunSummary(status = Status("split"), message = Message5("still open"), task = None)

    val run = HyloExecutionSpike.executeRecursive[IO](
      checkIfCompleted = node => IO.pure(Right(node)),
      collectPendingDependencies = node =>
        val pending =
          if node.issue.number === root.number then List(TaskNode(context, issue(2)))
          else Nil
        IO.pure(DependencyPlan(node, pending, hasOpenChildren = false))
      ,
      claimAndRun = node =>
        if node.issue.number === root.number then IO.raiseError(AssertionError("root must not run"))
        else IO.pure(blocked)
    )

    run(TaskNode(context, root)).map(result => assertEquals(result, blocked))

/** Same traversal, but driven by the real `Impl.checkIfCompleted`/`Impl.collectPendingDependencies` - real dependency
  * text parsing (`GitHub.getDependencies`), real split/open-children detection (`GitHub.parentIds`), real cache-peer
  * TTL grouping - against `RunEnv`'s in-memory issue map instead of a hand-rolled `DependencyPlan` double. Only
  * `claimAndRun` stays a test double: exercising the real one means a worktree, a git repo and an agent process, which
  * `RecursiveArrowsSuite` itself never does either.
  */
class HyloExecutionSpikeIntegrationSuite extends CatsEffectSuite:
  private val context = RunContext(os.pwd, AgentInventory(Nil), None)
  private def issue(number: Int, body: String = "", state: String = "open") =
    Issue(TaskNumber(number), IssueTitle(s"Task $number"), IssueBody(body), State(state))
  private def summary(message: String, task: Issue) =
    RunSummary(status = Status("completed"), message = Message5(message), task = Some(task))

  private def envOf(issues: List[Issue]): IO[RunEnv[IO]] =
    Ref[IO].of(issues.map(i => i.number -> i).toMap).flatMap(RunEnv.create[IO])

  private def claimAndRunRecording(claimed: Ref[IO, List[TaskNumber]]): TaskNode => RunF[IO][RunSummary] =
    node => Kleisli.liftF(claimed.update(_ :+ node.issue.number).as(summary("ran", node.issue)))

  test("a real chain of #-referenced dependencies runs leaf-first"):
    val leaf = issue(3)
    val middle = issue(2, "Depends on #3")
    val root = issue(1, "Depends on #2")

    for
      claimed <- Ref[IO].of(List.empty[TaskNumber])
      env <- envOf(List(root, middle, leaf))
      run = HyloExecutionSpike.executeRecursive[RunF[IO]](
        checkIfCompleted = Impl.checkIfCompleted[RunF[IO]].run,
        collectPendingDependencies = Impl.collectPendingDependencies[IO].run,
        claimAndRun = claimAndRunRecording(claimed)
      )
      result <- run(TaskNode(context, root)).run(env)
      order <- claimed.get
    yield
      assertEquals(result, summary("ran", root))
      assertEquals(order, List(leaf.number, middle.number, root.number))

  test("a closed dependency is skipped without calling claimAndRun on it"):
    val closedDep = issue(2, state = "closed")
    val root = issue(1, "Depends on #2")

    for
      claimed <- Ref[IO].of(List.empty[TaskNumber])
      // The closed dependency is absent from openIssues, exactly as `loadOpenIssues`
      // seeds it in production: collectPendingDependencies only returns siblings
      // still present in the map.
      env <- envOf(List(root))
      run = HyloExecutionSpike.executeRecursive[RunF[IO]](
        checkIfCompleted = Impl.checkIfCompleted[RunF[IO]].run,
        collectPendingDependencies = Impl.collectPendingDependencies[IO].run,
        claimAndRun = claimAndRunRecording(claimed)
      )
      result <- run(TaskNode(context, root)).run(env)
      order <- claimed.get
    yield
      assertEquals(result, summary("ran", root))
      assertEquals(order, List(root.number))

  test("a task already split with open children is not run directly; the walk reports split"):
    val child = issue(2, "Parent: #1")
    val root = issue(1, "Execution: split")

    for
      claimed <- Ref[IO].of(List.empty[TaskNumber])
      env <- envOf(List(root, child))
      run = HyloExecutionSpike.executeRecursive[RunF[IO]](
        checkIfCompleted = Impl.checkIfCompleted[RunF[IO]].run,
        collectPendingDependencies = Impl.collectPendingDependencies[IO].run,
        claimAndRun = claimAndRunRecording(claimed)
      )
      result <- run(TaskNode(context, root)).run(env)
      order <- claimed.get
    yield
      assertEquals(result.status, Status("split"))
      // The child ran (it has no dependencies/children of its own), the parent did not.
      assertEquals(order, List(child.number))

/** `HyloExecutionSpike.executeRecursive` needs only `Monad[F]`, not `Sync[F]` - checked by instantiating it at `F =
  * OptionT[IO, *]`, the same effect `Replayability.ReplayFlow` wraps a `Kleisli` around (`ReplayFlow[F] = Kleisli[[X]
  * \=>> OptionT[F, X], A, B]`, `Replayability.scala:39`). `replayFlowMonoid.combine` (`Replayability.scala:53`) is "try
  * the replay cache (`OptionT.some`), else fall back to the real arrow (`OptionT.liftF`)" per field; this reproduces
  * exactly that shape for `claimAndRun` and confirms the traversal itself is indifferent to which monad transformer
  * sits under it.
  */
class HyloExecutionSpikeReplayLayerSuite extends CatsEffectSuite:
  private val context = RunContext(os.pwd, AgentInventory(Nil), None)
  private def issue(number: Int, body: String = "") =
    Issue(TaskNumber(number), IssueTitle(s"Task $number"), IssueBody(body), State("open"))
  private def summary(message: String, task: Issue) =
    RunSummary(status = Status("completed"), message = Message5(message), task = Some(task))

  // Mirrors Replayability.lift: no cache entry of its own, always falls through to the real effect.
  private def liftReal[A, B](real: A => IO[B]): A => OptionT[IO, B] =
    a => OptionT.liftF(real(a))

  // Mirrors replayFlowMonoid.combine: a cache hit short-circuits, a miss falls back to the real effect.
  private def replayOrReal[A, B](cache: Map[A, B], real: A => IO[B]): A => OptionT[IO, B] =
    a => cache.get(a).fold(OptionT.liftF(real(a)))(OptionT.some(_))

  test("the traversal runs unchanged over OptionT[IO, *], replaying a cached leaf and falling back for the rest"):
    val root = issue(1, "Depends on #2")
    val dependency = issue(2)
    val cachedDependencySummary = summary("replayed", dependency)

    Ref[IO].of(List.empty[TaskNumber]).flatMap { reallyClaimed =>
      val realClaimAndRun: TaskNode => IO[RunSummary] =
        node => reallyClaimed.update(_ :+ node.issue.number).as(summary("ran for real", node.issue))

      val run = HyloExecutionSpike.executeRecursive[[X] =>> OptionT[IO, X]](
        checkIfCompleted = liftReal(node => IO.pure(Right(node))),
        collectPendingDependencies = liftReal { node =>
          val pending =
            if node.issue.number === root.number then List(TaskNode(context, dependency)) else Nil
          IO.pure(DependencyPlan(node, pending, hasOpenChildren = false))
        },
        // Only the dependency has a cache entry; the root always falls through to realClaimAndRun.
        claimAndRun = replayOrReal(Map(TaskNode(context, dependency) -> cachedDependencySummary), realClaimAndRun)
      )

      run(TaskNode(context, root)).value.flatMap { result =>
        reallyClaimed.get.map { reallyRan =>
          assertEquals(result, Some(summary("ran for real", root)))
          // The cached dependency never touched the real effect; only the root did.
          assertEquals(reallyRan, List(root.number))
        }
      }
    }
