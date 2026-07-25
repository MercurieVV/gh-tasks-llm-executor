import cats.data.Kleisli
import cats.effect.IO
import cats.effect.Ref
import cats.syntax.all.*
import munit.CatsEffectSuite

class RecursiveArrowsSuite extends CatsEffectSuite:
  type TestFlow[A, B] = Kleisli[IO, A, B]

  private val context = RunContext(os.pwd, AgentInventory(Nil), None)
  private def issue(number: Int) =
    Issue(TaskNumber(number), IssueTitle(s"Task $number"), IssueBody(""), State("open"))
  private def summary(message: String, task: Issue) =
    RunSummary(status = Status("completed"), message = Message5(message), task = Some(task))

  test("executeRecursive runs a node only after its dependencies have run"):
    val root = issue(1)
    val dependency = issue(2)

    Ref[IO].of(List.empty[TaskNumber]).flatMap { claimed =>
      val arrows = RecursiveArrows[TestFlow](
        checkIfCompleted = Kleisli((node: TaskNode) => IO.pure(Right(node))),
        collectPendingDependencies = Kleisli { (node: TaskNode) =>
          val pending =
            if node.issue.number === root.number then List(TaskNode(context, dependency))
            else Nil
          IO.pure(DependencyPlan(node, pending, hasOpenChildren = false))
        },
        recordDependencyOutcome = Kleisli(_ => IO.pure(Right(()))),
        routeDependencyOutcome = Kleisli { case (plan, _) => IO.pure(Right(plan.node)) },
        // The recursion reaches this arrow again through ArrowDefer.fix rather
        // than through a `defer` field supplied at the wiring site.
        claimAndRun = Kleisli { (node: TaskNode) =>
          claimed.update(_ :+ node.issue.number).as(summary("ran", node.issue))
        }
      )

      arrows.executeRecursive.run(TaskNode(context, root)).flatMap { result =>
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

    val arrows = RecursiveArrows[TestFlow](
      checkIfCompleted = Kleisli((node: TaskNode) => IO.pure(Right(node))),
      collectPendingDependencies = Kleisli { (node: TaskNode) =>
        val pending =
          if node.issue.number === root.number then List(TaskNode(context, issue(2)))
          else Nil
        IO.pure(DependencyPlan(node, pending, hasOpenChildren = false))
      },
      recordDependencyOutcome = Kleisli { case (_, depSummary) =>
        IO.pure(Either.cond(depSummary.status.value === "completed", (), depSummary))
      },
      routeDependencyOutcome = Kleisli {
        case (_, Left(stopped)) => IO.pure(Left(stopped))
        case (plan, Right(_))   => IO.pure(Right(plan.node))
      },
      claimAndRun = Kleisli { (node: TaskNode) =>
        if node.issue.number === root.number then IO.raiseError(AssertionError("root must not run"))
        else IO.pure(blocked)
      }
    )

    arrows.executeRecursive
      .run(TaskNode(context, root))
      .map(result => assertEquals(result, blocked))

class UntilClosedArrowsSuite extends CatsEffectSuite:
  type TestFlow[A, B] = Kleisli[IO, A, B]

  test("runUntilClosed repeats until routeContinuation stops it"):
    val issue = Issue(TaskNumber(1), IssueTitle("Root"), IssueBody(""), State("open"))
    val runner = TaskRunner(AgentBinary("claude"), None, None, None)
    val context = RunContext(os.pwd, AgentInventory(Nil), None)
    val candidate = TaskCandidate(context, issue, runner)
    val firstPass =
      RunSummary(status = Status("split"), message = Message5("split"), task = Some(issue))
    val finalPass =
      RunSummary(status = Status("completed"), message = Message5("done"), task = Some(issue))

    val arrows = UntilClosedArrows[TestFlow](
      refreshRoot = Kleisli(walk => IO.pure(Right(walk))),
      runRootOnce = Kleisli { walk =>
        val summary = if walk.iteration == 1 then firstPass else finalPass
        IO.pure((walk, summary))
      },
      routeContinuation = Kleisli { case (walk, summary) =>
        IO.pure(
          if summary.status.value == "completed" then Left(summary)
          else Right(walk.copy(iteration = walk.iteration + 1, previous = Some(summary)))
        )
      }
    )

    arrows.runUntilClosed
      .run(RootWalk(candidate, 1, None))
      .map(result => assertEquals(result, finalPass))

class ArrowTraverseSuite extends CatsEffectSuite:
  private val traverse = ArrowTraverse[[A, B] =>> Kleisli[IO, A, B]]

  test("all runs one arrow over every input"):
    traverse
      .all(Kleisli((n: Int) => IO.pure(n * 2)))
      .run(List(1, 2, 3))
      .map(result => assertEquals(result, List(2, 4, 6)))

  test("untilLeft stops at the first Left and skips the rest"):
    Ref[IO].of(List.empty[Int]).flatMap { seen =>
      val step = Kleisli { (n: Int) =>
        seen.update(_ :+ n).as(Either.cond(n < 2, (), s"stopped at $n"))
      }
      traverse
        .untilLeft(step)
        .run(List(1, 2, 3))
        .flatMap { result =>
          seen.get.map { visited =>
            assertEquals(result, Left("stopped at 2"))
            assertEquals(visited, List(1, 2))
          }
        }
    }

class ParallelArrowsSuite extends CatsEffectSuite:
  test("parAll runs one arrow over every input"):
    val double = Kleisli((n: Int) => IO.pure(n * 2))
    ParallelArrows
      .parAll(double)
      .run(List(1, 2, 3))
      .map(result => assertEquals(result, List(2, 4, 6)))

class TraversalArrowsSuite extends CatsEffectSuite:
  import BusinessLogicFixture.*

  private def arrows(untilClosedResult: RunSummary, runOnceResult: RunSummary) =
    TraversalArrows[TestFlow](
      routeRecursiveMode = Kleisli { candidate =>
        IO.pure(Either.cond(!candidate.context.recursive.value, candidate, candidate))
      },
      untilClosed = UntilClosedArrows[TestFlow](
        refreshRoot = Kleisli(walk => IO.pure(Right(walk))),
        runRootOnce = Kleisli(walk => IO.pure((walk, untilClosedResult))),
        routeContinuation = Kleisli { case (_, result) => IO.pure(Left(result)) }
      ),
      runOnce = Kleisli(_ => IO.pure(runOnceResult))
    )

  test("--recursive repeats the root walk until it closes"):
    arrows(summary("walked"), summary("once")).runCandidate
      .run(candidate(1, recursive = true))
      .map(result => assertEquals(result, summary("walked")))

  test("without --recursive the tree is walked exactly once"):
    arrows(summary("walked"), summary("once")).runCandidate
      .run(candidate(1, recursive = false))
      .map(result => assertEquals(result, summary("once")))
