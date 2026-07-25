import cats.data.Kleisli
import cats.effect.IO
import munit.CatsEffectSuite

class RecursiveArrowsSuite extends CatsEffectSuite:
  type TestFlow[A, B] = Kleisli[IO, A, B]

  test("executeRecursive supports runtime-deferred self references"):
    val issue = Issue(TaskNumber(1), IssueTitle("Root"), IssueBody(""), State("open"))
    val expected =
      RunSummary(status = Status("completed"), message = Message5("ran root"), task = Some(issue))
    val arrows = RecursiveArrows[TestFlow](
      checkIfCompleted = Kleisli((issue: Issue) => IO.pure(Right(issue))),
      runDependencies = _ => Kleisli((issue: Issue) => IO.pure(Right(issue))),
      claimAndRun = Kleisli(_ => IO.pure(expected)),
      defer = self => Kleisli(issue => self.run(issue))
    )

    arrows.executeRecursive
      .run(issue)
      .map(result => assertEquals(result, expected))

class UntilClosedArrowsSuite extends CatsEffectSuite:
  type TestFlow[A, B] = Kleisli[IO, A, B]

  test("runUntilClosed repeats via defer until routeContinuation stops it"):
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
      },
      defer = self => Kleisli(walk => self.run(walk))
    )

    arrows.runUntilClosed
      .run(RootWalk(candidate, 1, None))
      .map(result => assertEquals(result, finalPass))

class ParallelArrowsSuite extends CatsEffectSuite:
  test("parAll runs one arrow over every input"):
    val double = Kleisli((n: Int) => IO.pure(n * 2))
    ParallelArrows
      .parAll(double)
      .run(List(1, 2, 3))
      .map(result => assertEquals(result, List(2, 4, 6)))
