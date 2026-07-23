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
