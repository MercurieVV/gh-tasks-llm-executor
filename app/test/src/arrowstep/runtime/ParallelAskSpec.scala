package arrowstep.runtime

import arrowstep.core.{Answers, Ask, AskInput, Question, QuestionKind}
import cats.effect.{Deferred, IO}

import scala.concurrent.duration.*

final class ParallelAskSpec extends munit.CatsEffectSuite:

  private val leftQuestion =
    Question("left", "Left?", QuestionKind.FreeText, None, None, None)

  private val rightQuestion =
    Question("right", "Right?", QuestionKind.FreeText, None, None, None)

  private val leftInput =
    AskInput(List(leftQuestion), None)

  private val rightInput =
    AskInput(List(rightQuestion), None)

  test("both runs independent asks concurrently") {
    for
      leftStarted <- Deferred[IO, Unit]
      rightStarted <- Deferred[IO, Unit]
      left = new Ask[IO]:
        def apply(input: AskInput): IO[Answers] =
          leftStarted.complete(()) *> rightStarted.get.as(
            input.questions.headOption.fold(Answers(Map.empty))(question => Answers(Map(question.id -> "left-answer")))
          )
      right = new Ask[IO]:
        def apply(input: AskInput): IO[Answers] =
          rightStarted.complete(()) *> leftStarted.get.as(
            input.questions.headOption.fold(Answers(Map.empty))(question => Answers(Map(question.id -> "right-answer")))
          )
      result <- ParallelAsk.both[IO](left, right).run((leftInput, rightInput)).timeout(1.second)
    yield assertEquals(
      result,
      (Answers(Map("left" -> "left-answer")), Answers(Map("right" -> "right-answer")))
    )
  }

  test("many pairs asks and inputs in order") {
    val asks = List(
      StubAsk[IO](Answers(Map("left" -> "left-answer"))),
      StubAsk[IO](Answers(Map("right" -> "right-answer")))
    )

    ParallelAsk.many[IO](asks).run(List(leftInput, rightInput)).map { result =>
      assertEquals(result.map(_.toMap), List(Map("left" -> "left-answer"), Map("right" -> "right-answer")))
    }
  }

  test("many rejects mismatched ask and input counts") {
    val asks = List(StubAsk[IO](Answers(Map("left" -> "left-answer"))))

    ParallelAsk.many[IO](asks).run(List(leftInput, rightInput)).attempt.map { result =>
      assert(result.isLeft)
    }
  }
