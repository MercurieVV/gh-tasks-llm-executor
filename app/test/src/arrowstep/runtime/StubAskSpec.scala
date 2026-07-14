package arrowstep.runtime

import arrowstep.core.{Answers, AskInput, Dialogue, Question, QuestionKind, Validator}
import cats.Id

final class StubAskSpec extends munit.FunSuite:

  private val question =
    Question("lang", "yes or no?", QuestionKind.Choice(List("yes", "no")), None, None, None)
  private val input = AskInput(List(question), None)

  test("StubAsk(canned).apply(input) returns canned answers in F") {
    val canned = Answers(Map("lang" -> "yes"))
    val out = StubAsk[Id](canned).apply(input)

    assertEquals(out.toMap, canned.toMap)
  }

  test("StubAsk can drive askUntilValid bad-then-good") {
    val ask = StubAsk.fromFunction[Id] { input =>
      if input.context.isEmpty then Answers(Map("lang" -> "maybe"))
      else Answers(Map("lang" -> "yes"))
    }

    val valid = Dialogue.askUntilValid(ask, Validator.basic[Id]).run(input)

    assertEquals(valid.toMap, Map("lang" -> "yes"))
  }
