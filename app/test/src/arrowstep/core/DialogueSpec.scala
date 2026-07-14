package arrowstep.core

import cats.Id

/** Acceptance for the agent gap + re-ask loop.
  *
  * The D7 fence — "no `ValidAnswers` without a `Validator`" — is a **compile-time** guarantee:
  * `ValidAnswers.fromValidated` is `private[core]`, so no module outside `core` can mint one.
  * These runtime tests only exercise the loop's behaviour.
  */
final class DialogueSpec extends munit.FunSuite:

  private val q =
    Question("lang", "yes or no?", QuestionKind.Choice(List("yes", "no")), None, None, None)
  private val start = AskInput(List(q), None)

  test("askUntilValid re-asks once on a bad answer, then yields ValidAnswers") {
    // First ask (no context) answers badly; the re-ask (context set by reAsk) answers well.
    val ask = new Ask[Id]:
      def apply(input: AskInput): Id[Answers] =
        if input.context.isEmpty then Answers(Map("lang" -> "maybe"))
        else Answers(Map("lang" -> "yes"))

    val valid = Dialogue.askUntilValid(ask, Validator.basic[Id]).run(start)
    assertEquals(valid.toMap, Map("lang" -> "yes"))
  }

  test("askUntilValid short-circuits when the first answer is already valid") {
    val ask = new Ask[Id]:
      def apply(input: AskInput): Id[Answers] = Answers(Map("lang" -> "no"))
    val valid = Dialogue.askUntilValid(ask, Validator.basic[Id]).run(start)
    assertEquals(valid.toMap, Map("lang" -> "no"))
  }

  test("basic validator gives precise Choice problems") {
    val out = Validator.basic[Id].validate(List(q), Answers(Map("lang" -> "maybe")))
    assertEquals(out, Left(List(Problem("lang", "'maybe' not in [yes, no]"))))
  }

  test("basic validator rejects empty free text and reports missing answers") {
    val free = Question("name", "name?", QuestionKind.FreeText, None, None, None)
    val empty = Validator.basic[Id].validate(List(free), Answers(Map("name" -> "  ")))
    assertEquals(empty, Left(List(Problem("name", "must not be empty"))))

    val missing = Validator.basic[Id].validate(List(free), Answers(Map.empty))
    assertEquals(missing, Left(List(Problem("name", "no answer provided"))))
  }

  test("basic validator falls back to a question default") {
    val withDefault =
      Question("lang", "yes or no?", QuestionKind.Choice(List("yes", "no")), Some("yes"), None, None)
    val out = Validator.basic[Id].validate(List(withDefault), Answers(Map.empty))
    assertEquals(out.map(_.toMap), Right(Map("lang" -> "yes")))
  }

  test("reAsk carries offending questions with problem context on the next ask") {
    // Record each AskInput purely: Ask writes to a Writer log the loop accumulates for free.
    type Logged[A] = cats.data.Writer[List[AskInput], A]
    val ask = new Ask[Logged]:
      def apply(input: AskInput): Logged[Answers] =
        val answer = if input.context.isEmpty then "maybe" else "yes"
        cats.data.Writer(List(input), Answers(Map("lang" -> answer)))

    val (log, _) = Dialogue.askUntilValid(ask, Validator.basic[Logged]).run(start).run
      log match
        case _ :: reAsked :: Nil =>
          assertEquals(reAsked.questions.map(_.id), List("lang"))
          assert(reAsked.context.exists(_.contains("not in [yes, no]")))
          reAsked.questions.headOption match
            case Some(h) => assert(h.context.exists(_.contains("not in [yes, no]")))
            case None    => fail("re-asked input had no questions")
        case _ => assertEquals(log.length, 2)
  }
