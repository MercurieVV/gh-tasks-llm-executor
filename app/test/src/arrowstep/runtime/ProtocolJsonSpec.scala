package arrowstep.runtime

import arrowstep.core.Problem
import arrowstep.core.ProgramSays
import arrowstep.core.Question
import arrowstep.core.QuestionKind

final class ProtocolJsonSpec extends munit.FunSuite:

  test("renders NeedInput to the L0 wire shape") {
    val question = Question(
      id = "web-server",
      text = "Enable Web Server?",
      kind = QuestionKind.Choice(List("yes", "no")),
      default = Some("no"),
      current = Some("no"),
      context = None
    )

    val rendered = ProtocolJson.render(
      ProgramSays.NeedInput(Some("Configuring Scala project ./x"), List(question))
    )

    assertEquals(
      rendered,
      golden("need-input.json")
    )

    assertEquals(
      ProtocolJson.parse(rendered),
      Right(ProgramSays.NeedInput(Some("Configuring Scala project ./x"), List(question)))
    )
  }

  test("renders Rejected with problems and re-stated questions") {
    val question = Question(
      id = "module-name",
      text = "Module name?",
      kind = QuestionKind.FreeText,
      default = None,
      current = Some(""),
      context = Some("Use a Scala identifier.")
    )

    val rendered = ProtocolJson.toJson(
      ProgramSays.Rejected(
        List(Problem("module-name", "must not be empty")),
        List(question)
      )
    )

    assertEquals(rendered.render(), golden("rejected.json"))
    assertEquals(rendered("status").str, "rejected")
    assertEquals(rendered("problems")(0)("questionId").str, "module-name")
    assertEquals(rendered("problems")(0)("message").str, "must not be empty")
    assertEquals(rendered("questions")(0)("kind").str, "free-text")
    assert(!rendered("questions")(0).obj.contains("allowed"))
    assertEquals(rendered("questions")(0)("default"), ujson.Null)
    assertEquals(rendered("questions")(0)("current").str, "")
    assertEquals(rendered("questions")(0)("context").str, "Use a Scala identifier.")
    assertEquals(
      ProtocolJson.parse(rendered.render()),
      Right(
        ProgramSays.Rejected(
          List(Problem("module-name", "must not be empty")),
          List(question)
        )
      )
    )
  }

  test("renders Done with consumer JSON passed through") {
    val result = ujson.Obj("ok" -> true, "path" -> "build.sbt")

    assertEquals(
      ProtocolJson.render(ProgramSays.Done(result)),
      golden("done.json")
    )
    assertEquals(ProtocolJson.parse(golden("done.json")), Right(ProgramSays.Done(result)))
  }

  test("renders and parses answers through the centralized protocol codec") {
    val answers = arrowstep.core.Answers(Map("lang" -> "scala", "build" -> "mill"))

    assertEquals(ProtocolJson.renderAnswers(answers), """{"build":"mill","lang":"scala"}""")
    assertEquals(
      ProtocolJson.parseAnswers("""{"lang":"scala","build":"mill"}""").map(_.toMap),
      Some(answers.toMap)
    )
    assertEquals(ProtocolJson.parseAnswers("""{"lang":1}"""), None)
  }

  test("rejects malformed protocol JSON") {
    assertEquals(ProtocolJson.parse("{"), Left("invalid JSON"))
    assertEquals(ProtocolJson.parse("""{"status":"later"}"""), Left("unknown status: later"))
    assertEquals(
      ProtocolJson.parse("""{"status":"need-input","questions":{}}"""),
      Left("questions must be an array")
    )
  }

  private def golden(name: String): String =
    scala.io.Source.fromResource("arrowstep/runtime/" + name).mkString.trim
