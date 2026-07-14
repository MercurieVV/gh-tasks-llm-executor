package arrowstep.core

/** Guards the dialogue vocabulary: exhaustive `ProgramSays` matching → exit codes and opaque
  * accessors round-trip. The D7 fence (`ValidAnswers` has no public constructor) is enforced at
  * compile time — `ValidAnswers.fromValidated` is `private[core]`, so no external module can call it.
  */
final class VocabularySpec extends munit.FunSuite:

  private val q = Question("lang", "Which language?", QuestionKind.Choice(List("scala")),
    default = None, current = None, context = None)

  test("ProgramSays maps every case to a wire exit code") {
    val cases: List[(ProgramSays[Int], Int)] = List(
      ProgramSays.Done(42)                       -> 0,
      ProgramSays.NeedInput(None, List(q))       -> 2,
      ProgramSays.Rejected(List(Problem("lang", "nope")), List(q)) -> 2
    )
    cases.foreach { case (says, code) => assertEquals(says.exitCode, code) }
  }

  test("ProgramSays match is exhaustive over all cases") {
    def describe[R](s: ProgramSays[R]): String = s match
      case ProgramSays.Done(_)         => "done"
      case ProgramSays.NeedInput(_, _) => "need-input"
      case ProgramSays.Rejected(_, _)  => "rejected"
    assertEquals(describe(ProgramSays.Done(1)), "done")
    assertEquals(describe(ProgramSays.NeedInput(None, Nil): ProgramSays[Int]), "need-input")
    assertEquals(describe(ProgramSays.Rejected(Nil, Nil): ProgramSays[Int]), "rejected")
  }

  test("Answers round-trips through its accessors") {
    val a = Answers(Map("lang" -> "scala"))
    assertEquals(a.get("lang"), Some("scala"))
    assertEquals(a.get("missing"), None)
    assertEquals(a.toMap, Map("lang" -> "scala"))
  }

  test("ValidAnswers exposes read accessors over the private factory") {
    val v = ValidAnswers.fromValidated(Map("lang" -> "scala"))
    assertEquals(v.get("lang"), Some("scala"))
    assertEquals(v.toMap, Map("lang" -> "scala"))
  }
