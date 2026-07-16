import cats.Monoid

class TaskMetadataPhaseSuite extends munit.FunSuite:

  test("parse reads the Phase field") {
    val body =
      """Task metadata:
        |Evaluation: ready
        |Execution: implement
        |Phase: source-of-truth
        |""".stripMargin
    assertEquals(TaskMetadata.parse(body).phase, Some("source-of-truth"))
  }

  test("phase is None when no Phase line is present (backward compat)") {
    val body =
      """Task metadata:
        |Evaluation: ready
        |Execution: implement
        |""".stripMargin
    assertEquals(TaskMetadata.parse(body).phase, None)
  }

  test("render(parse(x)) round-trips the phase") {
    val body =
      """Some human-authored description.
        |
        |Task metadata:
        |Evaluation: ready
        |Execution: implement
        |Phase: plan
        |""".stripMargin
    val once = TaskMetadata.parse(body)
    val rendered = TaskMetadata.render(once)
    assertEquals(TaskMetadata.parse(rendered).phase, Some("plan"))
  }

  test("Monoid merge is right-biased on phase") {
    val older = TaskMetadata(phase = Some("plan"))
    val newer = TaskMetadata(phase = Some("implement"))
    assertEquals(Monoid[TaskMetadata].combine(older, newer).phase, Some("implement"))
    // newer with no phase falls back to older
    assertEquals(
      Monoid[TaskMetadata].combine(older, TaskMetadata()).phase,
      Some("plan")
    )
  }
