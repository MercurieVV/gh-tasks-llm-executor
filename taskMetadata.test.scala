import cats.Monoid

class TaskMetadataPhaseSuite extends munit.FunSuite:

  test("parse reads a Phase line"):
    val md = TaskMetadata.parse(
      """Task metadata:
        |Evaluation: ready
        |Execution: implement
        |Phase: implement""".stripMargin
    )
    assertEquals(md.evaluation, Some("ready"))
    assertEquals(md.execution, Some("implement"))
    assertEquals(md.phase, Some("implement"))

  test("absent Phase leaves phase empty (backward compatibility)"):
    val md = TaskMetadata.parse(
      """Task metadata:
        |Evaluation: ready
        |Execution: implement""".stripMargin
    )
    assertEquals(md.phase, None)
    // A body with no metadata at all is unchanged prose, no phase.
    val prose = TaskMetadata.parse("Just a plain description.")
    assertEquals(prose.phase, None)

  test("Phase round-trips through parse -> render -> parse"):
    val original = TaskMetadata(
      evaluation = Some("ready"),
      execution = Some("implement"),
      phase = Some("source-of-truth"),
      enrichedDescription = Some("Do the thing.")
    )
    val reparsed = TaskMetadata.parse(TaskMetadata.render(original))
    assertEquals(reparsed.phase, Some("source-of-truth"))
    assertEquals(reparsed.evaluation, Some("ready"))
    assertEquals(reparsed.execution, Some("implement"))
    assertEquals(reparsed.enrichedDescription, Some("Do the thing."))

  test("Monoid merges phase right-biased"):
    val older = TaskMetadata(phase = Some("plan"))
    val newer = TaskMetadata(phase = Some("test"))
    assertEquals(Monoid[TaskMetadata].combine(older, newer).phase, Some("test"))
    // newer without a phase falls back to older.
    assertEquals(
      Monoid[TaskMetadata].combine(older, TaskMetadata()).phase,
      Some("plan")
    )
