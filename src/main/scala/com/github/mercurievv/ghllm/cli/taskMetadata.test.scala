package com.github.mercurievv.ghllm.cli

import com.github.mercurievv.ghllm.*
import com.github.mercurievv.ghllm.agent.*
import com.github.mercurievv.ghllm.arrow.*
import com.github.mercurievv.ghllm.cli.*
import com.github.mercurievv.ghllm.git.*
import com.github.mercurievv.ghllm.metrics.*

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
    val reparsed = TaskMetadata.parse(TaskMetadata.render(original).value)
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

  test("Implemented mark round-trips and is absent by default"):
    val original = TaskMetadata(
      evaluation = Some("ready"),
      execution = Some("implement"),
      implemented = Some("task/42"),
      enrichedDescription = Some("Do the thing.")
    )
    val reparsed = TaskMetadata.parse(TaskMetadata.render(original).value)
    assertEquals(reparsed.implemented, Some("task/42"))
    assertEquals(reparsed.enrichedDescription, Some("Do the thing."))
    // Backward compatibility: metadata without the mark leaves it empty.
    val legacy = TaskMetadata.parse(
      """Task metadata:
        |Evaluation: ready
        |Execution: implement""".stripMargin
    )
    assertEquals(legacy.implemented, None)

  test("Monoid preserves an earlier Implemented mark when newer omits it"):
    val older = TaskMetadata(implemented = Some("task/7"))
    val newer = TaskMetadata(evaluation = Some("ready"))
    assertEquals(
      Monoid[TaskMetadata].combine(older, newer).implemented,
      Some("task/7")
    )
    // A non-empty mark keeps write() from treating the metadata as empty.
    assert(!TaskMetadata(implemented = Some("task/7")).isEmpty)

  test("parse reads a Required abilities/importance block into requiredAbilities"):
    val md = TaskMetadata.parse(
      """Task metadata:
        |Evaluation: ready
        |Execution: implement
        |Required abilities/importance:
        |- complex-reasoning: 1.0
        |- scala: 0.6""".stripMargin
    )
    assertEquals(md.requiredAbilities, Map("complex-reasoning" -> 1.0, "scala" -> 0.6))

  test("malformed ability bullets are skipped, not fatal"):
    val md = TaskMetadata.parse(
      """Task metadata:
        |Required abilities/importance:
        |- complex-reasoning: 1.0
        |- not-a-number: abc
        |- scala""".stripMargin
    )
    assertEquals(md.requiredAbilities, Map("complex-reasoning" -> 1.0))

  test("absent Required abilities/importance leaves it empty (backward compatibility)"):
    val md = TaskMetadata.parse(
      """Task metadata:
        |Evaluation: ready
        |Execution: implement""".stripMargin
    )
    assertEquals(md.requiredAbilities, Map.empty)

  test("Required abilities/importance round-trips through parse -> render -> parse"):
    val original = TaskMetadata.parse(
      """Task metadata:
        |Evaluation: ready
        |Execution: implement
        |Required abilities/importance:
        |- complex-reasoning: 1.0
        |- scala: 0.6""".stripMargin
    )
    val reparsed = TaskMetadata.parse(TaskMetadata.render(original).value)
    assertEquals(reparsed.requiredAbilities, Map("complex-reasoning" -> 1.0, "scala" -> 0.6))

  test("Monoid merges requiredAbilityLines right-biased, same as runnerLines"):
    val older = TaskMetadata.parse(
      """Task metadata:
        |Required abilities/importance:
        |- complex-reasoning: 1.0""".stripMargin
    )
    val newer = TaskMetadata.parse(
      """Task metadata:
        |Required abilities/importance:
        |- scala: 0.6""".stripMargin
    )
    assertEquals(
      Monoid[TaskMetadata].combine(older, newer).requiredAbilities,
      Map("scala" -> 0.6)
    )
    // newer without an abilities block falls back to older's.
    assertEquals(
      Monoid[TaskMetadata].combine(older, TaskMetadata()).requiredAbilities,
      Map("complex-reasoning" -> 1.0)
    )
