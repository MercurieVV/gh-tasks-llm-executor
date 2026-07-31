package com.github.mercurievv.ghllm.arrow

import com.github.mercurievv.ghllm.*
import com.github.mercurievv.ghllm.cli.{Phase, TaskMetadata}

/** Guards against the false green: a cheap runner that passes verification by weakening the verifier rather than by
  * solving the problem.
  *
  * This is the escalation ladder's worst outcome, not merely a bad one. The ladder's whole premise is that the verifier
  * is free and honest — a run that edits the tests it is judged by "saves" tokens by shipping wrong code, and then
  * records `outcome = "green"`, which feeds `successRate(phase, runner)` and biases selection *toward* the runner that
  * cheated. One such run makes the measurement worse than no measurement (TOKEN_EFFICIENCY_PLAN.md §2 Stage 1, risk 3).
  *
  * Only modification and deletion are blocked. Adding a test file is not gaming the verifier — it strengthens it — and
  * blocking additions would stop an implementer from covering the code it just wrote.
  */
object TestEditGuard:

  /** Phases permitted to change existing tests. `test` owns them outright; `plan` and `source-of-truth` are exempt
    * because they emit documents, so a test edit from one of them is out of contract in a way this guard is not the
    * right place to catch.
    */
  val ExemptPhases: Set[String] = Phase.All - Phase.Implement

  def isTestFile(path: String): Boolean =
    val normalised = path.trim.replace('\\', '/').toLowerCase
    val name = normalised.split('/').lastOption.getOrElse(normalised)
    normalised.startsWith("src/test/") ||
    normalised.contains("/src/test/") ||
    name.endsWith(".test.scala") ||
    name.endsWith("test.scala") ||
    name.endsWith("suite.scala") ||
    name.endsWith("spec.scala")

  def phaseOf(body: IssueBody): Option[String] =
    TaskMetadata.parse(body.value).phase.map(normalise).filter(_.nonEmpty)

  // Normalises here as well as in `phaseOf`: an exemption that depends on the
  // caller having normalised first fails open in the wrong direction — a
  // legitimate `test`-phase run gets blocked because its phase arrived as
  // " Test ".
  def isExempt(phase: Option[String]): Boolean =
    phase.map(normalise).exists(ExemptPhases.contains)

  private def normalise(phase: String): String = Phase.normalise(phase)

  /** The test files this run changed in a way that could weaken the verifier. Empty means the run is allowed to proceed
    * to validation.
    */
  def violations(phase: Option[String], modifiedOrDeleted: List[String]): List[String] =
    if isExempt(phase) then Nil
    else modifiedOrDeleted.filter(isTestFile).distinct.sorted

  def report(phase: Option[String], violations: List[String]): String =
    s"""Refusing to accept this run: it changed existing test files while in phase '${phase
        .getOrElse("unspecified")}'.
       |
       |${violations.map(file => s"  - $file").mkString("\n")}
       |
       |A run is verified by the tests, so a run that edits them cannot be trusted by
       |them. Revert the test changes and make the implementation satisfy the existing
       |tests, or split the test change into its own `test`-phase task.""".stripMargin
