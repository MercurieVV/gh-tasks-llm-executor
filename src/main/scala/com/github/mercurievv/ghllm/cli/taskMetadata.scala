package com.github.mercurievv.ghllm.cli

import com.github.mercurievv.ghllm.*
import com.github.mercurievv.ghllm.agent.*
import com.github.mercurievv.ghllm.arrow.*
import com.github.mercurievv.ghllm.cli.*
import com.github.mercurievv.ghllm.git.*
import com.github.mercurievv.ghllm.metrics.*

import cats.Monoid
import cats.effect.kernel.Sync
import cats.syntax.all.*

// Node output contract (Stage 4: T17).  Decision, touched files, referenced
// symbols and follow-ups are the only context a child task requires —
// carrying the full parent reasoning transcript taxes every downstream turn.
final case class TaskArtifact(
    decision: String,
    filesTouched: List[String] = Nil,
    symbols: List[String] = Nil,
    followUps: List[String] = Nil
)

object TaskArtifact:
  val DefaultMaxChars: Int = 2000
  val Marker = "\n\n[CONTEXT TRUNCATED]"

  /** Renders the artifact into the full structured text that a child task would
    * receive.  No truncation — call `boundedRender` to enforce the limit.
    */
  def render(artifact: TaskArtifact): String =
    val sb = new StringBuilder
    sb.append(s"Decision: ${artifact.decision}\n")
    if artifact.filesTouched.nonEmpty then
      sb.append(s"Files touched: ${artifact.filesTouched.mkString(", ")}\n")
    if artifact.symbols.nonEmpty then
      sb.append(s"Symbols: ${artifact.symbols.mkString(", ")}\n")
    if artifact.followUps.nonEmpty then
      sb.append(s"Follow-ups: ${artifact.followUps.mkString(", ")}\n")
    sb.toString

  /** Returns a version of the rendered artifact that is guaranteed to fit within
    * `limit` characters.  If the raw text exceeds the limit it is truncated and
    * an explicit `[CONTEXT TRUNCATED]` marker is appended so the next phase can
    * see that information was dropped.
    */
  def boundedRender(artifact: TaskArtifact, limit: Int = DefaultMaxChars): String =
    bound(render(artifact), limit)

  /** The same bound applied to text that did not come from a `TaskArtifact`.
    *
    * Runners write their conclusion as free-form prose, so the contract cannot be
    * enforced at the point it is produced without also constraining what they
    * emit. It is enforced where the text becomes someone else's context instead —
    * an unbounded "summary" pasted into every dependent task's prompt is the
    * transcript-shaped cost this contract exists to prevent.
    */
  def bound(text: String, limit: Int = DefaultMaxChars): String =
    val markerLen = Marker.length
    if limit < markerLen then Marker
    else if text.length <= limit then text
    else
      val keepLen = limit - markerLen
      if keepLen == 0 then Marker
      else text.take(keepLen) + Marker

// Structured evaluator/runner state for a task. Never written into the issue
// body directly — see TaskMetadataStore. Fields are individually mergeable so
// a follow-up metadata comment can update just evaluation/execution without
// having to restate runner/parent/dependency lines set by an earlier one.
final case class TaskMetadata(
    evaluation: Option[String] = None,
    execution: Option[String] = None,
    phase: Option[String] = None,
    // Durable "implementer already produced committed+pushed output" mark.
    // Value is the branch the work landed on. Written after commitIfChanged;
    // read before runAgent to guarantee the implementer LLM is not re-invoked
    // on work it already finished (see main.runAgent's already-implemented
    // short-circuit).
    implemented: Option[String] = None,
    // Durable "this user answer was already folded into an evaluation" mark
    // (digest of the answer text, see EvaluationArrows.answerDigest). Without
    // it, an answer comment stays "latest" forever and would suppress the
    // cached evaluation on every later replay, re-running the evaluator LLM.
    answerConsumed: Option[String] = None,
    runnerLines: List[String] = Nil,
    // Raw "Required abilities/importance:" block lines (header + bullets),
    // same shape as runnerLines. Preferred over a pinned runner for new
    // tasks: the evaluator names abstract abilities + importance
    // coefficients here, and AgentInventory.selectRunnerFor picks the
    // concrete runner at run time using live tool data - see
    // requiredAbilities / Priority.scala.
    requiredAbilityLines: List[String] = Nil,
    parentLines: List[String] = Nil,
    dependencyLines: List[String] = Nil,
    enrichedDescription: Option[String] = None
):
  def isEmpty: Boolean =
    evaluation.isEmpty && execution.isEmpty && phase.isEmpty &&
      implemented.isEmpty && answerConsumed.isEmpty && runnerLines.isEmpty &&
      requiredAbilityLines.isEmpty &&
      parentLines.isEmpty && dependencyLines.isEmpty && enrichedDescription.isEmpty

  // Ability name -> importance coefficient, parsed from requiredAbilityLines
  // bullets ("- ability: coefficient"). Malformed bullets are skipped rather
  // than failing the whole task's metadata.
  def requiredAbilities: Map[String, Double] =
    TaskMetadata.parseRequiredAbilities(requiredAbilityLines)

object TaskMetadata:

  val MetadataCommentPrefix = "task metadata:"

  given Monoid[TaskMetadata] with
    def empty: TaskMetadata = TaskMetadata()
    // Right-biased: `newer` wins per field, falling back to `older` only when
    // `newer` didn't restate that field.
    def combine(older: TaskMetadata, newer: TaskMetadata): TaskMetadata =
      TaskMetadata(
        evaluation = newer.evaluation.orElse(older.evaluation),
        execution = newer.execution.orElse(older.execution),
        phase = newer.phase.orElse(older.phase),
        implemented = newer.implemented.orElse(older.implemented),
        answerConsumed = newer.answerConsumed.orElse(older.answerConsumed),
        runnerLines =
          if newer.runnerLines.nonEmpty then newer.runnerLines
          else older.runnerLines,
        requiredAbilityLines =
          if newer.requiredAbilityLines.nonEmpty then newer.requiredAbilityLines
          else older.requiredAbilityLines,
        parentLines =
          if newer.parentLines.nonEmpty then newer.parentLines
          else older.parentLines,
        dependencyLines =
          if newer.dependencyLines.nonEmpty then newer.dependencyLines
          else older.dependencyLines,
        enrichedDescription = newer.enrichedDescription.orElse(older.enrichedDescription)
      )

  private val DepKeywords =
    List("depends on", "depend on", "dependency", "dependencies")
  private val ParentLineRegex = """(?i)\bparent\b\s*:?\s*#\d+""".r
  private val RunnerHeaderMarkers =
    List(
      "preferred llms/models/efforts/versions",
      "preferred llms/models/versions"
    )
  private val RequiredAbilityHeaderMarkers =
    List("required abilities/importance")

  // "- ability: coefficient" (or "* ability: coefficient"); coefficient must
  // parse as a Double. Lines that don't match this shape are dropped.
  private val AbilityBulletRegex =
    """(?i)^\s*[-*]\s*([a-z0-9][a-z0-9\- ]*)\s*:\s*([0-9]*\.?[0-9]+)\s*$""".r

  def parseRequiredAbilities(lines: List[String]): Map[String, Double] =
    lines
      .collect { case AbilityBulletRegex(ability, coefficient) =>
        scala.util.Try(coefficient.toDouble).toOption.map(ability.trim.toLowerCase -> _)
      }
      .flatten
      .toMap

  // Splits `text` into the structured metadata lines and the remaining prose,
  // and folds the structured lines into a TaskMetadata.
  def parse(text: String): TaskMetadata =
    val lines = text.linesIterator.toList

    def field(key: String): Option[String] =
      lines
        .map(_.trim)
        .collectFirst {
          case line if line.toLowerCase.startsWith(s"$key:") =>
            line.drop(key.length + 1).trim
        }
        .filter(_.nonEmpty)

    val runnerHeaderIdx = lines.indexWhere { line =>
      val lower = line.trim.toLowerCase
      RunnerHeaderMarkers.exists(lower.contains)
    }
    val runnerLines =
      if runnerHeaderIdx < 0 then Nil
      else
        lines(runnerHeaderIdx) :: lines
          .drop(runnerHeaderIdx + 1)
          .takeWhile(l => l.trim.startsWith("-") || l.trim.startsWith("*"))

    val abilityHeaderIdx = lines.indexWhere { line =>
      val lower = line.trim.toLowerCase
      RequiredAbilityHeaderMarkers.exists(lower.contains)
    }
    val requiredAbilityLines =
      if abilityHeaderIdx < 0 then Nil
      else
        lines(abilityHeaderIdx) :: lines
          .drop(abilityHeaderIdx + 1)
          .takeWhile(l => l.trim.startsWith("-") || l.trim.startsWith("*"))

    val parentLines =
      lines.filter(l => ParentLineRegex.findFirstIn(l).isDefined)
    val dependencyLines =
      lines.filter(l => DepKeywords.exists(k => l.toLowerCase.contains(k)))

    val structuredLines =
      (Set("task metadata:") ++
        List("evaluation:", "execution:", "phase:", "implemented:", "answer-consumed:") ++
        runnerLines ++ requiredAbilityLines ++ parentLines ++ dependencyLines).map(_.trim.toLowerCase)
    val prose = lines
      .filterNot { l =>
        val trimmed = l.trim
        val lower = trimmed.toLowerCase
        structuredLines.contains(lower) || lower.startsWith("evaluation:") ||
        lower.startsWith("execution:") || lower.startsWith("phase:") ||
        lower.startsWith("implemented:") || lower.startsWith("answer-consumed:")
      }
      .mkString("\n")
      .trim

    TaskMetadata(
      evaluation = field("evaluation"),
      execution = field("execution"),
      phase = field("phase"),
      implemented = field("implemented"),
      answerConsumed = field("answer-consumed"),
      runnerLines = runnerLines,
      requiredAbilityLines = requiredAbilityLines,
      parentLines = parentLines,
      dependencyLines = dependencyLines,
      enrichedDescription = Option.when(prose.nonEmpty)(prose)
    )

  // Renders a merged TaskMetadata back into the combined text the rest of the
  // pipeline reads as if it were the issue body (see effectiveIssue).
  def render(metadata: TaskMetadata): IssueBody =
    val metaLines = List(
      Some("Task metadata:"),
      metadata.evaluation.map(v => s"Evaluation: $v"),
      metadata.execution.map(v => s"Execution: $v"),
      metadata.phase.map(v => s"Phase: $v"),
      metadata.implemented.map(v => s"Implemented: $v"),
      metadata.answerConsumed.map(v => s"Answer-consumed: $v")
    ).flatten ++ metadata.parentLines ++ metadata.dependencyLines ++ metadata.runnerLines ++
      metadata.requiredAbilityLines
    val metaBlock = metaLines.mkString("\n")
    IssueBody(
      metadata.enrichedDescription.fold(metaBlock)(prose => s"$prose\n\n$metaBlock")
    )

trait TaskMetadataStore[F[_]]:
  def read(root: os.Path, task: Issue): F[TaskMetadata]
  def write(
      root: os.Path,
      taskId: TaskNumber,
      metadata: TaskMetadata
  ): F[Unit]

object TaskMetadataStore:
  // Persists metadata as an appended "Task metadata:" comment instead of
  // rewriting the issue body, so scripted evaluation/runner-selection state
  // never destroys human-authored task description text. Reads fold the
  // task's original body (oldest layer, for backward compatibility with
  // tasks that predate this store) with every metadata comment in
  // chronological order, latest field wins.
  def commentBased[F[_]](using F: Sync[F])(progress: String => F[Unit]): TaskMetadataStore[F] =
    new TaskMetadataStore[F]:
      def read(root: os.Path, task: Issue): F[TaskMetadata] =
        GitHub.metadataCommentBodies(root, task.number).map { commentBodies =>
          val legacyLayer = TaskMetadata.parse(task.body.value)
          (legacyLayer :: commentBodies.map(_.value).map(TaskMetadata.parse))
            .foldLeft(Monoid[TaskMetadata].empty)(Monoid[TaskMetadata].combine)
        }

      def write(
          root: os.Path,
          taskId: TaskNumber,
          metadata: TaskMetadata
      ): F[Unit] =
        if metadata.isEmpty then F.unit
        else
          GitHub.commentTaskMetadata(progress)(
            root,
            taskId,
            TaskMetadata.render(metadata)
          )
