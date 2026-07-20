import cats.Monoid
import cats.effect.kernel.Sync
import cats.syntax.all.*

// Structured evaluator/runner state for a task. Never written into the issue
// body directly — see TaskMetadataStore. Fields are individually mergeable so
// a follow-up metadata comment can update just evaluation/execution without
// having to restate runner/parent/dependency lines set by an earlier one.
final case class TaskMetadata(
    evaluation: Option[String] = None,
    execution: Option[String] = None,
    phase: Option[String] = None,
    runnerLines: List[String] = Nil,
    parentLines: List[String] = Nil,
    dependencyLines: List[String] = Nil,
    enrichedDescription: Option[String] = None
):
  def isEmpty: Boolean =
    evaluation.isEmpty && execution.isEmpty && phase.isEmpty &&
      runnerLines.isEmpty && parentLines.isEmpty && dependencyLines.isEmpty &&
      enrichedDescription.isEmpty

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
        runnerLines =
          if newer.runnerLines.nonEmpty then newer.runnerLines
          else older.runnerLines,
        parentLines =
          if newer.parentLines.nonEmpty then newer.parentLines
          else older.parentLines,
        dependencyLines =
          if newer.dependencyLines.nonEmpty then newer.dependencyLines
          else older.dependencyLines,
        enrichedDescription =
          newer.enrichedDescription.orElse(older.enrichedDescription)
      )

  private val DepKeywords =
    List("depends on", "depend on", "dependency", "dependencies")
  private val ParentLineRegex = """(?i)\bparent\b\s*:?\s*#\d+""".r
  private val RunnerHeaderMarkers =
    List(
      "preferred llms/models/efforts/versions",
      "preferred llms/models/versions"
    )

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

    val parentLines =
      lines.filter(l => ParentLineRegex.findFirstIn(l).isDefined)
    val dependencyLines =
      lines.filter(l => DepKeywords.exists(k => l.toLowerCase.contains(k)))

    val structuredLines =
      (Set("task metadata:") ++
        List("evaluation:", "execution:", "phase:") ++
        runnerLines ++ parentLines ++ dependencyLines).map(_.trim.toLowerCase)
    val prose = lines
      .filterNot { l =>
        val trimmed = l.trim
        val lower = trimmed.toLowerCase
        structuredLines.contains(lower) || lower.startsWith("evaluation:") ||
        lower.startsWith("execution:") || lower.startsWith("phase:")
      }
      .mkString("\n")
      .trim

    TaskMetadata(
      evaluation = field("evaluation"),
      execution = field("execution"),
      phase = field("phase"),
      runnerLines = runnerLines,
      parentLines = parentLines,
      dependencyLines = dependencyLines,
      enrichedDescription = Option.when(prose.nonEmpty)(prose)
    )

  // Renders a merged TaskMetadata back into the combined text the rest of the
  // pipeline reads as if it were the issue body (see effectiveIssue).
  def render(metadata: TaskMetadata): String =
    val metaLines = List(
      Some("Task metadata:"),
      metadata.evaluation.map(v => s"Evaluation: $v"),
      metadata.execution.map(v => s"Execution: $v"),
      metadata.phase.map(v => s"Phase: $v")
    ).flatten ++ metadata.parentLines ++ metadata.dependencyLines ++ metadata.runnerLines
    val metaBlock = metaLines.mkString("\n")
    metadata.enrichedDescription.fold(metaBlock)(prose =>
      s"$prose\n\n$metaBlock"
    )

trait TaskMetadataStore[F[_]]:
  def read(root: os.Path, task: Issue): F[TaskMetadata]
  def write(
      root: os.Path,
      taskId: TaskNumber,
      metadata: TaskMetadata,
      progress: String => F[Unit]
  ): F[Unit]

object TaskMetadataStore:
  // Persists metadata as an appended "Task metadata:" comment instead of
  // rewriting the issue body, so scripted evaluation/runner-selection state
  // never destroys human-authored task description text. Reads fold the
  // task's original body (oldest layer, for backward compatibility with
  // tasks that predate this store) with every metadata comment in
  // chronological order, latest field wins.
  def commentBased[F[_]](using F: Sync[F]): TaskMetadataStore[F] =
    new TaskMetadataStore[F]:
      def read(root: os.Path, task: Issue): F[TaskMetadata] =
        GitHub.metadataCommentBodies(root, task.number).map { commentBodies =>
          val legacyLayer = TaskMetadata.parse(task.body)
          (legacyLayer :: commentBodies.map(TaskMetadata.parse))
            .foldLeft(Monoid[TaskMetadata].empty)(Monoid[TaskMetadata].combine)
        }

      def write(
          root: os.Path,
          taskId: TaskNumber,
          metadata: TaskMetadata,
          progress: String => F[Unit]
      ): F[Unit] =
        if metadata.isEmpty then F.unit
        else
          GitHub.commentTaskMetadata(
            root,
            taskId,
            TaskMetadata.render(metadata),
            progress
          )
