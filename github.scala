import cats.effect.kernel.Sync
import cats.syntax.all.*

import scala.util.Try

final case class Issue(
    number: Int,
    title: String,
    body: String,
    state: String
)

object GitHub:

  def fetchIssues[F[_]: Sync](root: os.Path): F[List[Issue]] =
    Sync[F].blocking {
      val issuesJson = os
        .proc(
          "gh",
          "issue",
          "list",
          "--state",
          "open",
          "--limit",
          "1000",
          "--json",
          "number,title,body,state"
        )
        .call(cwd = root)
        .out
        .text()

      ujson.read(issuesJson).arr.toList.flatMap(parseIssue)
    }

  private def parseIssue(value: ujson.Value): Option[Issue] =
    value match
      case ujson.Obj(fields) =>
        for
          number <- fields.get("number").collect { case ujson.Num(value) =>
            value.toInt
          }
          title <- fields.get("title").collect { case ujson.Str(value) =>
            value
          }
        yield Issue(
          number = number,
          title = title,
          body = fields
            .get("body")
            .collect { case ujson.Str(value) => value }
            .getOrElse(""),
          state = fields
            .get("state")
            .collect { case ujson.Str(value) => value }
            .getOrElse("")
        )
      case _ => None

  def taskRunner(issue: Issue): Option[TaskRunner] =
    taskRunners(issue).headOption

  def taskRunners(issue: Issue): List[TaskRunner] =
    val fields = issue.body.linesIterator
      .flatMap(parseRunnerField)
      .toMap

    val listed = issue.body.linesIterator
      .flatMap(parseRunnerTriplets)
      .toList

    val single =
      fields.get("agent/model/version").flatMap(parseRunnerTriplet).orElse {
        for
          agent <- fields
            .get("agent")
            .orElse(fields.get("executor"))
            .orElse(fields.get("llm"))
          model <- fields.get("model")
          version <- fields.get("version").orElse(fields.get("model version"))
        yield TaskRunner(
          agent = agent,
          model = Some(model),
          version = Some(version)
        )
      }

    (listed ++ single.toList).distinct

  private val RunnerFieldRegex =
    """(?i)^\s*(?:[-*]\s*)?(?:preferred\s+)?(agent/model/version|agent|executor|llm|model|model\s+version|version)\s*(?:is\s+)?(?::|=)\s*(.+?)\s*$""".r

  private def parseRunnerField(line: String): Option[(String, String)] =
    line match
      case RunnerFieldRegex(key, value) if value.nonEmpty =>
        Some(key.toLowerCase -> value.trim)
      case _ => None

  private def parseRunnerTriplet(value: String): Option[TaskRunner] =
    value.split("/").toList.map(_.trim).filter(_.nonEmpty) match
      case agent :: model :: version :: Nil =>
        Some(TaskRunner(agent, Some(model), Some(version)))
      case _ => None

  private def parseRunnerTriplets(line: String): List[TaskRunner] =
    val normalized = line.trim.stripPrefix("-").stripPrefix("*").trim
    val lower = normalized.toLowerCase
    val explicitRunnerLine =
      lower.contains("preferred llms/models/versions") ||
        lower.contains("agent/model/version")

    if explicitRunnerLine then
      val values =
        normalized.split(":", 2).toList match
          case _ :: tail :: Nil => tail
          case _                => normalized

      values.split(",").toList.flatMap(parseRunnerTriplet)
    else Nil

  private val DepLineKeywords =
    List("depends on", "depend on", "dependency", "dependencies")

  private val IssueNumRegex = """#(\d+)""".r
  private val ParentRegex = """(?i)\bparent\b\s*:?\s*#(\d+)""".r

  private def getDependencies(body: String): List[Int] =
    body.linesIterator
      .flatMap { line =>
        val lower = line.toLowerCase
        if DepLineKeywords.exists(keyword => lower.contains(keyword)) then
          IssueNumRegex.findAllMatchIn(line).map(_.group(1).toInt)
        else Nil
      }
      .toList
      .distinct

  def parentIds(issue: Issue): List[Int] =
    issue.body.linesIterator
      .flatMap(line => ParentRegex.findAllMatchIn(line).map(_.group(1).toInt))
      .toList
      .distinct

  def hasUnresolvedDependencies(
      issue: Issue,
      openIssueNumbers: Set[Int]
  ): Boolean =
    getDependencies(issue.body).exists(openIssueNumbers.contains)

  def hasOpenChildren(issue: Issue, openIssues: List[Issue]): Boolean =
    openIssues.exists(child => parentIds(child).contains(issue.number))

  def dependencyConclusion[F[_]](
      root: os.Path,
      task: Issue,
      progress: String => F[Unit]
  )(using F: Sync[F]): F[Option[String]] =
    getDependencies(task.body).headOption match
      case None => F.pure(None)
      case Some(dependencyId) =>
        progress(
          s"Found dependency task #$dependencyId. Fetching conclusion comment..."
        ) *>
          F.blocking {
            Try {
              val res = os
                .proc(
                  "gh",
                  "issue",
                  "view",
                  dependencyId.toString,
                  "--json",
                  "comments"
                )
                .call(cwd = root)
              val comments = ujson.read(res.out.text())("comments").arr.toList
              comments
                .find(
                  commentBody(_).exists(_.toLowerCase.contains("conclusion"))
                )
                .orElse(comments.lastOption)
                .flatMap(commentBody)
            }.toEither
          }.flatMap {
            case Right(comment) => F.pure(comment)
            case Left(error) =>
              progress(
                s"Failed to read comments for dependency task #$dependencyId: ${error.getMessage}"
              ).as(None)
          }

  private def commentBody(value: ujson.Value): Option[String] =
    value match
      case ujson.Obj(fields) =>
        fields.get("body").collect { case ujson.Str(body) => body }
      case _ => None

  def commentRunOutput[F[_]](
      root: os.Path,
      taskId: Int,
      output: String,
      progress: String => F[Unit]
  )(using F: Sync[F]): F[Unit] =
    for
      _ <- progress(s"Commenting run output on task #$taskId...")
      commentBody =
        s"""Task run output:
```
$output
```"""
      tempFile <- F.blocking(os.temp(commentBody))
      _ <- call(
        root,
        "gh",
        "issue",
        "comment",
        taskId.toString,
        "--body-file",
        tempFile.toString
      )
    yield ()

  def commentConclusion[F[_]](
      root: os.Path,
      task: Issue,
      runner: TaskRunner,
      progress: String => F[Unit]
  )(using Sync[F]): F[Unit] =
    val body =
      s"""Conclusion:
Task #${task.number} completed successfully.

Runner:
${runner.display}
"""
    progress(s"Leaving conclusion comment on task #${task.number}...") *>
      call(
        root,
        "gh",
        "issue",
        "comment",
        task.number.toString,
        "--body",
        body
      )

  def updateIssueBody[F[_]](
      root: os.Path,
      taskId: Int,
      body: String,
      progress: String => F[Unit]
  )(using F: Sync[F]): F[Unit] =
    for
      _ <- progress(s"Updating task #$taskId description...")
      tempFile <- F.blocking(os.temp(body))
      _ <- call(
        root,
        "gh",
        "issue",
        "edit",
        taskId.toString,
        "--body-file",
        tempFile.toString
      )
    yield ()

  def commentNeedsUserInput[F[_]](
      root: os.Path,
      task: Issue,
      questions: String,
      progress: String => F[Unit]
  )(using Sync[F]): F[Unit] =
    val body =
      s"""Questions before execution:
$questions
"""
    progress(s"Leaving questions on task #${task.number}...") *>
      call(
        root,
        "gh",
        "issue",
        "comment",
        task.number.toString,
        "--body",
        body
      )

  def commentSplitEvaluation[F[_]](
      root: os.Path,
      task: Issue,
      progress: String => F[Unit]
  )(using Sync[F]): F[Unit] =
    val body =
      s"""Task #${task.number} was evaluated as needing split subtasks.

This parent task will not be implemented directly. Run child tasks first; when all children are completed, the parent will be eligible for completion check.
"""
    progress(s"Leaving split-evaluation comment on task #${task.number}...") *>
      call(
        root,
        "gh",
        "issue",
        "comment",
        task.number.toString,
        "--body",
        body
      )

  def checkParentsForCompletion[F[_]](
      root: os.Path,
      task: Issue,
      progress: String => F[Unit]
  )(using Sync[F]): F[Unit] =
    parentIds(task).traverse_ { parentId =>
      for
        openIssues <- fetchIssues(root)
        openChildren = openIssues.filter(child =>
          parentIds(child).contains(parentId)
        )
        _ <-
          if openChildren.isEmpty then
            progress(
              s"All child tasks for parent #$parentId are completed. Leaving completion-check comment..."
            ) *>
              call(
                root,
                "gh",
                "issue",
                "comment",
                parentId.toString,
                "--body",
                s"All child tasks for parent #$parentId are completed. Parent task is ready for completion check."
              )
          else
            progress(
              s"Parent #$parentId still has ${openChildren.size} open child task(s)."
            )
      yield ()
    }

  def pushCreateAndMergePr[F[_]](
      root: os.Path,
      worktreePath: os.Path,
      branchName: String,
      task: Issue,
      progress: String => F[Unit]
  )(using Sync[F]): F[Unit] =
    for
      _ <- progress("Pushing branch to origin...")
      _ <- call(worktreePath, "git", "push", "-u", "origin", branchName)
      _ <- progress("Creating Pull Request...")
      _ <- call(
        worktreePath,
        "gh",
        "pr",
        "create",
        "--title",
        s"Task #${task.number}: ${task.title}",
        "--body",
        s"Closes #${task.number}",
        "--head",
        branchName
      )
      _ <- progress("Merging Pull Request...")
      _ <- call(root, "gh", "pr", "merge", branchName, "--merge")
    yield ()

  def closeIssue[F[_]: Sync](root: os.Path, taskId: Int): F[Unit] =
    call(
      root,
      "gh",
      "issue",
      "close",
      taskId.toString,
      "--comment",
      s"Task #$taskId completed successfully. Worktree closed."
    )

  def setIssueStatus[F[_]](
      root: os.Path,
      taskId: Int,
      status: String,
      progress: String => F[Unit]
  )(using Sync[F]): F[Unit] =
    val (toAdd, toRemove) =
      if status === "in progress" then
        (
          List("status: in progress", "in progress"),
          List("status: completed", "completed")
        )
      else
        (
          List("status: completed", "completed"),
          List("status: in progress", "in progress")
        )

    val addFlags = toAdd.flatMap(label => Seq("--add-label", label))
    val removeFlags = toRemove.flatMap(label => Seq("--remove-label", label))
    val fullCmd =
      Seq("gh", "issue", "edit", taskId.toString) ++ addFlags ++ removeFlags

    call(root, fullCmd*).handleErrorWith { error =>
      progress(
        s"Warning: Failed to update GitHub labels for task #$taskId: ${error.getMessage}"
      )
    } *>
      call(root, "ght", "status", taskId.toString, status).attempt.void

  private def call[F[_]: Sync](cwd: os.Path, command: String*): F[Unit] =
    Sync[F].blocking {
      os.proc(command).call(cwd = cwd)
      ()
    }
