import arrowstep.core.*
import arrowstep.runtime.AgentMain
import arrowstep.runtime.ReplayAsk
import cats.data.Kleisli
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.all.*

import scala.annotation.tailrec
import scala.util.Try

final case class Issue(
    number: Int,
    title: String,
    body: String,
    state: String
)

final case class AppInput(root: os.Path, executorOverride: Option[String])

final case class RunContext(root: os.Path, executor: String)

final case class TaskSelection(context: RunContext, task: Option[Issue])

final case class RunSummary(
    status: String,
    message: String,
    task: Option[Issue]
):
  def toJson: ujson.Value =
    ujson.Obj.from(
      Seq(
        "status" -> ujson.Str(status),
        "message" -> ujson.Str(message),
        "task" -> task.fold[ujson.Value](ujson.Null) { issue =>
          ujson.Obj.from(
            Seq(
              "number" -> ujson.Num(issue.number),
              "title" -> ujson.Str(issue.title),
              "state" -> ujson.Str(issue.state)
            )
          )
        }
      )
    )

object Main extends IOApp:

  type -->[A, B] = Kleisli[IO, A, B]
  def run(args: List[String]): IO[ExitCode] =
    val parsed = extractExecutorArgs(args)
    AgentMain
      .run[IO](parsed.arrowstepArgs, os.pwd)(_ =>
        program(AppInput(os.pwd, parsed.executor))
      )
      .flatMap { outcome =>
        IO.print(outcome.stdout) *>
          IO.whenA(outcome.stderr.nonEmpty)(
            IO.delay(Console.err.print(outcome.stderr))
          ) *>
          IO.pure(ExitCode(outcome.exitCode))
      }

  private def program(input: AppInput): IO[ProgramSays[ujson.Value]] =
    taskFlow.run(input).map(summary => ProgramSays.Done(summary.toJson))

  private def taskFlow: AppInput --> RunSummary =
    resolveExecutor >>> selectTask >>> executeTask

  private def resolveExecutor: AppInput --> RunContext =
    Kleisli.apply{ input =>
      input.executorOverride match
        case Some(executor) =>
          IO.pure(RunContext(input.root, executor))
        case None =>
          for
            valid <- ReplayAsk
              .askUntilValid[IO](input.root, Validator.basic[IO])
              .run(executorInput)
            executor <- IO.fromOption(valid.toMap.get("executor"))(
              new RuntimeException("validated answers did not contain executor")
            )
          yield RunContext(input.root, executor)
    }

  private def selectTask: RunContext --> TaskSelection =
    Kleisli{ (context: RunContext) =>
      for
        _ <- progress("Fetching open issues from GitHub...")
        issues <- fetchIssues(context.root)
        candidates = issues.filter(hasTargetExecutor(context.executor))
        _ <- progress(
          s"Found ${candidates.size} open tasks with '${context.executor}' preferred executor."
        )
        openIssueNumbers = issues.map(_.number).toSet
        nextTask = candidates.find(task =>
          !hasUnresolvedDependencies(task, openIssueNumbers)
        )
      yield TaskSelection(context, nextTask)
    }

  private def executeTask: TaskSelection --> RunSummary =
    Kleisli.apply {
      case TaskSelection(context, None) =>
        IO.pure(
          RunSummary(
            status = "no-task",
            message =
              s"No tasks found without unresolved dependencies and with '${context.executor}' executor.",
            task = None
          )
        )

      case TaskSelection(context, Some(task)) =>
        runTask(context, task).as(
          RunSummary(
            status = "completed",
            message = s"Task #${task.number} completed successfully.",
            task = Some(task)
          )
        )
    }

  private def runTask(context: RunContext, task: Issue): IO[Unit] =
    val taskId = task.number
    val worktreePath = context.root / os.up / s"task-$taskId"
    val branchName = s"task-$taskId"

    for
      _ <- progress(s"Selected next task: #$taskId - ${task.title}")
      _ <- setIssueStatus(context.root, taskId, "in progress")
      parentConclusion <- parentConclusion(context.root, task)
      _ <- ensureWorktree(context.root, worktreePath, branchName)
      prompt = taskPrompt(task, parentConclusion)
      _ <- progress(s"Running task #$taskId with ${context.executor}...")
      output <- runCommandAndCapture(
        Seq(context.executor, "-p", prompt),
        worktreePath
      )
      _ <- commentRunOutput(context.root, taskId, output)
      changed <- filesChanged(worktreePath)
      _ <-
        if changed then
          commitAndMerge(context.root, worktreePath, branchName, task)
        else progress("No files changed.")
      _ <- cleanupWorktree(context.root, worktreePath, branchName)
      _ <- progress(s"Closing task #$taskId with comment...")
      _ <- setIssueStatus(context.root, taskId, "completed")
      _ <- closeIssue(context.root, taskId)
      _ <- progress("Task execution finished successfully.")
    yield ()

  private def executorInput: AskInput =
    AskInput(
      questions = List(
        Question(
          id = "executor",
          text = "Which LLM executor command should run selected GitHub tasks?",
          kind = QuestionKind.FreeText,
          default = Some("codex"),
          current = None,
          context = Some(
            "Use the same command name you would pass to --executor, for example codex, claude, gemini, or agy."
          )
        )
      ),
      context = Some(
        "gh-tasks-llm-executor needs an executor command before it can start a task worktree."
      )
    )

  private def fetchIssues(root: os.Path): IO[List[Issue]] =
    IO.blocking {
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

  private def hasTargetExecutor(executor: String)(issue: Issue): Boolean =
    val quotedExecutor = java.util.regex.Pattern.quote(executor)
    val executorRegex =
      s"""(?i)(?:preferr?ed\\s+executor|executor)\\s*(?:is\\s+)?(?:is:?\\s+)?:?\\s*$quotedExecutor""".r
    executorRegex.findFirstIn(issue.body).isDefined

  private val DepLineKeywords =
    List("depends on", "depend on", "dependency", "dependencies", "parent")

  private val IssueNumRegex = """#(\d+)""".r

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

  private def hasUnresolvedDependencies(
      issue: Issue,
      openIssueNumbers: Set[Int]
  ): Boolean =
    getDependencies(issue.body).exists(openIssueNumbers.contains)

  private def parentConclusion(root: os.Path, task: Issue): IO[Option[String]] =
    val ParentRegex = """(?i)parent:?\s*#(\d+)""".r
    ParentRegex.findFirstMatchIn(task.body).map(_.group(1).toInt) match
      case None => IO.pure(None)
      case Some(parentId) =>
        progress(
          s"Found parent task #$parentId. Fetching conclusion comment..."
        ) *>
          IO.blocking {
            Try {
              val res = os
                .proc(
                  "gh",
                  "issue",
                  "view",
                  parentId.toString,
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
            case Right(comment) => IO.pure(comment)
            case Left(error) =>
              progress(
                s"Failed to read comments for parent task #$parentId: ${error.getMessage}"
              ).as(None)
          }

  private def commentBody(value: ujson.Value): Option[String] =
    value match
      case ujson.Obj(fields) =>
        fields.get("body").collect { case ujson.Str(body) => body }
      case _ => None

  private def ensureWorktree(
      root: os.Path,
      worktreePath: os.Path,
      branchName: String
  ): IO[Unit] =
    for
      _ <- progress(s"Starting new worktree at $worktreePath")
      _ <- removeExistingWorktree(root, worktreePath)
      exists <- branchExists(root, branchName)
      _ <-
        if exists then
          progress(
            s"Branch $branchName already exists, adding worktree for it"
          ) *>
            call(
              root,
              "git",
              "worktree",
              "add",
              worktreePath.toString,
              branchName
            )
        else
          progress(s"Creating branch $branchName and adding worktree") *>
            call(
              root,
              "git",
              "worktree",
              "add",
              "-b",
              branchName,
              worktreePath.toString
            )
    yield ()

  private def removeExistingWorktree(
      root: os.Path,
      worktreePath: os.Path
  ): IO[Unit] =
    IO.blocking(os.exists(worktreePath)).flatMap {
      case false => IO.unit
      case true =>
        progress(
          s"Worktree directory $worktreePath already exists. Cleaning up..."
        ) *>
          call(
            root,
            "git",
            "worktree",
            "remove",
            "--force",
            worktreePath.toString
          ).attempt.void *>
          IO.blocking {
            if os.exists(worktreePath) then os.remove.all(worktreePath)
          }
    }

  private def branchExists(root: os.Path, branchName: String): IO[Boolean] =
    IO.blocking {
      Try {
        os.proc("git", "show-ref", "--verify", s"refs/heads/$branchName")
          .call(cwd = root)
      }.isSuccess
    }

  private def taskPrompt(
      task: Issue,
      parentConclusion: Option[String]
  ): String =
    val parentConclusionStr = parentConclusion
      .map(comment => s"\nParent Task Conclusion Comment:\n$comment\n")
      .getOrElse("")

    s"""Task ID: #${task.number}
Title: ${task.title}

Task Description:
${task.body}
$parentConclusionStr
Please implement this task in the current repository. Make any necessary file changes.
"""

  private def runCommandAndCapture(cmd: Seq[String], cwd: os.Path): IO[String] =
    IO.blocking {
      val result = os
        .proc(cmd)
        .call(cwd = cwd, stdout = os.Pipe, stderr = os.Pipe, check = false)
      val stdout = result.out.text()
      val stderr = result.err.text()
      val output = stdout + stderr
      if output.nonEmpty then Console.err.print(output)
      if result.exitCode === 0 then output
      else
        throw new RuntimeException(
          s"${cmd.headOption.getOrElse("command")} exited with ${result.exitCode}"
        )
    }

  private def commentRunOutput(
      root: os.Path,
      taskId: Int,
      output: String
  ): IO[Unit] =
    for
      _ <- progress(s"Commenting run output on task #$taskId...")
      commentBody =
        s"""Task run output:
```
$output
```"""
      tempFile <- IO.blocking(os.temp(commentBody))
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

  private def filesChanged(worktreePath: os.Path): IO[Boolean] =
    IO.blocking {
      os.proc("git", "status", "--porcelain")
        .call(cwd = worktreePath)
        .out
        .text()
        .trim
        .nonEmpty
    }

  private def commitAndMerge(
      root: os.Path,
      worktreePath: os.Path,
      branchName: String,
      task: Issue
  ): IO[Unit] =
    for
      _ <- progress("Files changed. Committing and merging changes...")
      _ <- call(worktreePath, "git", "add", "-A")
      _ <- call(
        worktreePath,
        "git",
        "commit",
        "-m",
        s"Implement task #${task.number}: ${task.title}"
      )
      hasRemote <- IO.blocking(
        os.proc("git", "remote").call(cwd = root).out.text().trim.nonEmpty
      )
      _ <-
        if hasRemote then pushCreateAndMergePr(worktreePath, branchName, task)
        else mergeLocally(root, worktreePath, branchName)
    yield ()

  private def pushCreateAndMergePr(
      worktreePath: os.Path,
      branchName: String,
      task: Issue
  ): IO[Unit] =
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
      _ <- call(worktreePath, "gh", "pr", "merge", "--merge", "--delete-branch")
    yield ()

  private def mergeLocally(
      root: os.Path,
      worktreePath: os.Path,
      branchName: String
  ): IO[Unit] =
    for
      mainBranch <- IO.blocking(
        os.proc("git", "branch", "--show-current")
          .call(cwd = root)
          .out
          .text()
          .trim
      )
      _ <- progress(s"Removing worktree at $worktreePath")
      _ <- call(
        root,
        "git",
        "worktree",
        "remove",
        "--force",
        worktreePath.toString
      )
      _ <- progress(s"Merging branch $branchName into $mainBranch...")
      _ <- call(root, "git", "merge", branchName)
      _ <- progress(s"Deleting local branch $branchName...")
      _ <- call(root, "git", "branch", "-d", branchName)
    yield ()

  private def cleanupWorktree(
      root: os.Path,
      worktreePath: os.Path,
      branchName: String
  ): IO[Unit] =
    IO.blocking(os.exists(worktreePath)).flatMap {
      case false => IO.unit
      case true =>
        progress(s"Removing worktree at $worktreePath") *>
          call(
            root,
            "git",
            "worktree",
            "remove",
            "--force",
            worktreePath.toString
          ) *>
          call(root, "git", "branch", "-D", branchName).attempt.void
    }

  private def closeIssue(root: os.Path, taskId: Int): IO[Unit] =
    call(
      root,
      "gh",
      "issue",
      "close",
      taskId.toString,
      "--comment",
      s"Task #$taskId completed successfully. Worktree closed."
    )

  private def setIssueStatus(
      root: os.Path,
      taskId: Int,
      status: String
  ): IO[Unit] =
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

  private def call(cwd: os.Path, command: String*): IO[Unit] =
    IO.blocking {
      os.proc(command).call(cwd = cwd)
      ()
    }

  private def progress(message: String): IO[Unit] =
    IO.delay(Console.err.println(message))

  private final case class ParsedArgs(
      executor: Option[String],
      arrowstepArgs: List[String]
  )

  private def extractExecutorArgs(args: List[String]): ParsedArgs =
    @tailrec
    def loop(
        remaining: List[String],
        clean: List[String],
        executor: Option[String]
    ): ParsedArgs =
      remaining match
        case Nil => ParsedArgs(executor, clean.reverse)
        case ("--executor" | "--llm") :: value :: tail =>
          loop(tail, clean, Some(value))
        case flag :: Nil if flag === "--executor" || flag === "--llm" =>
          loop(Nil, flag :: clean, executor)
        case head :: tail =>
          loop(tail, head :: clean, executor)

    loop(args, Nil, None)
