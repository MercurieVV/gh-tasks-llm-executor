import arrowstep.core.*
import arrowstep.runtime.AgentMain
import cats.arrow.ArrowChoice
import cats.data.Kleisli
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.kernel.Sync
import cats.syntax.all.*

import scala.annotation.tailrec
import scala.util.Try

final case class AppInput(root: os.Path)

final case class RunContext(root: os.Path)

final case class TaskRunner(
    agent: String,
    model: Option[String],
    version: Option[String]
):
  def display: String =
    val modelPart = model.fold("")(value => s", model: $value")
    val versionPart = version.fold("")(value => s", version: $value")
    s"agent: $agent$modelPart$versionPart"

  def command(prompt: String): Seq[String] =
    Seq(agent) ++ model.toList.flatMap(value => Seq("-m", value)) ++
      Seq("-p", prompt)

final case class RunnableTask(issue: Issue, runner: TaskRunner)

final case class TaskSelection(context: RunContext, task: Option[RunnableTask])

final case class NoTask(context: RunContext)

final case class TaskRun(
    context: RunContext,
    task: Issue,
    runner: TaskRunner,
    worktreePath: os.Path,
    branchName: String
)

final case class TaskWithPrompt(
    run: TaskRun,
    parentConclusion: Option[String]
)

final case class TaskWithOutput(run: TaskRun, output: String)

final case class ExistingBranch(run: TaskWithPrompt)

final case class NewBranch(run: TaskWithPrompt)

final case class ChangedTask(run: TaskWithOutput)

final case class UnchangedTask(run: TaskWithOutput)

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

  type -->[F[_], A, B] = Kleisli[F, A, B]
  type Flow[F[_]] = [A, B] =>> Kleisli[F, A, B]
  def run(args: List[String]): IO[ExitCode] =
    val arrowstepArgs = removeRunnerArgs(args)
    AgentMain
      .run[IO](arrowstepArgs, os.pwd)(_ => program(AppInput(os.pwd)))
      .flatMap { outcome =>
        IO.print(outcome.stdout) *>
          IO.whenA(outcome.stderr.nonEmpty)(
            IO.delay(Console.err.print(outcome.stderr))
          ) *>
          IO.pure(ExitCode(outcome.exitCode))
      }

  private def program[F[_]: Sync](
      input: AppInput
  ): F[ProgramSays[ujson.Value]] =
    taskFlow[F].run(input).map(summary => ProgramSays.Done(summary.toJson))

  private def taskFlow[F[_]: Sync]: -->[F, AppInput, RunSummary] =
    resolveContext[F] >>> selectTask[F] >>> executeTask[F]

  private def resolveContext[F[_]: Sync]: -->[F, AppInput, RunContext] =
    Kleisli(input => RunContext(input.root).pure[F])

  private def selectTask[F[_]: Sync]: -->[F, RunContext, TaskSelection] =
    Kleisli { (context: RunContext) =>
      for
        _ <- progress("Fetching open issues from GitHub...")
        issues <- GitHub.fetchIssues(context.root)
        openIssueNumbers = issues.map(_.number).toSet
        candidates = issues
          .filterNot(GitHub.hasUnresolvedDependencies(_, openIssueNumbers))
          .flatMap(task => GitHub.taskRunner(task).map(RunnableTask(task, _)))
        _ <- progress(
          s"Found ${candidates.size} runnable open tasks with agent/model/version metadata."
        )
        nextTask = candidates.headOption
      yield TaskSelection(context, nextTask)
    }

  private def executeTask[F[_]: Sync]: -->[F, TaskSelection, RunSummary] =
    selectedTask[F] >>> choose[F, NoTask, TaskRun, RunSummary](
      noTaskSummary[F],
      runSelectedTask[F]
    )

  private def selectedTask[F[_]: Sync]
      : -->[F, TaskSelection, Either[NoTask, TaskRun]] =
    Kleisli { selection =>
      selection.task
        .fold[Either[NoTask, TaskRun]](Left(NoTask(selection.context)))(task =>
          Right(taskRun(selection.context, task.issue, task.runner))
        )
        .pure[F]
    }

  private def noTaskSummary[F[_]: Sync]: -->[F, NoTask, RunSummary] =
    Kleisli { noTask =>
      val context = noTask.context
      RunSummary(
        status = "no-task",
        message =
          "No tasks found without unresolved dependencies and with agent/model/version metadata in the task description.",
        task = None
      ).pure[F]
    }

  private def runSelectedTask[F[_]: Sync]: -->[F, TaskRun, RunSummary] =
    taskExecution[F].map { run =>
      RunSummary(
        status = "completed",
        message = s"Task #${run.task.number} completed successfully.",
        task = Some(run.task)
      )
    }

  private def taskExecution[F[_]: Sync]: -->[F, TaskRun, TaskRun] =
    announceTask[F] >>>
      markInProgress[F] >>>
      fetchParentConclusion[F] >>>
      ensureTaskWorktree[F] >>>
      runExecutor[F] >>>
      commentOutput[F] >>>
      commitIfChanged[F] >>>
      cleanupTaskWorktree[F] >>>
      closeTask[F]

  private def announceTask[F[_]: Sync]: -->[F, TaskRun, TaskRun] =
    Kleisli { run =>
      progress(
        s"Selected next task: #${run.task.number} - ${run.task.title}"
      ).as(run)
    }

  private def markInProgress[F[_]: Sync]: -->[F, TaskRun, TaskRun] =
    Kleisli { run =>
      GitHub
        .setIssueStatus(
          run.context.root,
          run.task.number,
          "in progress",
          progress
        )
        .as(run)
    }

  private def fetchParentConclusion[
      F[_]: Sync
  ]: -->[F, TaskRun, TaskWithPrompt] =
    Kleisli { run =>
      GitHub
        .parentConclusion(run.context.root, run.task, progress)
        .map(parentConclusion => TaskWithPrompt(run, parentConclusion))
    }

  private def ensureTaskWorktree[
      F[_]: Sync
  ]: -->[F, TaskWithPrompt, TaskWithPrompt] =
    progressStartingWorktree[F] >>>
      removeExistingTaskWorktree[F] >>>
      branchPlan[F] >>>
      choose[F, ExistingBranch, NewBranch, TaskWithPrompt](
        addExistingBranchWorktree[F],
        addNewBranchWorktree[F]
      )

  private def progressStartingWorktree[
      F[_]: Sync
  ]: -->[F, TaskWithPrompt, TaskWithPrompt] =
    Kleisli { task =>
      progress(s"Starting new worktree at ${task.run.worktreePath}").as(task)
    }

  private def removeExistingTaskWorktree[
      F[_]: Sync
  ]: -->[F, TaskWithPrompt, TaskWithPrompt] =
    Kleisli { task =>
      removeExistingWorktree(
        task.run.context.root,
        task.run.worktreePath
      ).as(task)
    }

  private def branchPlan[
      F[_]: Sync
  ]: -->[F, TaskWithPrompt, Either[ExistingBranch, NewBranch]] =
    Kleisli { task =>
      branchExists(task.run.context.root, task.run.branchName).map {
        case true  => Left(ExistingBranch(task))
        case false => Right(NewBranch(task))
      }
    }

  private def addExistingBranchWorktree[
      F[_]: Sync
  ]: -->[F, ExistingBranch, TaskWithPrompt] =
    Kleisli { branch =>
      val run = branch.run.run
      progress(
        s"Branch ${run.branchName} already exists, adding worktree for it"
      ) *>
        call(
          run.context.root,
          "git",
          "worktree",
          "add",
          run.worktreePath.toString,
          run.branchName
        ).as(branch.run)
    }

  private def addNewBranchWorktree[
      F[_]: Sync
  ]: -->[F, NewBranch, TaskWithPrompt] =
    Kleisli { branch =>
      val run = branch.run.run
      progress(s"Creating branch ${run.branchName} and adding worktree") *>
        call(
          run.context.root,
          "git",
          "worktree",
          "add",
          "-b",
          run.branchName,
          run.worktreePath.toString
        ).as(branch.run)
    }

  private def runExecutor[F[_]: Sync]: -->[F, TaskWithPrompt, TaskWithOutput] =
    Kleisli { task =>
      val run = task.run
      val prompt = taskPrompt(run.task, run.runner, task.parentConclusion)
      for
        _ <- progress(
          s"Running task #${run.task.number} with ${run.runner.display}..."
        )
        output <- runCommandAndCapture(
          run.runner.command(prompt),
          run.worktreePath
        )
      yield TaskWithOutput(run, output)
    }

  private def commentOutput[F[_]: Sync]
      : -->[F, TaskWithOutput, TaskWithOutput] =
    Kleisli { task =>
      GitHub
        .commentRunOutput(
          task.run.context.root,
          task.run.task.number,
          task.output,
          progress
        )
        .as(task)
    }

  private def commitIfChanged[F[_]: Sync]
      : -->[F, TaskWithOutput, TaskWithOutput] =
    changedPlan[F] >>> choose[F, ChangedTask, UnchangedTask, TaskWithOutput](
      commitChangedTask[F],
      reportUnchangedTask[F]
    )

  private def changedPlan[F[_]: Sync]
      : -->[F, TaskWithOutput, Either[ChangedTask, UnchangedTask]] =
    Kleisli { task =>
      filesChanged(task.run.worktreePath).map {
        case true  => Left(ChangedTask(task))
        case false => Right(UnchangedTask(task))
      }
    }

  private def commitChangedTask[
      F[_]: Sync
  ]: -->[F, ChangedTask, TaskWithOutput] =
    Kleisli { changed =>
      val run = changed.run.run
      commitAndMerge(
        run.context.root,
        run.worktreePath,
        run.branchName,
        run.task
      ).as(changed.run)
    }

  private def reportUnchangedTask[
      F[_]: Sync
  ]: -->[F, UnchangedTask, TaskWithOutput] =
    Kleisli { unchanged =>
      progress("No files changed.").as(unchanged.run)
    }

  private def cleanupTaskWorktree[
      F[_]: Sync
  ]: -->[F, TaskWithOutput, TaskWithOutput] =
    Kleisli { task =>
      cleanupWorktree(
        task.run.context.root,
        task.run.worktreePath,
        task.run.branchName
      ).as(task)
    }

  private def closeTask[F[_]: Sync]: -->[F, TaskWithOutput, TaskRun] =
    Kleisli { task =>
      val run = task.run
      for
        _ <- progress(s"Closing task #${run.task.number} with comment...")
        _ <- GitHub.setIssueStatus(
          run.context.root,
          run.task.number,
          "completed",
          progress
        )
        _ <- GitHub.closeIssue(run.context.root, run.task.number)
        _ <- progress("Task execution finished successfully.")
      yield run
    }

  private def choose[F[_]: Sync, A, B, C](
      left: -->[F, A, C],
      right: -->[F, B, C]
  ): -->[F, Either[A, B], C] =
    ArrowChoice[Flow[F]].choice(left, right)

  private def taskRun(
      context: RunContext,
      task: Issue,
      runner: TaskRunner
  ): TaskRun =
    val taskId = task.number
    TaskRun(
      context = context,
      task = task,
      runner = runner,
      worktreePath = context.root / os.up / s"task-$taskId",
      branchName = s"task-$taskId"
    )

  private def removeExistingWorktree[F[_]](
      root: os.Path,
      worktreePath: os.Path
  )(using F: Sync[F]): F[Unit] =
    F.blocking(os.exists(worktreePath)).flatMap {
      case false => F.unit
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
          F.blocking {
            if os.exists(worktreePath) then os.remove.all(worktreePath)
          }
    }

  private def branchExists[F[_]: Sync](
      root: os.Path,
      branchName: String
  ): F[Boolean] =
    Sync[F].blocking {
      Try {
        os.proc("git", "show-ref", "--verify", s"refs/heads/$branchName")
          .call(cwd = root)
      }.isSuccess
    }

  private def taskPrompt(
      task: Issue,
      runner: TaskRunner,
      parentConclusion: Option[String]
  ): String =
    val parentConclusionStr = parentConclusion
      .map(comment => s"\nParent Task Conclusion Comment:\n$comment\n")
      .getOrElse("")

    s"""Task ID: #${task.number}
Title: ${task.title}
Agent: ${runner.agent}
Model: ${runner.model.getOrElse("")}
Version: ${runner.version.getOrElse("")}

Task Description:
${task.body}
$parentConclusionStr
Please implement this task in the current repository. Make any necessary file changes.
"""

  private def runCommandAndCapture[F[_]: Sync](
      cmd: Seq[String],
      cwd: os.Path
  ): F[String] =
    Sync[F].blocking {
      val result = os
        .proc(cmd)
        .call(cwd = cwd, stdout = os.Pipe, stderr = os.Pipe, check = false)
      val stdout = result.out.text()
      val stderr = result.err.text()
      val output = stdout + stderr
      if output.nonEmpty then Console.err.print(output)
      Either.cond(
        result.exitCode === 0,
        output,
        new RuntimeException(
          s"${cmd.headOption.getOrElse("command")} exited with ${result.exitCode}"
        )
      )
    }.rethrow

  private def filesChanged[F[_]: Sync](worktreePath: os.Path): F[Boolean] =
    Sync[F].blocking {
      os.proc("git", "status", "--porcelain")
        .call(cwd = worktreePath)
        .out
        .text()
        .trim
        .nonEmpty
    }

  private def commitAndMerge[F[_]](
      root: os.Path,
      worktreePath: os.Path,
      branchName: String,
      task: Issue
  )(using Sync[F]): F[Unit] =
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
      hasRemote <- Sync[F].blocking(
        os.proc("git", "remote").call(cwd = root).out.text().trim.nonEmpty
      )
      _ <-
        if hasRemote then
          GitHub.pushCreateAndMergePr(worktreePath, branchName, task, progress)
        else mergeLocally(root, worktreePath, branchName)
    yield ()

  private def mergeLocally[F[_]](
      root: os.Path,
      worktreePath: os.Path,
      branchName: String
  )(using Sync[F]): F[Unit] =
    for
      mainBranch <- Sync[F].blocking(
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

  private def cleanupWorktree[F[_]](
      root: os.Path,
      worktreePath: os.Path,
      branchName: String
  )(using F: Sync[F]): F[Unit] =
    F.blocking(os.exists(worktreePath)).flatMap {
      case false => F.unit
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

  private def call[F[_]: Sync](cwd: os.Path, command: String*): F[Unit] =
    Sync[F].blocking {
      os.proc(command).call(cwd = cwd)
      ()
    }

  private def progress[F[_]: Sync](message: String): F[Unit] =
    Sync[F].delay(Console.err.println(message))

  private def removeRunnerArgs(args: List[String]): List[String] =
    @tailrec
    def loop(
        remaining: List[String],
        clean: List[String]
    ): List[String] =
      remaining match
        case Nil => clean.reverse
        case ("--executor" | "--llm" | "--agent" | "--model") :: _ :: tail =>
          loop(tail, clean)
        case flag :: Nil
            if flag === "--executor" || flag === "--llm" ||
              flag === "--agent" || flag === "--model" =>
          loop(Nil, clean)
        case head :: tail =>
          loop(tail, head :: clean)

    loop(args, Nil)
