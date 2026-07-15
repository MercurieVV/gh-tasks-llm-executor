import cats.effect.kernel.Sync
import cats.syntax.all.*

final class Git[F[_]](using F: Sync[F]):

  def acquireWorktree(
      root: os.Path,
      worktreePath: os.Path,
      branchName: String,
      progress: String => F[Unit]
  ): F[Unit] =
    F.blocking(os.exists(worktreePath)).flatMap {
      case true =>
        F.raiseError(
          new RuntimeException(
            s"Cannot acquire worktree: $worktreePath already exists."
          )
        )
      case false =>
        progress(s"Creating worktree at $worktreePath on branch $branchName") *>
          call(
            root,
            "git",
            "worktree",
            "add",
            "-b",
            branchName,
            worktreePath.toString
          )
    }

  def releaseWorktree(
      root: os.Path,
      worktreePath: os.Path,
      branchName: String,
      progress: String => F[Unit]
  ): F[Unit] =
    for
      _ <- progress(s"Returning to project root at $root")
      _ <- call(root, "git", "status", "--short").void
      _ <- F.blocking(os.exists(worktreePath)).flatMap {
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
            )
      }
      _ <- call(root, "git", "branch", "-D", branchName).attempt.void
    yield ()

  def filesChanged(worktreePath: os.Path): F[Boolean] =
    F.blocking {
      os.proc("git", "status", "--porcelain")
        .call(cwd = worktreePath)
        .out
        .text()
        .trim
        .nonEmpty
    }

  def hasPublishableCommits(
      worktreePath: os.Path,
      branchName: String
  ): F[Boolean] =
    F.blocking {
      val remoteBranch = s"origin/$branchName"
      val hasRemoteBranch =
        os.proc("git", "rev-parse", "--verify", remoteBranch)
          .call(
            cwd = worktreePath,
            stdout = os.Pipe,
            stderr = os.Pipe,
            check = false
          )
          .exitCode === 0
      val baseRef = if hasRemoteBranch then remoteBranch else "origin/HEAD"
      val result =
        os.proc("git", "rev-list", "--count", s"$baseRef..HEAD")
          .call(
            cwd = worktreePath,
            stdout = os.Pipe,
            stderr = os.Pipe,
            check = false
          )
      result.exitCode === 0 && result.out.text().trim.toIntOption.exists(_ > 0)
    }

  def runProjectValidation(
      worktreePath: os.Path,
      progress: String => F[Unit]
  ): F[Unit] =
    validationHook(worktreePath).flatMap {
      case None =>
        progress(
          "No project validation hook found. Skipping local project checks."
        )
      case Some(hook) =>
        for
          _ <- progress(s"Running project validation hook: $hook")
          result <- F.blocking {
            os.proc(hook)
              .call(
                cwd = worktreePath,
                stdout = os.Pipe,
                stderr = os.Pipe,
                check = false
              )
          }
          stdout <- F.blocking(result.out.text())
          stderr <- F.blocking(result.err.text())
          output = (stdout + stderr).trim
          _ <-
            if output.nonEmpty then progress(output)
            else progress("Project validation hook produced no output.")
          _ <-
            if result.exitCode === 0 then
              progress("Project validation hook passed.")
            else
              F.raiseError(
                new RuntimeException(
                  s"Project validation hook failed with exit code ${result.exitCode}."
                )
              )
        yield ()
    }

  private def validationHook(worktreePath: os.Path): F[Option[os.Path]] =
    F.blocking {
      List(
        worktreePath / ".gh-tasks-llm-executor" / "validate",
        worktreePath / ".github" / "gh-tasks-llm-executor" / "validate",
        worktreePath / "scripts" / "gh-task-validate"
      ).find(path => os.exists(path) && os.isFile(path))
    }

  def commitAll(
      worktreePath: os.Path,
      task: Issue,
      commitTitle: Option[String] = None
  ): F[Unit] =
    call(worktreePath, "git", "add", "-A") *>
      call(
        worktreePath,
        "git",
        "commit",
        "-m",
        commitTitle
          .filter(_.trim.nonEmpty)
          .getOrElse(s"Implement task #${task.number}: ${task.title}")
      )

  def hasRemote(root: os.Path): F[Boolean] =
    F.blocking(
      os.proc("git", "remote").call(cwd = root).out.text().trim.nonEmpty
    )

  def mergeLocally(
      root: os.Path,
      worktreePath: os.Path,
      branchName: String,
      progress: String => F[Unit]
  ): F[Unit] =
    for
      mainBranch <- F.blocking(
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

  def cleanupWorktree(
      root: os.Path,
      worktreePath: os.Path,
      branchName: String,
      progress: String => F[Unit]
  ): F[Unit] =
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

  private def call(cwd: os.Path, command: String*): F[Unit] =
    TaskLogger.trace[F](s"command cwd=$cwd args=${formatCommand(command)}") *>
      F.blocking {
        os
          .proc(command)
          .call(cwd = cwd, stdout = os.Pipe, stderr = os.Pipe, check = false)
      }.flatMap { result =>
        for
          stdout <- F.blocking(result.out.text().trim)
          stderr <- F.blocking(result.err.text().trim)
          _ <-
            if stdout.nonEmpty then
              TaskLogger.trace[F](s"command stdout ${truncate(stdout)}")
            else F.unit
          _ <-
            if stderr.nonEmpty then
              TaskLogger.trace[F](s"command stderr ${truncate(stderr)}")
            else F.unit
          _ <-
            if result.exitCode === 0 then F.unit
            else
              F.raiseError(
                new RuntimeException(
                  s"Command failed with exit code ${result.exitCode}: ${formatCommand(command)}"
                )
              )
        yield ()
      }

  private def formatCommand(command: Seq[String]): String =
    command.map(value => s""""${truncate(value)}"""").mkString(" ")

  private def truncate(value: String): String =
    if value.length <= 160 then value else value.take(160) + "...[truncated]"

object Git:
  def apply[F[_]: Sync]: Git[F] = new Git[F]
