import cats.effect.kernel.Sync
import cats.syntax.all.*

import scala.util.Try

final class Git[F[_]](using F: Sync[F]):

  def removeExistingWorktree(
      root: os.Path,
      worktreePath: os.Path,
      progress: String => F[Unit]
  ): F[Unit] =
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

  def branchExists(root: os.Path, branchName: String): F[Boolean] =
    F.blocking {
      Try {
        os.proc("git", "show-ref", "--verify", s"refs/heads/$branchName")
          .call(cwd = root)
      }.isSuccess
    }

  def addExistingBranchWorktree(
      root: os.Path,
      worktreePath: os.Path,
      branchName: String,
      progress: String => F[Unit]
  ): F[Unit] =
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

  def addNewBranchWorktree(
      root: os.Path,
      worktreePath: os.Path,
      branchName: String,
      progress: String => F[Unit]
  ): F[Unit] =
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

  def filesChanged(worktreePath: os.Path): F[Boolean] =
    F.blocking {
      os.proc("git", "status", "--porcelain")
        .call(cwd = worktreePath)
        .out
        .text()
        .trim
        .nonEmpty
    }

  def commitAll(worktreePath: os.Path, task: Issue): F[Unit] =
    call(worktreePath, "git", "add", "-A") *>
      call(
        worktreePath,
        "git",
        "commit",
        "-m",
        s"Implement task #${task.number}: ${task.title}"
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
    F.blocking {
      os.proc(command).call(cwd = cwd)
      ()
    }

object Git:
  def apply[F[_]: Sync]: Git[F] = new Git[F]
