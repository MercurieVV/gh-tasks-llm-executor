import cats.effect.Resource
import cats.effect.kernel.Sync
import cats.syntax.all.*

final class IssueAlreadyClaimedException(taskNumber: Int)
    extends RuntimeException(
      s"Task #$taskNumber is already claimed by another process."
    )

// Cross-process claim on a GitHub issue, independent of the local
// git-worktree/branch locking in Git.scala. Backed by a push of a
// dedicated ref to origin: ref creation is atomic on the git server, so
// two processes racing on the same issue can never both succeed.
object IssueClaim:

  def acquire[F[_]: Sync](
      root: os.Path,
      taskNumber: Int,
      progress: String => F[Unit]
  ): Resource[F, Unit] =
    Resource.make(claim[F](root, taskNumber, progress))(_ =>
      release[F](root, taskNumber, progress)
    )

  private def claim[F[_]](
      root: os.Path,
      taskNumber: Int,
      progress: String => F[Unit]
  )(using F: Sync[F]): F[Unit] =
    for
      _ <- progress(s"Claiming task #$taskNumber...")
      result <- F.blocking(
        os.proc("git", "push", "origin", s"HEAD:${refName(taskNumber)}")
          .call(cwd = root, stdout = os.Pipe, stderr = os.Pipe, check = false)
      )
      _ <-
        if result.exitCode === 0 then progress(s"Claimed task #$taskNumber.")
        else
          val stderr = result.err.text()
          if isRefConflict(stderr) then
            F.raiseError(IssueAlreadyClaimedException(taskNumber))
          else
            F.raiseError(
              new RuntimeException(
                s"Failed to claim task #$taskNumber: ${stderr.trim}"
              )
            )
    yield ()

  private def release[F[_]](
      root: os.Path,
      taskNumber: Int,
      progress: String => F[Unit]
  )(using F: Sync[F]): F[Unit] =
    progress(s"Releasing claim on task #$taskNumber...") *>
      F.blocking(
        os.proc("git", "push", "origin", "--delete", refName(taskNumber))
          .call(cwd = root, stdout = os.Pipe, stderr = os.Pipe, check = false)
      ).attempt.void

  private def isRefConflict(stderr: String): Boolean =
    val lower = stderr.toLowerCase
    lower.contains("already exists") || lower.contains("stale info") ||
    lower.contains("fetch first") || lower.contains("non-fast-forward")

  private def refName(taskNumber: Int): String =
    s"refs/gh-tasks-llm-executor/claims/task-$taskNumber"