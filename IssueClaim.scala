import cats.effect.Resource
import cats.effect.kernel.Sync
import cats.syntax.all.*
import cats.data.Kleisli

final class IssueAlreadyClaimedException(taskNumber: TaskNumber)
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
      taskNumber: TaskNumber,
      progress: String => F[Unit]
  ): Resource[F, Unit] =
    Resource.make(claim[F](progress).run((root, taskNumber)))(_ => release[F](progress).run((root, taskNumber)))

  private val StaleThresholdSeconds = 4 * 60 * 60 // 4 hours

  private def checkAndReleaseIfStale[F[_]](progress: String => F[Unit])(using
      F: Sync[F]
  ): Kleisli[F, (os.Path, TaskNumber), Boolean] =
    Kleisli.apply { case (root, taskNumber) =>
      val ref = refName(taskNumber)
      for
        _ <- progress(s"Checking if claim on task #$taskNumber is stale...")
        fetchResult <- F.blocking(
          os.proc("git", "fetch", "origin", ref)
            .call(cwd = root, stdout = os.Pipe, stderr = os.Pipe, check = false)
        )
        stale <-
          if fetchResult.exitCode === 0 then
            for
              timestampStr <- F.blocking(
                os.proc("git", "show", "-s", "--format=%ct", "FETCH_HEAD")
                  .call(cwd = root, stderr = os.Pipe)
                  .out
                  .text()
                  .trim
              )
              commitTime = scala.util.Try(timestampStr.toLong).getOrElse(0L)
              currentTime = System.currentTimeMillis() / 1000L
              ageSeconds = currentTime - commitTime
              isStale = ageSeconds > StaleThresholdSeconds
              _ <-
                if isStale then
                  progress(
                    s"Task #$taskNumber claim is stale (${ageSeconds / 3600} hours old). Force-releasing it..."
                  ) *> F
                    .blocking(
                      os.proc("git", "push", "origin", "--delete", ref)
                        .call(
                          cwd = root,
                          stdout = os.Pipe,
                          stderr = os.Pipe,
                          check = false
                        )
                    )
                    .void
                else F.unit
            yield isStale
          else F.pure(false)
      yield stale
    }

  private def claim[F[_]](progress: String => F[Unit])(using
      F: Sync[F]
  ): Kleisli[F, (os.Path, TaskNumber), Unit] =
    Kleisli.apply { case (root, taskNumber) =>
      val uuid = java.util.UUID.randomUUID().toString
      for
        _ <- progress(s"Claiming task #$taskNumber...")
        commitHash <- F.blocking {
          os.proc(
            "git",
            "commit-tree",
            "HEAD^{tree}",
            "-m",
            s"Claim task $taskNumber - $uuid"
          ).call(cwd = root)
            .out
            .text()
            .trim
        }
        result <- F.blocking(
          os.proc("git", "push", "origin", s"$commitHash:${refName(taskNumber)}")
            .call(cwd = root, stdout = os.Pipe, stderr = os.Pipe, check = false)
        )
        _ <-
          if result.exitCode === 0 then progress(s"Claimed task #$taskNumber.")
          else
            val stderr = result.err.text()
            if isRefConflict(stderr) then
              checkAndReleaseIfStale[F](progress).run((root, taskNumber)).flatMap {
                case true =>
                  // Stale lock was released, try again
                  claim[F](progress).run((root, taskNumber))
                case false =>
                  F.raiseError(IssueAlreadyClaimedException(taskNumber))
              }
            else
              F.raiseError(
                new RuntimeException(
                  s"Failed to claim task #$taskNumber: ${stderr.trim}"
                )
              )
      yield ()
    }

  private def release[F[_]](progress: String => F[Unit])(using
      F: Sync[F]
  ): Kleisli[F, (os.Path, TaskNumber), Unit] =
    Kleisli.apply { case (root, taskNumber) =>
      progress(s"Releasing claim on task #$taskNumber...") *>
        F.blocking(
          os.proc("git", "push", "origin", "--delete", refName(taskNumber))
            .call(cwd = root, stdout = os.Pipe, stderr = os.Pipe, check = false)
        ).attempt
          .void
    }

  private def isRefConflict(stderr: String): Boolean =
    val lower = stderr.toLowerCase
    lower.contains("already exists") || lower.contains("stale info") ||
    lower.contains("fetch first") || lower.contains("non-fast-forward")

  private def refName(taskNumber: TaskNumber): String =
    s"refs/gh-tasks-llm-executor/claims/task-${taskNumber.value}"
