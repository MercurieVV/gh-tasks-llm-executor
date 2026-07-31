package com.github.mercurievv.ghllm.git

import com.github.mercurievv.ghllm.*
import com.github.mercurievv.ghllm.agent.*
import com.github.mercurievv.ghllm.arrow.*
import com.github.mercurievv.ghllm.cli.*
import com.github.mercurievv.ghllm.git.*
import com.github.mercurievv.ghllm.metrics.*

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

  /** A network flap, not a verdict from the server.
    *
    * The distinction decides whether the task is retried or dropped: on 2026-07-31 a resolver failure mid-run made two
    * queued tasks "fail unrecoverably" and skipped them for the rest of the run, while the claim ref they never took
    * stayed behind on origin. Neither is a statement about the task.
    */
  def isTransientNetwork(stderr: String): Boolean =
    val lower = stderr.toLowerCase
    List(
      "could not resolve host",
      "temporary failure in name resolution",
      "connection timed out",
      "connection reset",
      "operation timed out",
      "i/o timeout",
      "failed to connect",
      "the remote end hung up",
      "rpc failed",
      "unexpected disconnect",
      "502 bad gateway",
      "503 service unavailable",
      "504 gateway"
    ).exists(lower.contains)

  // Three attempts over ~1 minute: long enough to ride out a resolver blip or a
  // laptop changing networks, short enough that a genuine outage still ends the
  // run rather than parking it.
  private val TransientBackoff = List(5000L, 15000L, 40000L)

  /** Re-runs `attempt` while it reports a transient network failure. `attempt` returns the process result so this can
    * read stderr without deciding what a non-transient exit code means — that stays with the caller.
    */
  def retryOnTransient[F[_]](progress: String => F[Unit])(
      what: String,
      attempt: F[os.CommandResult]
  )(using F: Sync[F]): F[os.CommandResult] =
    def loop(remaining: List[Long]): F[os.CommandResult] =
      attempt.flatMap { result =>
        val stderr = result.err.text()
        if result.exitCode === 0 || !isTransientNetwork(stderr) then result.pure[F]
        else
          remaining match
            case Nil => result.pure[F]
            case delay :: rest =>
              progress(
                s"$what failed on a network error: ${stderr.trim.linesIterator.toList.headOption.getOrElse("")}. Retrying in ${delay / 1000}s..."
              ) *> F.blocking(Thread.sleep(delay)) *> loop(rest)
      }

    loop(TransientBackoff)

  private val StaleThresholdSeconds = 4 * 60 * 60 // 4 hours

  def hasActiveClaim[F[_]: Sync](
      root: os.Path,
      taskNumber: TaskNumber,
      progress: String => F[Unit]
  ): F[Boolean] =
    checkAndReleaseIfStale[F](progress).run((root, taskNumber)).flatMap {
      case true => false.pure[F]
      case false =>
        Sync[F].blocking {
          os.proc("git", "ls-remote", "--exit-code", "origin", refName(taskNumber))
            .call(cwd = root, stdout = os.Pipe, stderr = os.Pipe, check = false)
            .exitCode == 0
        }
    }

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
        result <- retryOnTransient[F](progress)(
          s"Claiming task #$taskNumber",
          F.blocking(
            os.proc("git", "push", "origin", s"$commitHash:${refName(taskNumber)}")
              .call(cwd = root, stdout = os.Pipe, stderr = os.Pipe, check = false)
          )
        )
        _ <-
          if result.exitCode === 0 then
            progress(s"Claimed task #$taskNumber.") *> (
              if Cli.fetchOriginEnabled then
                progress(s"Fetching branch changes for task #$taskNumber from origin...") *>
                  F.blocking {
                    os.proc("git", "fetch", "origin", s"task-${taskNumber.value}")
                      .call(cwd = root, stdout = os.Pipe, stderr = os.Pipe, check = false)
                  }.void
              else F.unit
            )
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
      // A release that quietly fails leaves the ref on origin, and the next run
      // reads it as another process holding the task - for four hours, until the
      // staleness threshold expires it. Worth retrying, and worth saying so when
      // the retries run out.
      progress(s"Releasing claim on task #$taskNumber...") *>
        retryOnTransient[F](progress)(
          s"Releasing claim on task #$taskNumber",
          F.blocking(
            os.proc("git", "push", "origin", "--delete", refName(taskNumber))
              .call(cwd = root, stdout = os.Pipe, stderr = os.Pipe, check = false)
          )
        ).attempt.flatMap {
          case Right(result) if result.exitCode === 0 => F.unit
          case Right(result) =>
            progress(
              s"Warning: claim ref for task #$taskNumber was not released: ${result.err.text().trim}. " +
                s"Release it with scripts/unclaim-task.scala -- ${taskNumber.value} if the next run reports it claimed."
            )
          case Left(error) =>
            progress(s"Warning: failed to release claim on task #$taskNumber: ${error.getMessage}")
        }
    }

  private def isRefConflict(stderr: String): Boolean =
    val lower = stderr.toLowerCase
    lower.contains("already exists") || lower.contains("stale info") ||
    lower.contains("fetch first") || lower.contains("non-fast-forward")

  private def refName(taskNumber: TaskNumber): String =
    s"refs/gh-tasks-llm-executor/claims/task-${taskNumber.value}"
