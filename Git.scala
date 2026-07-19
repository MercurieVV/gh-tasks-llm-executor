import cats.data.Kleisli
import cats.effect.kernel.Sync
import cats.syntax.all.*

final class Git[F[_]](using F: Sync[F]):

  def acquireWorktree: Kleisli[
    F,
    (os.Path, os.Path, String, Option[String], String => F[Unit]),
    Unit
  ] =
    Kleisli.apply {
      case input @ (root, worktreePath, branchName, baseBranch, progress) =>
        F.blocking(os.exists(worktreePath)).flatMap {
          case true =>
            progress(
              s"Leftover worktree detected at $worktreePath. Cleaning up..."
            ) *> (releaseWorktree.local[
              (os.Path, os.Path, String, Option[String], String => F[Unit])
            ] {
              case (
                    root: os.Path,
                    worktreePath: os.Path,
                    branchName: String,
                    _: Option[String],
                    progress: (String => F[Unit])
                  ) =>
                (root, worktreePath, branchName, progress)
            } *> acquireWorktree).run(input)
          case false =>
            call(
              root,
              "git",
              "branch",
              "-D",
              branchName
            ).attempt.void *> branchExistsOnOrigin((root, branchName)).flatMap {
              case true =>
                progress(
                  s"Remote branch origin/$branchName found. Recreating worktree tracking remote..."
                ) *> call(
                  root,
                  "git",
                  "branch",
                  branchName,
                  s"origin/$branchName"
                ) *> call(
                  root,
                  "git",
                  "worktree",
                  "add",
                  worktreePath.toString,
                  branchName
                )
              case false =>
                baseBranch
                  .traverse_(ensureBranch(root, _, progress)) *> progress(
                  s"Creating worktree at $worktreePath on branch $branchName${baseBranch
                      .fold("")(base => s" (base: $base)")}"
                ) *> call(
                  root,
                  Seq(
                    "git",
                    "worktree",
                    "add",
                    "-b",
                    branchName,
                    worktreePath.toString
                  ) ++ baseBranch.toList*
                )
            }
        }
    }

  // Creates a shared integration branch used as the base for a family of
  // subtask branches, so those subtasks merge into it rather than directly
  // into the default branch (see taskRun's baseBranch computation). A
  // subtask branch always exists locally once created; an integration
  // branch may not, so create (and, if there's a remote, publish) it once,
  // on first use, from wherever HEAD currently is.
  def ensureBranch(
      root: os.Path,
      branchName: String,
      progress: String => F[Unit]
  ): F[Unit] =
    branchExistsLocally((root, branchName)).flatMap {
      case true => F.unit
      case false =>
        hasRemote(root).flatMap { remote =>
          if remote then
            branchExistsOnOrigin((root, branchName)).flatMap {
              case true =>
                progress(
                  s"Creating local integration branch $branchName tracking origin/$branchName..."
                ) *> call(
                  root,
                  "git",
                  "branch",
                  branchName,
                  s"origin/$branchName"
                )
              case false =>
                progress(s"Creating integration branch $branchName...") *>
                  call(root, "git", "branch", branchName) *>
                  call(root, "git", "push", "-u", "origin", branchName)
            }
          else
            progress(s"Creating integration branch $branchName...") *>
              call(root, "git", "branch", branchName)
        }
    }

  private def branchExistsLocally: Kleisli[F, (os.Path, String), Boolean] =
    Kleisli.apply { case (root, branchName) =>
      F.blocking {
        os.proc("git", "rev-parse", "--verify", branchName)
          .call(cwd = root, stdout = os.Pipe, stderr = os.Pipe, check = false)
          .exitCode === 0
      }
    }

  private def branchExistsOnOrigin: Kleisli[F, (os.Path, String), Boolean] =
    Kleisli.apply { case (root, branchName) =>
      F.blocking {
        os.proc("git", "rev-parse", "--verify", s"origin/$branchName")
          .call(cwd = root, stdout = os.Pipe, stderr = os.Pipe, check = false)
          .exitCode === 0
      }
    }

  def releaseWorktree
      : Kleisli[F, (os.Path, os.Path, String, String => F[Unit]), Unit] =
    Kleisli.apply { case (root, worktreePath, branchName, progress) =>
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
              ).attempt.flatMap {
                case Right(_) => F.unit
                case Left(_) =>
                  progress(
                    s"Standard git worktree remove failed. Force deleting directory $worktreePath..."
                  ) *>
                    F.blocking(os.remove.all(worktreePath))
              }
        }
        _ <- call(root, "git", "branch", "-D", branchName).attempt.void
      yield ()
    }

  // Commits that never made it to a normal push/PR (e.g. the push step
  // itself failed, such as a rejected pre-push hook) would otherwise be
  // destroyed when releaseWorktree force-removes the worktree/branch.
  // Call this from within the worktree resource's `.use` block on failure,
  // while the worktree still exists, to push them to a recovery ref first.
  def preserveUnpushedCommits
      : Kleisli[F, (os.Path, String, Option[String], String => F[Unit]), Unit] =
    Kleisli.apply { case (worktreePath, branchName, baseBranch, progress) =>
      hasPublishableCommits((worktreePath, branchName, baseBranch)).flatMap {
        case false =>
          F.unit
        case true =>
          val recoveryRef = s"refs/gh-tasks-llm-executor/failed/$branchName"
          progress(
            s"Branch $branchName has commits not on origin; preserving them at $recoveryRef before cleanup..."
          ) *> call(
            worktreePath,
            "git",
            "push",
            "-f",
            "origin",
            s"HEAD:$recoveryRef"
          ).attempt.flatMap {
            case Right(_) =>
              progress(s"Preserved unpushed commits at $recoveryRef.")
            case Left(error) =>
              progress(
                s"Warning: failed to preserve unpushed commits at $recoveryRef: ${error.getMessage}"
              )
          }
      }
    }

  def filesChanged: Kleisli[F, os.Path, Boolean] =
    Kleisli.apply { worktreePath =>
      F.blocking {
        os.proc("git", "status", "--porcelain")
          .call(cwd = worktreePath)
          .out
          .text()
          .trim
          .nonEmpty
      }
    }

  def hasPublishableCommits
      : Kleisli[F, (os.Path, String, Option[String]), Boolean] =
    Kleisli.apply { case (worktreePath, branchName, baseBranch) =>
      F.blocking {
        val remoteBranch = s"origin/$branchName"
        val hasRemoteBranch = os
          .proc("git", "rev-parse", "--verify", remoteBranch)
          .call(
            cwd = worktreePath,
            stdout = os.Pipe,
            stderr = os.Pipe,
            check = false
          )
          .exitCode === 0
        val baseRef =
          if (hasRemoteBranch) remoteBranch
          else
            baseBranch.getOrElse {
              val hasMaster = os
                .proc("git", "rev-parse", "--verify", "master")
                .call(
                  cwd = worktreePath,
                  stdout = os.Pipe,
                  stderr = os.Pipe,
                  check = false
                )
                .exitCode === 0
              if (hasMaster) "master" else "main"
            }
        val result = os
          .proc("git", "rev-list", "--count", s"$baseRef..HEAD")
          .call(
            cwd = worktreePath,
            stdout = os.Pipe,
            stderr = os.Pipe,
            check = false
          )
        result.exitCode === 0 && result.out
          .text()
          .trim
          .toIntOption
          .exists(_ > 0)
      }
    }

  def runProjectValidation: Kleisli[F, (os.Path, String => F[Unit]), Unit] =
    Kleisli.apply { case (worktreePath, progress) =>
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
    }

  private def validationHook: Kleisli[F, os.Path, Option[os.Path]] =
    Kleisli.apply { worktreePath =>
      F.blocking {
        List(
          worktreePath / ".gh-tasks-llm-executor" / "validate",
          worktreePath / ".github" / "gh-tasks-llm-executor" / "validate",
          worktreePath / "scripts" / "gh-task-validate"
        ).find(path => os.exists(path) && os.isFile(path))
      }
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

  def hasRemote: Kleisli[F, os.Path, Boolean] =
    Kleisli.apply { root =>
      F.blocking(
        os.proc("git", "remote").call(cwd = root).out.text().trim.nonEmpty
      )
    }

  def mergeLocally: Kleisli[
    F,
    (os.Path, os.Path, String, Option[String], String => F[Unit]),
    Unit
  ] =
    Kleisli.apply {
      case (root, worktreePath, branchName, baseBranch, progress) =>
        for
          currentBranch <- F.blocking(
            os.proc("git", "branch", "--show-current")
              .call(cwd = root)
              .out
              .text()
              .trim
          )
          targetBranch = baseBranch.getOrElse(currentBranch)
          switchesBranch = targetBranch =!= currentBranch
          _ <- progress(s"Removing worktree at $worktreePath")
          _ <- call(
            root,
            "git",
            "worktree",
            "remove",
            "--force",
            worktreePath.toString
          )
          _ <- baseBranch.traverse_(ensureBranch(root, _, progress))
          _ <-
            if switchesBranch then call(root, "git", "checkout", targetBranch)
            else F.unit
          _ <- progress(s"Merging branch $branchName into $targetBranch...")
          _ <- call(root, "git", "merge", branchName)
          _ <- progress(s"Deleting local branch $branchName...")
          _ <- call(root, "git", "branch", "-d", branchName)
          _ <-
            if switchesBranch then call(root, "git", "checkout", currentBranch)
            else F.unit
        yield ()
    }

  def cleanupWorktree
      : Kleisli[F, (os.Path, os.Path, String, String => F[Unit]), Unit] =
    Kleisli.apply { case (root, worktreePath, branchName, progress) =>
      F.blocking(os.exists(worktreePath)).flatMap {
        case false =>
          F.unit
        case true =>
          progress(s"Removing worktree at $worktreePath") *> call(
            root,
            "git",
            "worktree",
            "remove",
            "--force",
            worktreePath.toString
          ) *> call(root, "git", "branch", "-D", branchName).attempt.void
      }
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
