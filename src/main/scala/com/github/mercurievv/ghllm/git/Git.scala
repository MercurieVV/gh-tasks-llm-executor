package com.github.mercurievv.ghllm.git

import com.github.mercurievv.ghllm.*
import com.github.mercurievv.ghllm.agent.*
import com.github.mercurievv.ghllm.arrow.*
import com.github.mercurievv.ghllm.cli.*
import com.github.mercurievv.ghllm.git.*
import com.github.mercurievv.ghllm.metrics.*

import cats.data.Kleisli
import cats.effect.kernel.Sync
import cats.syntax.all.*

final class Git[F[_]](using F: Sync[F])(progress: String => F[Unit]):

  def acquireWorktree: Kleisli[
    F,
    (os.Path, os.Path, BranchName, Option[BranchName]),
    Unit
  ] =
    Kleisli.apply { case input @ (root, worktreePath, branchName, baseBranch) =>
      F.blocking(os.exists(worktreePath)).flatMap {
        case true =>
          progress(
            s"Leftover worktree detected at $worktreePath. Cleaning up..."
          ) *> (releaseWorktree.local[
            (os.Path, os.Path, BranchName, Option[BranchName])
          ] {
            case (
                  root: os.Path,
                  worktreePath: os.Path,
                  branchName: BranchName,
                  _: Option[BranchName]
                ) =>
              (root, worktreePath, branchName)
          } *> acquireWorktree).run(input)
        case false =>
          val fetch =
            if Cli.fetchOriginEnabled then
              hasRemote(root).flatMap {
                case true =>
                  progress("Fetching all changes from origin...") *>
                    call(root, "git", "fetch", "--prune", "origin").attempt.void
                case false => F.unit
              }
            else F.unit
          fetch *> call(
            root,
            "git",
            "branch",
            "-D",
            branchName.value
          ).attempt.void *> branchExistsOnOrigin((root, branchName)).flatMap {
            case true =>
              progress(
                s"Remote branch origin/$branchName found. Recreating worktree tracking remote..."
              ) *> call(
                root,
                "git",
                "branch",
                branchName.value,
                s"origin/$branchName"
              ) *> call(
                root,
                "git",
                "worktree",
                "add",
                worktreePath.toString,
                branchName.value
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
                  branchName.value,
                  worktreePath.toString
                ) ++ baseBranch.toList.map(_.value)*
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
      branchName: BranchName,
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
                  branchName.value,
                  s"origin/$branchName"
                )
              case false =>
                progress(s"Creating integration branch $branchName...") *>
                  call(root, "git", "branch", branchName.value) *>
                  call(root, "git", "push", "-u", "origin", branchName.value)
            }
          else
            progress(s"Creating integration branch $branchName...") *>
              call(root, "git", "branch", branchName.value)
        }
    }

  private def branchExistsLocally: Kleisli[F, (os.Path, BranchName), Boolean] =
    Kleisli.apply { case (root, branchName) =>
      F.blocking {
        os.proc("git", "rev-parse", "--verify", branchName.value)
          .call(cwd = root, stdout = os.Pipe, stderr = os.Pipe, check = false)
          .exitCode === 0
      }
    }

  // Public reachability check: does the pushed branch still exist on origin?
  // acquireWorktree recreates a worktree from origin/<branch> (and `git
  // branch -D`s any local-only branch), so an origin branch is the durable
  // proof that already-implemented work survives into the next invocation.
  def hasOriginBranch: Kleisli[F, (os.Path, BranchName), Boolean] =
    branchExistsOnOrigin

  private def branchExistsOnOrigin: Kleisli[F, (os.Path, BranchName), Boolean] =
    Kleisli.apply { case (root, branchName) =>
      F.blocking {
        os.proc("git", "rev-parse", "--verify", s"origin/$branchName")
          .call(cwd = root, stdout = os.Pipe, stderr = os.Pipe, check = false)
          .exitCode === 0
      }
    }

  def releaseWorktree: Kleisli[F, (os.Path, os.Path, BranchName), Unit] =
    Kleisli.apply { case (root, worktreePath, branchName) =>
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
        _ <- call(root, "git", "branch", "-D", branchName.value).attempt.void
      yield ()
    }

  // Commits that never made it to a normal push/PR (e.g. the push step
  // itself failed, such as a rejected pre-push hook, or commitAll's --no-verify
  // recovery commit after a rejected pre-commit hook) would otherwise be
  // destroyed when releaseWorktree force-removes the worktree/branch.
  // Call this from within the worktree resource's `.use` block on failure,
  // while the worktree still exists, to preserve them at a recovery ref first.
  // The local ref is created unconditionally (branch -D in releaseWorktree
  // only deletes the branch ref, not refs/gh-tasks-llm-executor/*, and worktree
  // refs live in the shared .git dir so they survive worktree removal); the
  // ref is additionally pushed to origin, when a remote exists, as an offsite
  // backup.
  def preserveUnpushedCommits: Kleisli[F, (os.Path, BranchName, Option[BranchName]), Unit] =
    Kleisli.apply { case (worktreePath, branchName, baseBranch) =>
      hasPublishableCommits((worktreePath, branchName, baseBranch)).flatMap {
        case false =>
          F.unit
        case true =>
          val recoveryRef = s"refs/gh-tasks-llm-executor/failed/$branchName"
          progress(
            s"Branch $branchName has commits not on origin; preserving them at $recoveryRef before cleanup..."
          ) *> call(worktreePath, "git", "update-ref", recoveryRef, "HEAD").attempt
            .flatMap {
              case Right(_) =>
                progress(s"Preserved unpushed commits locally at $recoveryRef.")
              case Left(error) =>
                progress(
                  s"Warning: failed to create local recovery ref $recoveryRef: ${error.getMessage}"
                )
            } *> hasRemote(worktreePath).flatMap {
            case false => F.unit
            case true =>
              call(
                worktreePath,
                "git",
                "push",
                "-f",
                "origin",
                s"HEAD:$recoveryRef"
              ).attempt.flatMap {
                case Right(_) =>
                  progress(s"Pushed recovery ref $recoveryRef to origin.")
                case Left(error) =>
                  progress(
                    s"Warning: failed to push recovery ref $recoveryRef to origin: ${error.getMessage}"
                  )
              }
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

  // Resolves what "before this task" means: the pushed branch if there is one,
  // else the task's base, else whichever default branch this repo uses.
  private def baseRefOf(
      worktreePath: os.Path,
      branchName: BranchName,
      baseBranch: Option[BranchName]
  ): BranchName =
    val remoteBranch = BranchName(s"origin/${branchName.value}")
    val hasRemoteBranch = os
      .proc("git", "rev-parse", "--verify", remoteBranch.value)
      .call(cwd = worktreePath, stdout = os.Pipe, stderr = os.Pipe, check = false)
      .exitCode === 0
    if hasRemoteBranch then remoteBranch
    else
      baseBranch.getOrElse(BranchName({
        val hasMaster = os
          .proc("git", "rev-parse", "--verify", "master")
          .call(cwd = worktreePath, stdout = os.Pipe, stderr = os.Pipe, check = false)
          .exitCode === 0
        if hasMaster then "master" else "main"
      }))

  /** Paths this run modified, deleted or renamed — committed or not.
    *
    * Additions are deliberately excluded: the caller ([[TestEditGuard]]) blocks
    * changes that could weaken an existing test, and a new file weakens nothing.
    */
  def modifiedOrDeletedFiles: Kleisli[F, (os.Path, BranchName, Option[BranchName]), List[String]] =
    Kleisli.apply { case (worktreePath, branchName, baseBranch) =>
      F.blocking {
        val baseRef = baseRefOf(worktreePath, branchName, baseBranch)
        val committed = os
          .proc("git", "diff", "--name-only", "--diff-filter=MDR", s"${baseRef.value}...HEAD")
          .call(cwd = worktreePath, stdout = os.Pipe, stderr = os.Pipe, check = false)
        val committedFiles =
          if committed.exitCode === 0 then nonEmptyLines(committed.out.text()) else Nil
        (committedFiles ++ uncommittedModifiedOrDeleted(worktreePath)).distinct.sorted
      }
    }

  /** Every path this run touched, additions included — what a dependent task
    * needs in order to start from file names instead of searching for them
    * (TOKEN_EFFICIENCY_PLAN.md §2 Stage 2, "never 'go find it'").
    *
    * Read from git rather than from the runner's prose: the set is already
    * known exactly, so asking a model to restate it spends tokens to obtain a
    * worse answer.
    */
  def changedFiles: Kleisli[F, (os.Path, BranchName, Option[BranchName]), List[String]] =
    Kleisli.apply { case (worktreePath, branchName, baseBranch) =>
      F.blocking {
        val baseRef = baseRefOf(worktreePath, branchName, baseBranch)
        val committed = os
          .proc("git", "diff", "--name-only", s"${baseRef.value}...HEAD")
          .call(cwd = worktreePath, stdout = os.Pipe, stderr = os.Pipe, check = false)
        val committedFiles =
          if committed.exitCode === 0 then nonEmptyLines(committed.out.text()) else Nil
        val pending = os
          .proc("git", "status", "--porcelain")
          .call(cwd = worktreePath, stdout = os.Pipe, stderr = os.Pipe, check = false)
        val pendingFiles =
          if pending.exitCode =!= 0 then Nil
          else
            nonEmptyLines(pending.out.text(), trim = false).flatMap { line =>
              val path = line.drop(3).trim
              // "old -> new": the new path is the one that now exists.
              val subject = path.split(" -> ").lastOption.map(_.trim).getOrElse(path)
              Option.when(subject.nonEmpty)(subject)
            }
        (committedFiles ++ pendingFiles).distinct.sorted
      }
    }

  /** The uncommitted half of [[modifiedOrDeletedFiles]], for callers that judge
    * a worktree before anything is committed and so have no base ref to diff
    * against — the repair loop validates and only then commits.
    */
  def modifiedOrDeletedInWorktree: Kleisli[F, os.Path, List[String]] =
    Kleisli.apply(worktreePath => F.blocking(uncommittedModifiedOrDeleted(worktreePath).distinct.sorted))

  private def uncommittedModifiedOrDeleted(worktreePath: os.Path): List[String] =
    // Porcelain's two status columns cover index and worktree; an untracked
    // file reads "??" and must not count, so match the letters explicitly.
    val pending = os
      .proc("git", "status", "--porcelain")
      .call(cwd = worktreePath, stdout = os.Pipe, stderr = os.Pipe, check = false)
    if pending.exitCode =!= 0 then Nil
    else
      nonEmptyLines(pending.out.text(), trim = false).flatMap { line =>
        val status = line.take(2)
        val path = line.drop(3).trim
        // A rename prints "old -> new"; the old path is the one at risk.
        val subject = path.split(" -> ").headOption.map(_.trim).getOrElse(path)
        if status.exists(c => c === 'M' || c === 'D' || c === 'R') && subject.nonEmpty then Some(subject)
        else None
      }

  private def nonEmptyLines(text: String, trim: Boolean = true): List[String] =
    text.linesIterator.map(line => if trim then line.trim else line).filter(_.trim.nonEmpty).toList

  def hasPublishableCommits: Kleisli[F, (os.Path, BranchName, Option[BranchName]), Boolean] =
    Kleisli.apply { case (worktreePath, branchName, baseBranch) =>
      F.blocking {
        val baseRef = baseRefOf(worktreePath, branchName, baseBranch)
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

  // Runs a repository-owned validation script after the agent changes the worktree
  // and before the executor commits/publishes/closes the task. The executor cannot
  // know each project's correct build, lint, format, test, or policy checks, so the
  // project supplies one executable hook that acts as the local quality gate.
  def runProjectValidation: Kleisli[F, (os.Path), Unit] =
    Kleisli.apply { case (worktreePath) =>
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
              if result.exitCode === 0 then progress("Project validation hook passed.")
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
    val message = commitTitle
      .filter(_.trim.nonEmpty)
      .getOrElse(s"Implement task #${task.number}: ${task.title}")
    call(worktreePath, "git", "add", "-A") *>
      call(worktreePath, "git", "commit", "-m", message).handleErrorWith { original =>
        // A rejecting pre-commit hook (e.g. failing lint/format check) leaves
        // the work staged but uncommitted. releaseWorktree force-removes the
        // worktree unconditionally, which would silently discard it. Snapshot
        // it with --no-verify so it becomes a real commit; preserveUnpushedCommits
        // (called on error before the worktree is released) then pushes it to
        // a recovery ref instead of losing it. onError would still rethrow
        // `original` after running this handler, defeating the point (the
        // caller would still crash/treat the task as failed even though the
        // work is now safely committed) — so recover instead, and only
        // rethrow if the recovery commit itself also fails.
        call(
          worktreePath,
          "git",
          "commit",
          "--no-verify",
          "-m",
          s"WIP (recovery, pre-commit hook failed): $message"
        ).attempt.flatMap {
          case Right(_) => F.unit
          case Left(_)  => F.raiseError(original)
        }
      }

  // Runs the repo's prePush hook (tests/lint/format); raises on rejection.
  // A rejection is either mechanical (remote moved — fixed here for free with
  // a rebase+retry, no agent needed) or a real hook failure (failing
  // test/lint — needs code judgment). Only the latter should ever reach a
  // repair agent, so the mechanical case is handled inline; repair/retry for
  // genuine failures stays an orchestration concern — see main.scala's
  // publishRemote.
  def push: Kleisli[F, (os.Path, BranchName), Unit] =
    Kleisli.apply { case (worktreePath, branchName) =>
      pushWithRebaseRetry(worktreePath, branchName, MaxPushRebaseRetries)
    }

  private val MaxPushRebaseRetries = 2

  private def pushWithRebaseRetry(
      worktreePath: os.Path,
      branchName: BranchName,
      retriesLeft: Int
  ): F[Unit] =
    call(worktreePath, "git", "push", "-u", "origin", branchName.value).handleErrorWith { error =>
      if retriesLeft > 0 && isNonFastForwardRejection(error) then
        progress(
          s"Push rejected (remote moved); rebasing onto origin/$branchName and retrying ($retriesLeft left)..."
        ) *> call(worktreePath, "git", "pull", "--rebase", "origin", branchName.value)
          .handleErrorWith { pullError =>
            if isMissingRemoteRef(pullError, branchName) then
              progress(
                s"Remote branch origin/$branchName no longer exists; retrying push without rebase..."
              )
            else F.raiseError(pullError)
          } *> pushWithRebaseRetry(worktreePath, branchName, retriesLeft - 1)
      else F.raiseError(error)
    }

  private def isNonFastForwardRejection(error: Throwable): Boolean =
    val message = Option(error.getMessage).getOrElse("")
    message.contains("non-fast-forward") ||
    message.contains("fetch first") ||
    message.contains("[rejected]") && message.contains("stale info")

  private def isMissingRemoteRef(error: Throwable, branchName: BranchName): Boolean =
    val message = Option(error.getMessage).getOrElse("")
    message.contains(s"couldn't find remote ref ${branchName.value}")

  def hasRemote: Kleisli[F, os.Path, Boolean] =
    Kleisli.apply { root =>
      F.blocking(
        os.proc("git", "remote").call(cwd = root).out.text().trim.nonEmpty
      )
    }

  def mergeLocally: Kleisli[
    F,
    (os.Path, os.Path, BranchName, Option[BranchName]),
    Unit
  ] =
    Kleisli.apply { case (root, worktreePath, branchName, baseBranch) =>
      for
        currentBranch <- F.blocking(
          os.proc("git", "branch", "--show-current")
            .call(cwd = root)
            .out
            .text()
            .trim
        )
        targetBranch = baseBranch.getOrElse(BranchName(currentBranch))
        switchesBranch = targetBranch =!= BranchName(currentBranch)
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
          if switchesBranch then call(root, "git", "checkout", targetBranch.value)
          else F.unit
        _ <- progress(s"Merging branch $branchName into $targetBranch...")
        _ <- call(root, "git", "merge", branchName.value)
        _ <- progress(s"Deleting local branch $branchName...")
        _ <- call(root, "git", "branch", "-d", branchName.value)
        _ <-
          if switchesBranch then call(root, "git", "checkout", currentBranch)
          else F.unit
      yield ()
    }

  // Attempts to fold the PR's base branch into the current worktree branch to
  // resolve a GitHub-reported merge conflict. A clean, non-conflicting merge
  // commits itself; a conflicting one is left mid-merge (conflict markers in
  // place) for the caller to hand off to a repair agent — see main.scala's
  // resolveMergeConflict.
  def mergeBaseBranch: Kleisli[F, (os.Path, String), Boolean] =
    Kleisli.apply { case (worktreePath, baseBranch) =>
      call(worktreePath, "git", "fetch", "origin", baseBranch) *>
        F.blocking(
          os
            .proc("git", "merge", s"origin/$baseBranch", "--no-edit")
            .call(cwd = worktreePath, stdout = os.Pipe, stderr = os.Pipe, check = false)
            .exitCode === 0
        )
    }

  def hasUnresolvedConflicts: Kleisli[F, os.Path, Boolean] =
    unresolvedConflictFiles.map(_.nonEmpty)

  def unresolvedConflictFiles: Kleisli[F, os.Path, List[String]] =
    Kleisli.apply { worktreePath =>
      F.blocking(
        os
          .proc("git", "diff", "--name-only", "--diff-filter=U")
          .call(cwd = worktreePath, stdout = os.Pipe, stderr = os.Pipe, check = false)
          .out
          .text()
          .trim
          .linesIterator
          .map(_.trim)
          .filter(_.nonEmpty)
          .toList
      )
    }

  def abortMerge: Kleisli[F, os.Path, Unit] =
    Kleisli.apply { worktreePath =>
      call(worktreePath, "git", "merge", "--abort").attempt.void
    }

  // Escalation hygiene: a failed cheap attempt leaves partial edits behind, and
  // without discarding them the stronger runner inherits garbage and pays to
  // untangle it (TOKEN_EFFICIENCY_PLAN.md §2 Stage 1, risk 2). Committed work is
  // kept — this resets to HEAD, it does not rewind history.
  def resetWorktree: Kleisli[F, os.Path, Unit] =
    Kleisli.apply { worktreePath =>
      requireLinkedWorktree(worktreePath) *>
        progress(s"Discarding uncommitted changes in worktree $worktreePath") *>
        call(worktreePath, "git", "reset", "--hard") *>
        call(worktreePath, "git", "clean", "-fd")
    }

  // A guard, not politeness: `reset --hard` plus `clean -fd` aimed at the user's
  // primary checkout would destroy their uncommitted work. A linked worktree has
  // `.git` as a FILE (a `gitdir:` pointer); a primary checkout has it as a
  // DIRECTORY, so that is the discriminator, and anything else fails loudly.
  private def requireLinkedWorktree(worktreePath: os.Path): F[Unit] =
    F.blocking(os.isFile(worktreePath / ".git")).flatMap {
      case true => F.unit
      case false =>
        F.raiseError(
          RuntimeException(
            s"Refusing to reset $worktreePath: not a linked git worktree (no `.git` pointer file)."
          )
        )
    }

  def cleanupWorktree: Kleisli[F, (os.Path, os.Path, BranchName), Unit] =
    Kleisli.apply { case (root, worktreePath, branchName) =>
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
          ) *> call(root, "git", "branch", "-D", branchName.value).attempt.void
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
            if stdout.nonEmpty then TaskLogger.trace[F](s"command stdout ${truncate(stdout)}")
            else F.unit
          _ <-
            if stderr.nonEmpty then TaskLogger.trace[F](s"command stderr ${truncate(stderr)}")
            else F.unit
          _ <-
            if result.exitCode === 0 then F.unit
            else
              // Prefer stderr, but a failed prePush hook (tests/lint/format)
              // writes its summary to stdout, so fall back to whichever
              // stream is non-empty rather than assuming stderr always has
              // the useful part.
              val detail = Seq(stderr, stdout).find(_.nonEmpty)
              F.raiseError(
                new RuntimeException(
                  s"Command failed with exit code ${result.exitCode}: ${formatCommand(command)}" +
                    detail.fold("")(d => s"\n${truncateForError(d)}")
                )
              )
        yield ()
      }

  private def formatCommand(command: Seq[String]): String =
    command.map(value => s""""${truncate(value)}"""").mkString(" ")

  private def truncate(value: String): String =
    if value.length <= 160 then value else value.take(160) + "...[truncated]"

  // Failure detail fed to a repair agent (see main.scala's repairPrompt), not
  // the trace log: a blocked `git push` carries mill's full compile/test
  // output, with the actual failing-test summary near the end, not the
  // start, so this keeps the tail instead of truncate's head-only 160 chars.
  private def truncateForError(value: String): String =
    val limit = 6000
    if value.length <= limit then value
    else s"...[truncated ${value.length - limit} earlier chars]...\n" + value.takeRight(limit)

object Git:
  def apply[F[_]: Sync](progress: String => F[Unit]): Git[F] = new Git[F](progress)
