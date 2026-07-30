package com.github.mercurievv.ghllm.git

import com.github.mercurievv.ghllm.*
import com.github.mercurievv.ghllm.agent.*
import com.github.mercurievv.ghllm.arrow.*
import com.github.mercurievv.ghllm.cli.*
import com.github.mercurievv.ghllm.git.*
import com.github.mercurievv.ghllm.metrics.*

import cats.effect.IO
import cats.effect.Ref
import munit.CatsEffectSuite

class GitSuite extends CatsEffectSuite:

  private def repoWithOrigin(): (os.Path, os.Path) =
    val root = os.temp.dir(prefix = "git-push-test")
    val origin = os.temp.dir(prefix = "git-push-origin")
    os.proc("git", "init", "-q", "--bare").call(cwd = origin)
    os.proc("git", "init", "-q", "-b", "master").call(cwd = root)
    configureUser(root)
    os.proc("git", "remote", "add", "origin", origin.toString).call(cwd = root)
    os.write(root / "README.md", "seed\n")
    os.proc("git", "add", "README.md").call(cwd = root)
    os.proc("git", "commit", "-q", "-m", "seed").call(cwd = root)
    os.proc("git", "push", "-u", "origin", "master").call(cwd = root)
    (root, origin)

  private def configureUser(root: os.Path): Unit =
    os.proc("git", "config", "user.email", "test@example.com").call(cwd = root)
    os.proc("git", "config", "user.name", "Test").call(cwd = root)

  private def commitFile(root: os.Path, file: String, content: String, message: String): Unit =
    os.write.over(root / file, content)
    os.proc("git", "add", file).call(cwd = root)
    os.proc("git", "commit", "-q", "-m", message).call(cwd = root)

  test("resetWorktree discards uncommitted and untracked work in a linked worktree"):
    val (root, _) = repoWithOrigin()
    val worktree = os.temp.dir(prefix = "git-reset-worktree") / "task-1"
    os.proc("git", "worktree", "add", "-q", "-b", "task-1", worktree.toString).call(cwd = root)
    os.write.over(worktree / "README.md", "modified\n")
    os.write(worktree / "junk.txt", "left behind by a failed attempt\n")

    Ref[IO].of(List.empty[String]).flatMap { messages =>
      Git[IO](message => messages.update(_ :+ message))
        .resetWorktree(worktree)
        .map { _ =>
          val status = os.proc("git", "status", "--porcelain").call(cwd = worktree).out.text().trim
          assertEquals(status, "")
          assertEquals(os.read(worktree / "README.md"), "seed\n")
          assert(!os.exists(worktree / "junk.txt"))
        }
    }

  test("resetWorktree refuses to touch a primary checkout"):
    val (root, _) = repoWithOrigin()
    os.write.over(root / "README.md", "uncommitted work the user cares about\n")

    Ref[IO].of(List.empty[String]).flatMap { messages =>
      Git[IO](message => messages.update(_ :+ message))
        .resetWorktree(root)
        .attempt
        .map { result =>
          assert(result.isLeft)
          assert(result.left.toOption.exists(_.getMessage.contains("not a linked git worktree")))
          // The guard must fail before doing anything, not after.
          assertEquals(os.read(root / "README.md"), "uncommitted work the user cares about\n")
        }
    }

  test("pre-push hook failures are not mistaken for remote-moved branches"):
    val (root, _) = repoWithOrigin()
    val branch = BranchName("task-1")
    os.proc("git", "checkout", "-q", "-b", branch.value).call(cwd = root)
    commitFile(root, "task.txt", "task\n", "task")
    val hook = root / ".git" / "hooks" / "pre-push"
    os.write(
      hook,
      """#!/bin/sh
        |echo "pre-push rejected"
        |exit 1
        |""".stripMargin
    )
    os.proc("chmod", "+x", hook.toString).call(cwd = root)

    Ref[IO].of(List.empty[String]).flatMap { messages =>
      Git[IO](message => messages.update(_ :+ message))
        .push(root, branch)
        .attempt
        .flatMap { result =>
          messages.get.map { seen =>
            assert(result.isLeft)
            assert(!seen.exists(_.contains("rebasing onto origin/task-1")))
            assert(!seen.exists(_.contains("Remote branch origin/task-1 no longer exists")))
          }
        }
    }

  test("non-fast-forward push rebases onto an existing remote branch and retries"):
    val (root, origin) = repoWithOrigin()
    val branch = BranchName("task-1")
    os.proc("git", "checkout", "-q", "-b", branch.value).call(cwd = root)
    commitFile(root, "local.txt", "local 1\n", "local 1")
    os.proc("git", "push", "-u", "origin", branch.value).call(cwd = root)

    val clone = os.temp.dir(prefix = "git-push-clone")
    os.proc("git", "clone", "-q", origin.toString, clone.toString).call()
    configureUser(clone)
    os.proc("git", "checkout", "-q", branch.value).call(cwd = clone)
    commitFile(clone, "remote.txt", "remote\n", "remote")
    os.proc("git", "push", "origin", branch.value).call(cwd = clone)

    commitFile(root, "local.txt", "local 2\n", "local 2")

    Ref[IO].of(List.empty[String]).flatMap { messages =>
      Git[IO](message => messages.update(_ :+ message))
        .push(root, branch)
        .flatMap(_ => messages.get)
        .map { seen =>
          assert(seen.exists(_.contains("rebasing onto origin/task-1")))
          val originHead =
            os.proc("git", "rev-parse", s"origin/${branch.value}").call(cwd = root).out.text().trim
          val localHead = os.proc("git", "rev-parse", "HEAD").call(cwd = root).out.text().trim
          assertEquals(localHead, originHead)
        }
    }
