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
import cats.data.Kleisli
import cats.syntax.all.*

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
      Git[IO](message => messages.update(_ :+ message)).push(root, branch) *> messages.get
        .map { seen =>
          assert(seen.exists(_.contains("rebasing onto origin/task-1")))
          val originHead =
            os.proc("git", "rev-parse", s"origin/${branch.value}").call(cwd = root).out.text().trim
          val localHead = os.proc("git", "rev-parse", "HEAD").call(cwd = root).out.text().trim
          assertEquals(localHead, originHead)
        }
    }

class ModifiedOrDeletedFilesSuite extends CatsEffectSuite:

  private def repo(): os.Path =
    val root = os.temp.dir(prefix = "git-changed-files")
    os.proc("git", "init", "-q", "-b", "master").call(cwd = root)
    os.proc("git", "config", "user.email", "test@example.com").call(cwd = root)
    os.proc("git", "config", "user.name", "Test").call(cwd = root)
    os.write(root / "Foo.scala", "object Foo\n")
    os.write(root / "Foo.test.scala", "class FooSuite\n")
    os.proc("git", "add", ".").call(cwd = root)
    os.proc("git", "commit", "-q", "-m", "seed").call(cwd = root)
    root

  private def changed(worktree: os.Path): IO[List[String]] =
    Git[IO](_ => IO.unit).modifiedOrDeletedFiles
      .run((worktree, BranchName("task-1"), Some(BranchName("master"))))

  private def branch(root: os.Path): os.Path =
    val worktree = os.temp.dir(prefix = "git-changed-worktree") / "task-1"
    os.proc("git", "worktree", "add", "-q", "-b", "task-1", worktree.toString).call(cwd = root)
    worktree

  test("an uncommitted edit to an existing file is reported"):
    val worktree = branch(repo())
    os.write.over(worktree / "Foo.test.scala", "class FooSuite // weakened\n")

    changed(worktree).map(files => assertEquals(files, List("Foo.test.scala")))

  test("a committed edit is reported too — an agent may commit its own work"):
    val root = repo()
    val worktree = branch(root)
    os.write.over(worktree / "Foo.test.scala", "class FooSuite // weakened\n")
    os.proc("git", "add", ".").call(cwd = worktree)
    os.proc("git", "commit", "-q", "-m", "edit").call(cwd = worktree)

    changed(worktree).map(files => assertEquals(files, List("Foo.test.scala")))

  test("a deleted test is reported — deleting the verifier passes it too"):
    val worktree = branch(repo())
    os.remove(worktree / "Foo.test.scala")

    changed(worktree).map(files => assertEquals(files, List("Foo.test.scala")))

  test("a newly added file is not reported"):
    // Adding a test strengthens the verifier; blocking it would stop an
    // implementer from covering the code it just wrote.
    val worktree = branch(repo())
    os.write(worktree / "Bar.test.scala", "class BarSuite\n")

    changed(worktree).map(files => assertEquals(files, Nil))

  test("an untracked file is not mistaken for a modification"):
    val worktree = branch(repo())
    os.write(worktree / "scratch.txt", "notes\n")

    changed(worktree).map(files => assertEquals(files, Nil))

  test("a clean worktree reports nothing"):
    changed(branch(repo())).map(files => assertEquals(files, Nil))

  test("production and test edits are reported together, sorted"):
    val worktree = branch(repo())
    os.write.over(worktree / "Foo.scala", "object Foo // changed\n")
    os.write.over(worktree / "Foo.test.scala", "class FooSuite // changed\n")

    changed(worktree).map(files => assertEquals(files, List("Foo.scala", "Foo.test.scala")))

class ModifiedOrDeletedInWorktreeSuite extends CatsEffectSuite:

  private def worktree(): os.Path =
    val root = os.temp.dir(prefix = "git-worktree-only")
    os.proc("git", "init", "-q", "-b", "master").call(cwd = root)
    os.proc("git", "config", "user.email", "test@example.com").call(cwd = root)
    os.proc("git", "config", "user.name", "Test").call(cwd = root)
    os.write(root / "Foo.test.scala", "class FooSuite\n")
    os.proc("git", "add", ".").call(cwd = root)
    os.proc("git", "commit", "-q", "-m", "seed").call(cwd = root)
    root

  private def changed: Kleisli[IO, os.Path, List[String]] =
  Git[IO](_ => IO.unit).modifiedOrDeletedInWorktree

  test("an uncommitted test edit is seen without any base ref"):
    // The repair loop validates before committing, so it has no branch to diff.
    val root = worktree()
    os.write.over(root / "Foo.test.scala", "class FooSuite // weakened\n")

    changed(root).map(files => assertEquals(files, List("Foo.test.scala")))

  test("a committed edit is invisible here — by design, this is the worktree only"):
    val root = worktree()
    os.write.over(root / "Foo.test.scala", "class FooSuite // weakened\n")
    os.proc("git", "add", ".").call(cwd = root)
    os.proc("git", "commit", "-q", "-m", "edit").call(cwd = root)

    changed(root).map(files => assertEquals(files, Nil))

  test("an untracked new test is not a modification"):
    val root = worktree()
    os.write(root / "Bar.test.scala", "class BarSuite\n")

    changed(root).map(files => assertEquals(files, Nil))

class ChangedFilesSuite extends CatsEffectSuite:

  private def repo(): os.Path =
    val root = os.temp.dir(prefix = "git-touched-files")
    os.proc("git", "init", "-q", "-b", "master").call(cwd = root)
    os.proc("git", "config", "user.email", "test@example.com").call(cwd = root)
    os.proc("git", "config", "user.name", "Test").call(cwd = root)
    os.write(root / "Foo.scala", "object Foo\n")
    os.proc("git", "add", ".").call(cwd = root)
    os.proc("git", "commit", "-q", "-m", "seed").call(cwd = root)
    root

  private def branch(root: os.Path): os.Path =
    val worktree = os.temp.dir(prefix = "git-touched-worktree") / "task-1"
    os.proc("git", "worktree", "add", "-q", "-b", "task-1", worktree.toString).call(cwd = root)
    worktree

  private def touched(worktree: os.Path): IO[List[String]] =
    Git[IO](_ => IO.unit).changedFiles
      .run((worktree, BranchName("task-1"), Some(BranchName("master"))))

  test("a clean worktree touched nothing"):
    touched(branch(repo())).map(files => assertEquals(files, Nil))

  test("additions count here, unlike modifiedOrDeletedFiles"):
    // This list is context for the next task, not a guard: a file the run
    // created is exactly what a dependent task needs to be pointed at.
    val worktree = branch(repo())
    os.write(worktree / "Bar.scala", "object Bar\n")

    touched(worktree).map(files => assertEquals(files, List("Bar.scala")))

  test("committed and uncommitted changes are merged, deduplicated and sorted"):
    val root = repo()
    val worktree = branch(root)
    os.write.over(worktree / "Foo.scala", "object Foo // one\n")
    os.proc("git", "add", ".").call(cwd = worktree)
    os.proc("git", "commit", "-q", "-m", "edit").call(cwd = worktree)
    os.write.over(worktree / "Foo.scala", "object Foo // two\n")
    os.write(worktree / "Bar.scala", "object Bar\n")

    touched(worktree).map(files => assertEquals(files, List("Bar.scala", "Foo.scala")))

  test("a rename reports the path that now exists"):
    val worktree = branch(repo())
    os.proc("git", "mv", "Foo.scala", "Renamed.scala").call(cwd = worktree)

    touched(worktree).map(files => assert(files.contains("Renamed.scala"), files.toString))

class CommitAllAgentScratchSuite extends CatsEffectSuite:
  private def repo(): os.Path =
    val root = os.temp.dir(prefix = "commit-all-scratch")
    os.proc("git", "init", "-q", "-b", "master").call(cwd = root)
    os.proc("git", "config", "user.email", "test@example.com").call(cwd = root)
    os.proc("git", "config", "user.name", "Test").call(cwd = root)
    os.write(root / "README.md", "seed\n")
    os.proc("git", "add", "README.md").call(cwd = root)
    os.proc("git", "commit", "-q", "-m", "seed").call(cwd = root)
    root

  private def trackedFiles(root: os.Path): Set[String] =
    os.proc("git", "ls-tree", "-r", "--name-only", "HEAD")
      .call(cwd = root)
      .out
      .lines()
      .toSet

  test("commitAll stages the agent's work but not the agent's scratch"):
    // `git add -A` published .aider.chat.history.md (the whole transcript) and a
    // 778 KB tags cache to a public master on 2026-08-01.
    val root = repo()
    os.write(root / "Feature.scala", "object Feature\n")
    os.write(root / ".aider.chat.history.md", "# aider chat started\n")
    os.write(root / ".aider.input.history", "prompts\n")
    os.write(root / ".aider.tags.cache.v4" / "cache.db", "binary", createFolders = true)
    os.write(root / ".aider.tags.cache.v4" / "7f" / "19" / "e87.val", "blob", createFolders = true)

    val task = Issue(TaskNumber(1), IssueTitle("Add feature"), IssueBody("body"), State("open"))

    Git[IO](_ => IO.unit)
      .commitAll(root, task)
      .map { _ =>
        val tracked = trackedFiles(root)
        assert(tracked.contains("Feature.scala"), tracked)
        assert(!tracked.exists(_.startsWith(".aider")), tracked)
      }

  test("an already-tracked scratch file is not re-staged by a later run"):
    // The repositories this runs against may already carry committed scratch
    // (this one did). Excluding at staging must not resurrect it as a change.
    val root = repo()
    os.write(root / ".aider.input.history", "old\n")
    os.proc("git", "add", "-f", ".aider.input.history").call(cwd = root)
    os.proc("git", "commit", "-q", "-m", "pre-existing scratch").call(cwd = root)

    os.write.over(root / ".aider.input.history", "churned by this run\n")
    os.write(root / "Feature.scala", "object Feature\n")

    val task = Issue(TaskNumber(2), IssueTitle("Add feature"), IssueBody("body"), State("open"))

    Git[IO](_ => IO.unit)
      .commitAll(root, task)
      .map { _ =>
        val changed = os.proc("git", "show", "--name-only", "--format=", "HEAD")
          .call(cwd = root)
          .out
          .lines()
          .toSet
        assertEquals(changed, Set("Feature.scala"))
      }
