package com.github.mercurievv.ghllm.git

import com.github.mercurievv.ghllm.*
import com.github.mercurievv.ghllm.agent.*
import com.github.mercurievv.ghllm.arrow.*
import com.github.mercurievv.ghllm.cli.*
import com.github.mercurievv.ghllm.git.*
import com.github.mercurievv.ghllm.metrics.*

import cats.effect.IO
import munit.CatsEffectSuite

class GitHubMergeIntegrationBranchSuite extends munit.FunSuite:

  private def gitRepo(): os.Path =
    val root = os.temp.dir(prefix = "github-merge-integration-test")
    os.proc("git", "init", "-q", "-b", "master").call(cwd = root)
    os.proc("git", "config", "user.email", "test@example.com").call(cwd = root)
    os.proc("git", "config", "user.name", "Test").call(cwd = root)
    os.write(root / "README.md", "seed")
    os.proc("git", "add", "README.md").call(cwd = root)
    os.proc("git", "commit", "-q", "-m", "seed").call(cwd = root)
    root

  test("commitsAhead is zero for a branch with no new commits over the default branch"):
    val root = gitRepo()
    os.proc("git", "branch", "task-4").call(cwd = root)
    assertEquals(GitHub.commitsAhead(root, BranchName("task-4")), 0)

  test("commitsAhead counts commits made only on the integration branch"):
    val root = gitRepo()
    os.proc("git", "checkout", "-q", "-b", "task-4").call(cwd = root)
    os.write(root / "change.txt", "subtask work")
    os.proc("git", "add", "change.txt").call(cwd = root)
    os.proc("git", "commit", "-q", "-m", "subtask change").call(cwd = root)
    assertEquals(GitHub.commitsAhead(root, BranchName("task-4")), 1)

  test("the integration ref follows origin, which is where subtask PRs are merged"):
    // The exact shape that closed parent #39 over 17 stranded commits: the
    // subtask pull request merged on GitHub, so origin/task-4 moved and the
    // local task-4 stayed at the branch point.
    val origin = gitRepo()
    val root = os.temp.dir(prefix = "github-merge-integration-clone")
    os.proc("git", "clone", "-q", origin.toString, root.toString).call(cwd = root / os.up)
    os.proc("git", "config", "user.email", "test@example.com").call(cwd = root)
    os.proc("git", "config", "user.name", "Test").call(cwd = root)
    os.proc("git", "branch", "task-4").call(cwd = root)

    os.proc("git", "checkout", "-q", "-b", "task-4").call(cwd = origin)
    os.write(origin / "merged-by-github.txt", "subtask work")
    os.proc("git", "add", "merged-by-github.txt").call(cwd = origin)
    os.proc("git", "commit", "-q", "-m", "subtask merged on GitHub").call(cwd = origin)
    os.proc("git", "checkout", "-q", "master").call(cwd = origin)

    val ref = GitHub.integrationRef(root, BranchName("task-4"))
    assertEquals(ref, Some("origin/task-4"))
    assertEquals(GitHub.commitsAhead(root, BranchName("task-4")), 0)
    assert(ref.map(GitHub.commitsAheadOfRef(root, _)).contains(1))

  test("a branch that exists only locally is still found"):
    val root = gitRepo()
    os.proc("git", "branch", "task-4").call(cwd = root)
    assertEquals(GitHub.integrationRef(root, BranchName("task-4")), Some("task-4"))

  test("an absent integration branch is absent on both sides"):
    assertEquals(GitHub.integrationRef(gitRepo(), BranchName("task-4")), None)

  test("in-progress cleanup only removes task status labels"):
    val current = List("status: in progress", "in progress", "agent: aider", "model: haiku")
    assertEquals(
      GitHub.labelsToRemove(GitHub.InProgressStatusLabels, current),
      List("status: in progress", "in progress")
    )

class SettledParentsSuite extends munit.FunSuite:
  private def issue(number: Int, state: String, body: String = "") =
    Issue(TaskNumber(number), IssueTitle(s"task $number"), IssueBody(body), State(state))

  test("an open parent whose children are all closed is swept"):
    val issues = List(issue(10, "OPEN"), issue(11, "CLOSED", "Parent: #10"), issue(12, "CLOSED", "Parent: #10"))
    assertEquals(GitHub.settledParents(issues).map(_.number.value), List(10))

  test("a parent with an open child is left to checkParentsForCompletion"):
    val issues = List(issue(10, "OPEN"), issue(11, "CLOSED", "Parent: #10"), issue(12, "OPEN", "Parent: #10"))
    assertEquals(GitHub.settledParents(issues), Nil)

  test("a leaf is never swept — its task-N branch is ahead while its own PR is in flight"):
    // Sweeping leaves would land unreviewed work straight on the default branch.
    assertEquals(GitHub.settledParents(List(issue(10, "OPEN"), issue(11, "OPEN"))), Nil)

  test("a closed parent is not reopened by the sweep"):
    val issues = List(issue(10, "CLOSED"), issue(11, "CLOSED", "Parent: #10"))
    assertEquals(GitHub.settledParents(issues), Nil)

class SyncIntegrationBranchSuite extends CatsEffectSuite:
  private def git(cwd: os.Path, args: String*): os.CommandResult =
    os.proc("git" +: args).call(cwd = cwd, stdout = os.Pipe, stderr = os.Pipe, check = false)

  /** An origin whose task-4 branched before master moved on. `conflicting` decides whether master's move touches the
    * same file the branch changed.
    */
  private def diverged(conflicting: Boolean): os.Path =
    val origin = os.temp.dir(prefix = "integration-origin")
    git(origin, "init", "-q", "-b", "master")
    git(origin, "config", "user.email", "test@example.com")
    git(origin, "config", "user.name", "Test")
    os.write(origin / "shared.txt", "seed\n")
    git(origin, "add", ".")
    git(origin, "commit", "-q", "-m", "seed")

    git(origin, "checkout", "-q", "-b", "task-4")
    os.write.over(origin / "shared.txt", "branch work\n")
    git(origin, "add", ".")
    git(origin, "commit", "-q", "-m", "subtask work")

    git(origin, "checkout", "-q", "master")
    if conflicting then os.write.over(origin / "shared.txt", "master work\n")
    else os.write(origin / "elsewhere.txt", "master work\n")
    git(origin, "add", ".")
    git(origin, "commit", "-q", "-m", "master moved on")
    origin

  private def cloneOf(origin: os.Path): os.Path =
    val root = os.temp.dir(prefix = "integration-clone") / "repo"
    git(root / os.up, "clone", "-q", origin.toString, root.toString)
    git(root, "config", "user.email", "test@example.com")
    git(root, "config", "user.name", "Test")
    root

  test("a mechanically mergeable base is merged in and pushed"):
    val origin = diverged(conflicting = false)
    git(origin, "config", "receive.denyCurrentBranch", "ignore")
    val root = cloneOf(origin)

    GitHub.syncIntegrationBranchWithDefault[IO](_ => IO.unit)(root, BranchName("task-4")).map { synced =>
      assert(synced)
      git(root, "fetch", "origin", "task-4")
      // master's commit is now an ancestor of the integration branch.
      assertEquals(git(root, "merge-base", "--is-ancestor", "origin/master", "origin/task-4").exitCode, 0)
    }

  test("a conflict git cannot resolve is left for a human, with no half-merged state"):
    val origin = diverged(conflicting = true)
    git(origin, "config", "receive.denyCurrentBranch", "ignore")
    val root = cloneOf(origin)

    GitHub.syncIntegrationBranchWithDefault[IO](_ => IO.unit)(root, BranchName("task-4")).map { synced =>
      assert(!synced)
      git(root, "fetch", "origin", "task-4")
      assertEquals(git(root, "merge-base", "--is-ancestor", "origin/master", "origin/task-4").exitCode, 1)
    }

/** A subtask opens its PR against `task-<parent>`. Nothing owned the existence of that branch, so the first subtask of
  * a parent found no base on origin and `gh pr create` failed after the agent had already done the work.
  */
class EnsureIntegrationBaseSuite extends CatsEffectSuite:
  private def git(cwd: os.Path, args: String*): os.CommandResult =
    os.proc("git" +: args).call(cwd = cwd, stdout = os.Pipe, stderr = os.Pipe, check = false)

  private def cloneWithOrigin(): os.Path =
    val origin = os.temp.dir(prefix = "ensure-base-origin")
    git(origin, "init", "-q", "-b", "master")
    git(origin, "config", "user.email", "test@example.com")
    git(origin, "config", "user.name", "Test")
    git(origin, "config", "receive.denyCurrentBranch", "ignore")
    os.write(origin / "seed.txt", "seed\n")
    git(origin, "add", ".")
    git(origin, "commit", "-q", "-m", "seed")

    val root = os.temp.dir(prefix = "ensure-base-clone") / "repo"
    git(root / os.up, "clone", "-q", origin.toString, root.toString)
    git(root, "config", "user.email", "test@example.com")
    git(root, "config", "user.name", "Test")
    root

  test("a missing integration base is created on origin from the default branch"):
    val root = cloneWithOrigin()
    GitHub
      .ensureIntegrationBase[IO](_ => IO.unit)((root, Some(BranchName("task-5"))))
      .productR(GitHub.remoteBranchExists[IO](root, BranchName("task-5")))
      .map { exists =>
        assert(exists)
        git(root, "fetch", "-q", "origin")
        // It starts at the default branch, which is what later merges back.
        assertEquals(
          git(root, "rev-parse", "origin/task-5").out.text().trim,
          git(root, "rev-parse", "origin/master").out.text().trim
        )
      }

  test("an existing integration base is left exactly where it is"):
    val root = cloneWithOrigin()
    os.write(root / "work.txt", "subtask work\n")
    git(root, "checkout", "-q", "-b", "task-5")
    git(root, "add", ".")
    git(root, "commit", "-q", "-m", "existing integration work")
    git(root, "push", "-q", "origin", "task-5")
    val before = git(root, "rev-parse", "origin/task-5").out.text().trim

    GitHub.ensureIntegrationBase[IO](_ => IO.unit)((root, Some(BranchName("task-5")))).map { _ =>
      git(root, "fetch", "-q", "origin")
      assertEquals(git(root, "rev-parse", "origin/task-5").out.text().trim, before)
    }

  test("a task with no parent has no integration base to create"):
    val root = cloneWithOrigin()
    GitHub.ensureIntegrationBase[IO](_ => IO.unit)((root, None)).map { _ =>
      assertEquals(git(root, "ls-remote", "--heads", "origin").out.text().trim.linesIterator.size, 1)
    }
