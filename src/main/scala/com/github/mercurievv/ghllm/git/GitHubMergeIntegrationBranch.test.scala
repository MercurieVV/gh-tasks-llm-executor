package com.github.mercurievv.ghllm.git

import com.github.mercurievv.ghllm.*
import com.github.mercurievv.ghllm.agent.*
import com.github.mercurievv.ghllm.arrow.*
import com.github.mercurievv.ghllm.cli.*
import com.github.mercurievv.ghllm.git.*
import com.github.mercurievv.ghllm.metrics.*

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
