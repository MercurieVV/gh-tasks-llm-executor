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
