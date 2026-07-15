# gh-tasks-llm-executor

Picks up a runnable GitHub issue in a target repo, claims it, implements it in
an isolated git worktree with an LLM agent, and opens/merges a PR — or targets
one specific issue with `--task`.

## Run directly from GitHub (no clone needed)

`scala-cli` can fetch `.scala` sources straight from raw GitHub URLs, so you
don't need to clone this repo to use it. Run it with your **target** repo as
the current directory — that's where worktrees, `gh` calls, and issue claims
happen:

```bash
cd /path/to/target-repo

BASE=https://raw.githubusercontent.com/MercurieVV/gh-tasks-llm-executor/master

scala-cli run \
  $BASE/project-remote.scala \
  $BASE/main.scala $BASE/Git.scala $BASE/github.scala \
  $BASE/IssueClaim.scala $BASE/AgentExecutor.scala $BASE/AgentInventory.scala \
  $BASE/TaskLogger.scala \
  -- --task=123
```

- `project-remote.scala` stands in for `project.scala` here — it carries the
  same `//> using scala`/`//> using dep` directives, but not the
  `//> using exclude ...` ones. Those are directory-scoped and make
  `scala-cli` fail (`os.PathError$InvalidSegment`) when every input is a
  remote URL instead of a local path, so `project.scala` itself can't be
  used for this launch path.
- Drop `-- --task=123` to let it auto-select the next runnable open issue
  instead of a specific one. `--issue=123` also works.
- Wrap it in a shell function/alias if you use it often:

  ```bash
  gh-task() {
    BASE=https://raw.githubusercontent.com/MercurieVV/gh-tasks-llm-executor/master
    scala-cli run \
      $BASE/project-remote.scala \
      $BASE/main.scala $BASE/Git.scala $BASE/github.scala \
      $BASE/IssueClaim.scala $BASE/AgentExecutor.scala $BASE/AgentInventory.scala \
      $BASE/TaskLogger.scala \
      -- "$@"
  }
  # then: gh-task --task=123
  ```

To pin a release instead of always running the latest `master`, replace
`master` in `BASE` with a commit SHA or tag.

## Run from a local clone

```bash
git clone https://github.com/MercurieVV/gh-tasks-llm-executor.git
cd /path/to/target-repo
scala-cli run /path/to/gh-tasks-llm-executor -- --task=123
```

## Requirements on the target repo

- A git repo with an `origin` remote pointing at the GitHub repo that owns
  the issue — used for `gh issue`/`gh pr` calls and for the cross-process
  issue claim (a ref pushed to `origin`, see `IssueClaim.scala`).
- `gh` CLI installed and authenticated for that repo.
- Optional: `.gh-tasks-llm-executor/agent-runners.json` in the repo root to
  declare available agent runners/models. If absent, it falls back to a
  single `claude --model opus` runner (see `AgentInventory.scala`).

## What it does

1. Fetches open issues, filters out ones with unresolved dependencies, open
   child tasks, or a `needs-input` status.
2. Evaluates the (or each, in order) candidate issue and claims it via
   `IssueClaim` — a ref push to `origin` that's atomic on the git server, so
   two processes racing the same issue can't both win. A losing claim just
   moves on to the next candidate instead of failing the run.
3. Creates an isolated git worktree/branch (`Git.acquireWorktree`), runs the
   configured agent, validates, commits, opens/merges a PR, and releases the
   worktree and claim.
