# gh-tasks-llm-executor

Picks up a runnable GitHub issue in a target repo, claims it, implements it in
an isolated git worktree with an LLM agent, and opens/merges a PR — or targets
one specific issue with `--task`.

## Run directly from GitHub (no clone needed)

`scripts/remote-run.sh` fetches every `.scala` source this app needs straight
from raw GitHub URLs and runs `scala-cli` against them — no clone required.
Run it with your **target** repo as the current directory — that's where
worktrees, `gh` calls, and issue claims happen:

```bash
cd /path/to/target-repo

curl -fsSL https://raw.githubusercontent.com/MercurieVV/gh-tasks-llm-executor/master/scripts/remote-run.sh | bash -s -- --task=123
```

- `scala-cli` resolves and caches the JVM dependencies itself on every run —
  the script only pins which source files to fetch.
- Drop `--task=123` to let it auto-select the next runnable open issue
  instead of a specific one. `--issue=123` also works.
- Add `--recursive` to walk each open root task's full dependency tree to
  closure (including subtasks created mid-run by a split) before moving to
  the next root, instead of only picking off already-ready leaf tasks.
- Pin a release instead of always running latest `master` with
  `GH_TASKS_REF`:

  ```bash
  curl -fsSL https://raw.githubusercontent.com/MercurieVV/gh-tasks-llm-executor/master/scripts/remote-run.sh | GH_TASKS_REF=<sha-or-tag> bash -s -- --task=123
  ```

- Prefer no piping-to-bash? Same launcher, run locally instead:

  ```bash
  curl -fsSL https://raw.githubusercontent.com/MercurieVV/gh-tasks-llm-executor/master/scripts/remote-run.sh -o gh-task.sh
  chmod +x gh-task.sh
  ./gh-task.sh --task=123
  ```

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

Raw model prices live in `.gh-tasks-llm-executor/model-prices.json`. Discovery
only reads this committed file and never calls the network. To refresh it,
review updated vendor prices into a JSON file with the same schema, then run
`scala-cli scripts/refresh-model-prices.scala -- /path/to/reviewed-prices.json`
followed by `scala-cli scripts/discover-agent-runners.scala`.

### Collect agent-runners info

Run from repo root (writes/updates
`.gh-tasks-llm-executor/agent-runners.json` in place):

```bash
scala-cli scripts/discover-agent-runners.scala
```

Probes locally installed CLIs (`claude`, `codex`, `gemini`, ...) for
availability/version, cross-references `model-prices.json` for pricing, and
regenerates the runner list the executor selects from at run time — rerun it
whenever installed agent CLIs or their models change.

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

If evaluation needs clarification, the script posts a `Questions before
execution:` issue comment, plays a best-effort notification sound, then keeps
the current run alive while polling for a human reply. Defaults are 45 minutes
total wait and 30 seconds between polls. Configure with:

- `GH_TASKS_USER_INPUT_WAIT_MINUTES`
- `GH_TASKS_USER_INPUT_POLL_SECONDS`
- `GH_TASKS_USER_INPUT_SOUND=0` to disable sound
