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
only reads this committed file and never calls the network. On a target repo
that has no copy yet, `resolveContext` seeds one automatically on first run
from the pricing bundled with this project (`resources/model-prices.json`,
never overwrites an existing file). To refresh it with updated vendor prices
instead, review them into a JSON file with the same schema, then run
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

This also runs automatically: `resolveContext` refreshes the snapshot
in-process (`AgentRunnersDiscovery.scala`, a duplicate of this script kept in
sync by hand) once per invocation, only if it's older than
`GH_TASKS_AGENT_RUNNERS_TTL_MINUTES` (default 60). A probe failure never
blocks task execution, same as the vendor-budget refresh below.

### Vendor budget balancing

Each vendor (claude, codex, gemini, deepseek) has its own token/spend quota,
so `AgentTool.priority` factors in how much of that quota is already used —
a saturated vendor loses tiebreaks against similarly-priced tools before
crossing into a worse capability tier, and hard-exhaustion marks it
unavailable outright. See `.gh-tasks-llm-executor/vendor-budgets.json`
(gitignored, per-machine, carries $ figures).

This runs automatically: `resolveContext` refreshes the snapshot in-process
(`VendorBudgets.scala`) once per invocation, only if it's older than
`GH_TASKS_VENDOR_BUDGET_TTL_MINUTES` (default 15). A probe failure never
blocks task execution. To refresh manually instead:

```bash
scala-cli scripts/discover-vendor-budgets.scala
```

Env vars: `AGENT_RUNNER_CLAUDE_SESSION_BUDGET_USD` (default 25, claude has no
vendor-reported quota so this is a self-imposed cost cap),
`AGENT_RUNNER_DEEPSEEK_MONTHLY_BUDGET_EUR` (default 20), `DEEPSEEK_API_KEY`
(required for the deepseek probe), `GOOGLE_CLOUD_PROJECT` (required for the
gemini/Vertex probe).

### Local token metrics

Token metrics are exported with otel4s/OpenTelemetry to VictoriaMetrics by default.
Grafana is provisioned as the local metrics UI, using VictoriaMetrics as a
Prometheus-compatible datasource. Start the local metrics stack with:

```bash
docker compose up grafana
```

The local VictoriaMetrics OTLP endpoint is
`http://localhost:8428/opentelemetry/v1/metrics`. Grafana is available at
`http://localhost:3000` with anonymous local admin access and a provisioned
`Token Metrics` dashboard.

The local viewer queries VictoriaMetrics unless `--backend=jsonl` or
`GH_TASKS_TOKEN_METRICS_BACKEND=jsonl` is used:

```bash
scala-cli run /path/to/gh-tasks-llm-executor -- metrics
scala-cli run /path/to/gh-tasks-llm-executor -- metrics summary --vendor=codex
scala-cli run /path/to/gh-tasks-llm-executor -- metrics readiness
scala-cli run /path/to/gh-tasks-llm-executor -- metrics --task=123 --json
```

`metrics readiness` answers whether runner selection has anything to work with.
`AgentInventory.selectRunnerFor` needs 20 recorded runs per `(phase, runner)`
before `successRate` and `meanUsage` return a figure; below that it falls back to
`Priority.score`, which is correct but looks identical to the ladder economics
being switched off. This view reports how many events carry no phase/runner at
all, and how many more each measured pair needs:

```
50 event(s), 50 without a phase/runner (invisible to runner selection), minSample=20
No (phase, runner) pair is measured. Runner selection is falling back to Priority.score
for every phase - the cost ratio and success rate are not being consulted at all.
```

Supported filters: `--vendor`, `--task`/`--issue`, `--since`, `--until`,
`--limit`, `--backend`, `--victoria-url`, and `--path` for the JSONL fallback.

`--json` emits the same encoding the JSONL backend writes (`TokenMetrics.eventJson`),
so token counts are nested under `usage` and every measurement dimension —
`phase`, `runner`, `turnCount`, `escalated`, `outcome` — is present. It was a
separate flat encoding until 2026-07-31, and that copy had none of those five.

Metrics can still be persisted locally as JSONL at
`.gh-tasks-llm-executor/token-metrics.jsonl` through
`TokenMetrics.JsonlTokenMetricsBackend`.

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
