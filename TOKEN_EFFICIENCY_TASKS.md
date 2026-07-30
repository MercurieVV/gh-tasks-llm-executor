# Token Efficiency — Task Breakdown

Executable decomposition of `TOKEN_EFFICIENCY_PLAN.md`.

Each task is scoped so a weak model can implement it without making design decisions:
one or two files, the exact insertion point, the target signature written out, and a
named test as the completion check. The `Do NOT` fence exists because weak models
expand scope — it is part of the specification, not advice.

## Status (verified against the code, 2026-07-30)

Read this before picking up a task — several are already done and the plan text
around them is older than the code.

| Task | State | Evidence |
|---|---|---|
| T01–T05 | done | `TokenMetricsEvent` carries `phase`/`runner`/`model`/`turnCount`/`escalated`/`outcome`; exported as OTel attributes; `cacheHitRatio` reads them back |
| T06–T11 | done | `VerificationResult.scala`; `routeRunnerFallback` escalates on `Red`, hard-resets first, `MaxEscalationDepth = 2` |
| T12 | done | `AgentInventory.selectRunnerFor` + `breakEvenRateAgainst` + `successRate(minSample = 20)` |
| T13–T15 | done | `Impl.ScalaSemanticMandate`; memoised `refreshSemanticDbIfNeeded`; `ScalaTextToolCallEvent` |
| T16 | done | `TurnCap` (default 25) raises `TurnCapExceeded`, which becomes a `Red` |
| T17 | done | `TaskArtifact.bound` enforced in `GitHub.renderDependencyConclusions` — the contract existed unused until it was wired there |
| T18 | done as data only | `PrefixKey` is built in `NodeProfiles` for cost attribution; nothing caches on it |
| T19 | done | `Impl.taskPrompt` is ordered most-stable-first; `PromptSegmentOrderSuite` pins it |
| T20 | done for `claude` root batches; untested until now | `markCachePeers` (≥3 peers + `--parallel`) → `ENABLE_PROMPT_CACHING_1H` in the subprocess env. Not applied to a split's children, and a no-op for non-`claude` agents |
| T21–T22 | done | `TaskF`, `TaskGraph.coalgebra`, `TaskTree.costAlgebra`, `annotate` |
| T23 | partial | the fold is reachable from the `estimate` CLI path (`PlanEstimate`), but execution routing still does not consume it — the cost model informs a human, not the scheduler |

Beyond the numbered tasks: Stage 5 has no task breakdown, and three of its leaks
have been closed directly (the implementer's split instructions, the evaluator's
verdict/preservation restating, and the runner-authored file list, now read from
git). No before/after payoff measurement exists yet — Stage 0 records the
dimensions, but nothing has been run long enough to compare.

## How to read a task

| Field | Meaning |
|---|---|
| **Files** | The only files that may be modified. Touching others = the task was misread. |
| **Depends** | Must be merged first. |
| **Tier** | Suggested runner strength. See the tier legend below. |
| **Do** | The change, stated so no design choice remains. |
| **Done when** | The check. Almost always a named munit test that must pass. |
| **Do NOT** | Explicit scope fence. |

### Tier legend

- **low** — mechanical. Add a field, thread a parameter, write a table-driven test.
  Safe for Haiku-class runners.
- **medium** — localised logic with a stated algorithm. Needs care, not invention.
- **high** — genuine design latitude remains. Do not route these to a weak runner;
  under-powering here is what the plan's own §0 warns about.

Verify every change with `scala-cli test .`. Use `mcp__scala-semantic__*` for symbol,
type, signature and reference questions on `.scala` files — never `cat`/`rg`/`sed`.
Run `scripts/refresh-semanticdb.sh` first if the index is stale.

## Known code shape (verified, do not re-derive)

```scala
// TokenMetrics.scala:14
final case class TokenMetricsEvent(
    timestampMillis: Long,
    vendor: TokenUsage.Vendor,
    usage: TokenUsage.TokenSnapshot,
    taskNumber: Option[TaskNumber],
    model: Option[String],
    scope: String
)

// TokenUsage.scala:30 — cacheRead/cacheWrite ALREADY EXIST
final case class TokenSnapshot(input: Long, output: Long,
                               cacheRead: Long, cacheWrite: Long, total: Long)

// AgentInventory.scala:171
def selectRunnerFor(requiredAbilities: Map[String, Double],
                    preferred: List[TaskRunner]): Option[TaskRunner]

// AgentInventory.scala:182
def nextStrongerImplementor(runner: TaskRunner): Option[TaskRunner]

// Implementations.scala:858 — escalates on Throwable only
def routeRunnerFallback[F[_]: Sync]
  : -->[F, (PreparedTask, Throwable), Either[Throwable, PreparedTask]]
```

`AgentTool` already carries `inputUsdPerMTok`, `outputUsdPerMTok`, `cost`, `priority`
(`AgentInventory.scala:30-31, 40, 64`).

---

# Stage 0 — Measurement

Prerequisite for Stage 1. No behaviour changes here; only recording.

## T01 — Add measurement fields to `TokenMetricsEvent`

- **Files:** `TokenMetrics.scala`, `TokenMetrics.test.scala`
- **Depends:** —
- **Tier:** low

**Do.** Add these fields to `TokenMetricsEvent`, all with defaults so existing
construction sites keep compiling:

```scala
phase: Option[String] = None,
runner: Option[String] = None,
turnCount: Option[Int] = None,
escalated: Boolean = false,
outcome: Option[String] = None   // "green" | "red" | "error"
```

Extend the JSONL write/read in `JsonlTokenMetricsBackend` to round-trip them. Absent
keys in an old line must decode to the defaults.

**Done when.** `TokenMetrics.test.scala` has a test named `jsonl round-trips
measurement fields` asserting a fully populated event survives `record` → `query`
unchanged, and a second test `jsonl decodes legacy lines without measurement fields`
asserting a line lacking the new keys decodes with defaults.

**Do NOT.** Populate the fields anywhere. Change OTel export. Change `TokenSnapshot`.

## T02 — Export the new fields as OTel attributes

- **Files:** `TokenMetrics.scala`
- **Depends:** T01
- **Tier:** low

**Do.** In the private `attributes` method (`TokenMetrics.scala:120`), emit one
attribute per new field, skipping `None`. Attribute keys: `phase`, `runner`,
`turn_count`, `escalated`, `outcome`.

**Done when.** `scala-cli test .` passes and a test named `attributes include
measurement fields` asserts the produced attribute keys for a populated event.

**Do NOT.** Change the metric name, the existing attributes, or the export protocol.
Do not add cardinality-unbounded attributes such as task title.

## T03 — Record cache hit ratio in the summary

- **Files:** `TokenMetrics.scala`, `TokenMetrics.test.scala`
- **Depends:** —
- **Tier:** low

**Do.** Add to `TokenMetricsBackend`:

```scala
def cacheHitRatio(query: TokenMetricsQuery): Double
```

Defined as `cacheRead / (cacheRead + input)` over the summed snapshot, returning `0.0`
when the denominator is zero. `TokenSnapshot` already carries `cacheRead`; no new
capture is needed.

**Done when.** Test `cacheHitRatio is zero for empty query` and `cacheHitRatio sums
across events` pass.

**Do NOT.** Add caching behaviour. This task only reports what is already collected.

## T04 — Populate `phase` and `runner` at the recording call sites

- **Files:** `Implementations.scala` (recording sites only)
- **Depends:** T01
- **Tier:** medium

**Do.** Find every `TokenMetricsEvent(...)` construction with
`mcp__scala-semantic__find_usages` on the `TokenMetricsEvent` symbol. At each site,
pass `phase` from the task's `Phase:` metadata (see `taskMetadata.scala`) and `runner`
from `claimedTask.runner.display`.

**Done when.** Every construction site passes both, and `scala-cli test .` is green.

**Do NOT.** Populate `turnCount`, `escalated` or `outcome` — those are T05 and T09.
Do not restructure the arrows.

## T05 — Populate `outcome` and `turnCount`

- **Files:** `Implementations.scala`, `AgentExecutor.scala`
- **Depends:** T04
- **Tier:** medium

**Do.** Set `outcome` to `"green"` when the post-run verification passes, `"red"` when
it fails, `"error"` when the runner threw. Set `turnCount` from the runner's reported
turn count where the runner exposes one; leave `None` where it does not.

**Done when.** A test asserts that a simulated failing run records `outcome = "red"`.

**Do NOT.** Change what verification runs, or add escalation. Recording only.

---

# Stage 1 — Escalation ladder

The highest-value change. T06–T08 are independent and can run in parallel.

## T06 — Introduce `VerificationResult`

- **Files:** `VerificationResult.scala` (new), `VerificationResult.test.scala` (new)
- **Depends:** —
- **Tier:** low

**Do.** New file, exactly:

```scala
enum VerificationResult:
  case Green
  case Red(summary: String, detail: String)
  case Failed(error: Throwable)

object VerificationResult:
  def fromThrowable(error: Throwable): VerificationResult = Failed(error)

  extension (self: VerificationResult)
    def isGreen: Boolean = self match
      case Green => true
      case _     => false

    // Bounded seed for the escalated attempt: never the failed transcript.
    def escalationSeed: Option[String] = self match
      case Green            => None
      case Red(s, d)        => Some(s"$s\n\n$d")
      case Failed(e)        => Option(e.getMessage)
```

**Done when.** Tests assert `isGreen` for each case and that `escalationSeed` is
`None` only for `Green`.

**Do NOT.** Wire it into anything. That is T09.

## T07 — Add the cost-ratio helper to `AgentTool`

- **Files:** `AgentInventory.scala`, `AgentInventory.test.scala`
- **Depends:** —
- **Tier:** low

**Do.** Add to `AgentTool` (near `cost`, `AgentInventory.scala:40`):

```scala
// Break-even success rate for trying this tool before `stronger`:
// the ladder wins iff observed success rate p > c/s. See
// TOKEN_EFFICIENCY_PLAN.md §2 Stage 1.
def breakEvenRateAgainst(stronger: AgentTool): Option[Double] =
  for
    c <- cost
    s <- stronger.cost
    if s > 0
  yield math.min(1.0, c / s)
```

**Done when.** Tests: a tool 20× cheaper yields `Some(0.05)`; a tool 2× cheaper yields
`Some(0.5)`; a tool more expensive than `stronger` clamps to `Some(1.0)`; a tool with
no price yields `None`.

**Do NOT.** Use it in selection. That is T12.

## T08 — Query observed success rate per (phase, runner)

- **Files:** `TokenMetrics.scala`, `TokenMetrics.test.scala`
- **Depends:** T01
- **Tier:** medium

**Do.** Add to `TokenMetricsBackend`:

```scala
// Observed p: fraction of recorded runs for this (phase, runner) whose
// outcome was "green". None when the sample is smaller than `minSample`.
def successRate(phase: String, runner: String, minSample: Int = 20): Option[Double]
```

Count only events where `outcome` is defined. `None` on a sample below `minSample` —
an unmeasured pair must not be treated as a 100% success rate.

**Done when.** Tests: below-threshold sample returns `None`; 8 green of 40 returns
`Some(0.2)`; events with `outcome = None` are excluded from both numerator and
denominator.

**Do NOT.** Change selection behaviour.

## T09 — Generalise `routeRunnerFallback` to `VerificationResult`

- **Files:** `Implementations.scala`
- **Depends:** T06
- **Tier:** medium

**Do.** At `Implementations.scala:858`, change the input type from `Throwable` to
`VerificationResult`:

```scala
def routeRunnerFallback[F[_]: Sync]
  : -->[F, (PreparedTask, VerificationResult), Either[VerificationResult, PreparedTask]]
```

Keep the body's structure. Escalate on `Red` and `Failed`; `Green` must never reach
this arrow — return it unchanged as `Left` if it does. At each existing call site, wrap
the throwable with `VerificationResult.fromThrowable`, so behaviour is unchanged by
this task alone.

**Done when.** `scala-cli test .` passes, `RepairLoop.test.scala` still green, and a
new test asserts `Red` produces a stronger runner in the returned `PreparedTask`.

**Do NOT.** Change *when* the arrow is invoked, add the verification step, or touch
runner selection. This is a type generalisation only — behaviour identical.

## T10 — Hard-reset the worktree before escalating

- **Files:** `Git.scala`, `Git.test.scala`
- **Depends:** —
- **Tier:** medium

**Do.** Add a `resetWorktree` operation performing `git reset --hard` plus
`git clean -fd` scoped to the task's worktree path. Call it in the escalation branch of
`routeRunnerFallback` before the stronger runner starts.

A failed cheap attempt leaves partial edits; without this the strong runner inherits
garbage and pays to untangle it (`TOKEN_EFFICIENCY_PLAN.md` §2 Stage 1, risk 2).

**Done when.** A test creates a worktree with an uncommitted change, calls
`resetWorktree`, and asserts a clean status.

**Do NOT.** Reset anything outside the task's own worktree path. Do not touch the
user's primary checkout. Guard the path and fail loudly if it is not a worktree.

## T11 — Cap escalation depth

- **Files:** `Implementations.scala`, `RepairLoop.test.scala`
- **Depends:** T09
- **Tier:** low

**Do.** Thread an `escalationDepth: Int = 0` through `PreparedTask`, incremented on
each escalation. At depth 2, stop escalating and return `Left` so the task surfaces to
a human.

**Done when.** Test `escalation stops at depth 2` asserts a third escalation attempt
returns `Left`.

**Do NOT.** Make the cap configurable. Hardcode 2.

## T12 — Break-even predicate in runner selection

- **Files:** `AgentInventory.scala`, `Priority.scala`, `AgentInventory.test.scala`
- **Depends:** T07, T08
- **Tier:** high

**Do.** Change `selectRunnerFor` (`AgentInventory.scala:171`) so that when a metrics
backend is available it prefers the cheapest candidate satisfying
`successRate(phase, runner) > candidate.breakEvenRateAgainst(nextStronger)`. When
either term is `None` (unpriced tool or insufficient sample), fall back to the current
`Priority.score` ordering unchanged.

**Done when.** Tests cover: cheap tool above break-even is selected; cheap tool below
break-even is skipped for the stronger one; missing sample falls back to existing
ordering with no change in result.

**Do NOT.** Route this to a weak runner — the fallback semantics carry real design
latitude, and getting them wrong silently degrades every task selection. Do not remove
`Priority.score`; it remains the fallback path.

---

# Stage 3 — Force `scala-semantic` on subagents

Independent of Stages 0–1. Good parallel work; T13 and T15 are mechanical.

## T13 — Inject the mandatory tool-use block into subagent prompts

- **Files:** `Implementations.scala` or `EvaluationArrows.scala` (prompt assembly only)
- **Depends:** —
- **Tier:** low

**Do.** Define a single constant holding the mandate (symbol/type/signature/hierarchy/
implicit/reference/call-path questions go through `mcp__scala-semantic__*`, never
`cat`/`rg`/`sed`; `document_outline` instead of reading a file). Append it to every
subagent prompt for a task touching `.scala` files.

**Done when.** A test asserts the assembled prompt for a Scala task contains the
constant, and the prompt for a non-Scala task does not.

**Do NOT.** Reword the existing prompt. Append only.

## T14 — Refresh SemanticDB before dispatch, memoised

- **Files:** `Implementations.scala`, `RunEnv.scala`
- **Depends:** —
- **Tier:** medium

**Do.** Before dispatching a Scala-touching task, run
`scripts/refresh-semanticdb.sh` — but memoise on a hash of the `.scala` source set so
it runs once per unchanged tree, not once per leaf.

**Done when.** A test asserts the refresh runs once across two dispatches with an
unchanged source hash, and twice when the hash changes.

**Do NOT.** Run it per subagent unconditionally — that would cost more wall-clock than
the tokens saved.

## T15 — Count `rg`/`cat` calls against `.scala` in subagent transcripts

- **Files:** `TaskLogger.scala`, `TokenMetrics.scala`
- **Depends:** T01
- **Tier:** low

**Do.** Scan the subagent transcript for shell invocations of `cat`, `rg`, `grep`,
`sed`, `head` or `tail` whose arguments include a `.scala` path. Record the count as
an OTel counter `scala_text_tool_calls` with the `runner` and `phase` attributes.

Unenforced prompt instructions get ignored. Without this metric there is no way to know
whether T13 worked.

**Done when.** A test feeds a transcript fixture with three matching and two
non-matching invocations and asserts a count of three.

**Do NOT.** Block or fail the run on a nonzero count. Measure first.

---

# Stage 2 — Turn-count reduction

## T16 — Enforce a per-leaf turn cap

- **Files:** `AgentExecutor.scala`
- **Depends:** T05
- **Tier:** medium

**Do.** Add a per-leaf turn cap. On exceeding it, terminate the runner and emit
`VerificationResult.Red("turn cap exceeded", …)` so the existing escalation path
handles it, rather than letting the run grind. Default cap 25; read from
`.gh-tasks-llm-executor/` config if present.

**Done when.** A test asserts a runner reporting 26 turns yields `Red`.

**Do NOT.** Implement escalation here — T09 owns it. Emit the `Red` and return.

---

# Stage 4 — Context as pointers

## T17 — Bound the task artifact

- **Files:** `taskMetadata.scala`, `taskMetadata.test.scala`
- **Depends:** —
- **Tier:** medium

**Do.** Define the node output contract as a case class — decision, files touched,
symbols, follow-ups — with a hard character bound (start at 2000). Truncate with an
explicit marker on overflow rather than silently passing an unbounded blob downstream.

**Done when.** Tests assert a bounded artifact round-trips, and an oversized one is
truncated with the marker present.

**Do NOT.** Change what the runners emit yet. Define and enforce the contract first.

---

# Stage 6 — Prefix ordering and caching

Lowest value. Do last. See `TOKEN_EFFICIENCY_PLAN.md` §2 Stage 6.

## T18 — `PrefixKey` as arrow payload data

- **Files:** `PrefixKey.scala` (new), `PrefixKey.test.scala` (new)
- **Depends:** —
- **Tier:** low

**Do.**

```scala
final case class PrefixKey(runner: String, model: Option[String],
                           worktree: os.Path, stablePrefixHash: String)
```

Plus `PrefixKey.of(...)` computing `stablePrefixHash` as a SHA-256 of the concatenated
stable prompt layers `[0..2]`.

**Done when.** Tests assert identical inputs give an identical hash, and that changing
any layer changes it.

**Do NOT.** Use `IOLocal`. This is explicit payload data so `TokenMetrics` can attribute
cache hits (plan §2 Stage 6).

## T19 — Order assembled prompts most-stable-first

- **Files:** prompt assembly site from T13
- **Depends:** T13
- **Tier:** medium

**Do.** Reorder to: `[0]` system + tool defs, `[1]` repo conventions, `[2]`
source-of-truth output, `[3]` parent artifact, `[4]` task instruction. Caching is
prefix-only, so a volatile segment placed early invalidates everything after it.

**Done when.** A test asserts the segment order in the assembled prompt.

**Do NOT.** Change segment *content*. Reorder only.

## T20 — Set the 1-hour cache TTL at fan-out points

- **Files:** runner invocation site
- **Depends:** T19
- **Tier:** medium

**Do.** Where a task fans out to siblings, mark the shared prefix with the extended
(1-hour) cache TTL. Costs ~2× to write; break-even ~3 reads — so apply it only at
fan-out with ≥3 children, not on a linear chain.

**Done when.** A test asserts the TTL flag is set for a 3-child fan-out and unset for a
single-child edge.

**Do NOT.** Launch siblings staggered — they must start together or the window is lost.

**Already implemented — for `claude`, at root-candidate level.**
`BusinessLogic.markCachePeers` sets `TaskRunner.extendedCacheTtl` when at least 3
selected candidates share the same `(agent, model)` and `--parallel` is on;
`TaskRunner.invocationEnvironment` turns that into `ENABLE_PROMPT_CACHING_1H=1`, which
`AgentExecutor` puts into the subprocess environment. The env var is how a vendor CLI
exposes what is otherwise an API-level `cache_control` field — there is no argv flag,
which is why grepping the command builders suggests the feature is absent.

Limits worth knowing before extending it:

- **`claude` only.** `invocationEnvironment` is empty for every other agent, so marking
  a `codex`/`gemini`/`aider` peer group is a silent no-op rather than a saving.
- **Root candidates, not fan-out children.** The peer group is the selected root batch.
  A task's dependencies still run sequentially through `RecursiveArrows.untilLeft`, so
  the children of one split never form a peer group at all. That is the remaining half
  of this task.
- **Concurrency is not required.** A 1-hour window is precisely what decouples readers
  from the writer, so `ParallelArrows.MaxParallelism = 2` does not undercut the ≥3
  threshold — the third peer reads the prefix later, not simultaneously.

---

# Recursion schemes

Droste supports Scala 3. This work is worth doing before Stages 4/6 accumulate several
bespoke traversals that then have to be unified.

## T21 — Add Droste and define the pattern functor

- **Files:** `project.scala`, `TaskTree.scala` (new)
- **Depends:** —
- **Tier:** medium

**Do.** Add the Droste dependency to `project.scala` alongside the existing
`//> using dependency` lines. Define `TaskF[A]` as the pattern functor for the task
tree (a node's own data plus `List[A]` children) with its `Functor` instance, and a
`Fix`/`Cofree` carrier.

**Done when.** `scala-cli compile .` passes and a test builds a 3-node tree and folds
it to a node count with `cata`.

**Do NOT.** Port existing traversals yet. Establish the functor first.

## T22 — Cost estimation as an algebra

- **Files:** `TaskTree.scala`, `TaskTree.test.scala`
- **Depends:** T21, T07
- **Tier:** high

**Do.** Express the plan's cost model as an algebra `TaskF[Cost] => Cost` folded with
`cata`, and annotate each node with `Cofree[TaskF, Attr]` carrying its `PrefixKey`,
tier and cost estimate. Coefficients come from Stage 0 metrics.

**Done when.** Property test: adding a sibling to a fan-out never increases the
estimated per-node cost.

**Do NOT.** Route to a weak runner — the algebra is the plan's decision procedure, and
a wrong fold silently misroutes every task.

## T23 — Port the traversals

- **Files:** call sites of the existing hand-rolled tree walks
- **Depends:** T22
- **Tier:** high

**Do.** Replace hand-rolled recursion over the task tree with algebras over the shared
traversal, one call site per commit.

**Done when.** Behaviour is unchanged and existing tests stay green.

**Do NOT.** Combine with behaviour changes. Pure refactor, one site at a time.

---

# Dependency graph

```
T01 ──┬── T02
      ├── T04 ── T05 ── T16
      ├── T08 ──┐
      └── T15   │
                ├── T12   (also needs T07)
T07 ──────┬─────┘
          └── T22 (also needs T21)

T06 ── T09 ── T11
T09 ..... T10 (T10 independent; wire into T09's escalation branch)

T13 ── T19 ── T20
T21 ── T22 ── T23

T03, T14, T17, T18  — independent, any time
```

## Suggested parallel waves

1. **Wave 1 (all independent, mostly low tier):** T01, T03, T06, T07, T10, T13, T14,
   T17, T18, T21
2. **Wave 2:** T02, T04, T08, T09, T15, T19
3. **Wave 3:** T05, T11, T20
4. **Wave 4 (high tier, do not weaken):** T12, T16, T22, T23

Wave 1 is nine low/medium tasks with no shared files — a clean fan-out, which is also
the exact shape Stage 6's sibling caching is designed for. Launch them together.