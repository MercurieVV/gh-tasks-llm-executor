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
| T06–T11 | done | `VerificationResult.scala`; `routeRunnerFallback` escalates on `Red`, hard-resets first, `MaxEscalationDepth = 2`. Corrected 2026-07-31: the verifier ran *after* the retry loop, so only turn caps and dead processes ever escalated — a failing compile or test killed the task. The retried unit is now `runTaskWithRunner.andThen(runProjectValidation)` |
| T12 | done | `AgentInventory.selectRunnerFor` + `breakEvenRateAgainst` + `successRate(minSample = 20)` |
| T13–T15 | done | `Impl.ScalaSemanticMandate`; memoised `refreshSemanticDbIfNeeded`; `ScalaTextToolCallEvent` |
| T16 | done | `TurnCap` (default 25) raises `TurnCapExceeded`, which becomes a `Red` |
| T17 | done | `TaskArtifact.bound` enforced in `GitHub.renderDependencyConclusions` — the contract existed unused until it was wired there |
| T18 | done, and now read | `PrefixKey(runner, model, worktree)` is built in `NodeProfiles` and decides which children amortise a parent's prefix in `TaskTree.foldNode`. Nothing caches on it *at run time* — that is still Stage 6. `stablePrefixHash` deleted 2026-07-31; the surviving fields wired into the cost fold the same day. See both amendments under T18 |
| T19 | done | `Impl.taskPrompt` is ordered most-stable-first; `PromptSegmentOrderSuite` pins it |
| T20 | done for `claude`, both paths | `markCachePeers` (root batch) and `collectPendingDependencies` (a split's siblings) mark ≥3 same-`(agent, model)` peers → `ENABLE_PROMPT_CACHING_1H` in the subprocess env. Independent of `--parallel`: the window outlives the run. Still a no-op for non-`claude` agents |
| T21–T22 | done | `TaskF`, `TaskGraph.coalgebra`, `TaskTree.costAlgebra`, `annotate` |
| T23 | done, scope decided | the tree fold stays a planning/reporting artifact; the measurement it exposes now drives routing leaf-locally via `AgentTool.costWith`. See "Why the fold does not route" below |

### Why the fold does not route (T22/T23 scope decision, 2026-07-30)

T22's `Do NOT` calls the cost algebra "the plan's decision procedure", which implied
execution should consume the annotated tree. It should not, and this is deliberate.

Every scheduling decision this executor makes is **leaf-local**: which runner takes
this task, whether to escalate after a red, whether this task's siblings are cache
peers. None of them needs to know a subtree's shape or total. What the tree fold
uniquely produces is `subtreeUsd` — a whole-plan number — and whole-plan questions are
answered before anything runs, by a person deciding whether a plan is worth starting.
That is the `estimate` CLI, and it already works.

Making `executeRecursive` consume the `Cofree` annotation would make a per-run GitHub
tree walk a prerequisite for every routing decision, to inform choices that do not
depend on the tree.

What *was* missing is that the measurement behind the fold never reached the decisions
that could use it. `AgentTool.cost` assumed a fixed 20k input / 4k output for every
runner and every phase, so `breakEvenRateAgainst`'s `c/s` was a ratio of list prices
wearing a cost ratio's clothes — and with the same constants on both sides, they
cancelled. `costWith(meanUsage(phase, runner))` closes that: same Stage 0 events the
fold reads, applied where the decision is actually made.

### Stage 2 tool-ergonomics audit (2026-07-31)

Stage 2's third bullet — "audit tool ergonomics — anywhere an agent needs 3 calls to
accomplish one thing" — run against the 1,336 recorded runs in
`.gh-tasks-llm-executor/logs` (1,297 aider, 33 codex, 5 claude). Only the codex and
claude runs can use MCP: `TaskRunner.shouldInjectScalaSemanticInstruction` gates the
mandate on `agent.value` being `claude` or `codex`, so aider runs are outside this
audit and navigate by aider's own repo map.

**Finding 1 — the workspace-root handshake. Fixed.** The mandate told the agent to
call `set_workspace_root` before its first query, and that tool needs an absolute
path — which no prompt contained. So each Scala task opened with a discovery round
trip (`get_workspace_root`, or `pwd`) to learn a directory the executor had chosen
itself. `taskPrompt` now emits a `Worktree:` line and the mandate points at it. The
line sits in the volatile tail on purpose: the mandate is prepended at position 0 by
`TaskRunner.effectivePrompt` and is fully constant, which is what makes it cacheable
across siblings (T19/T20). Putting a per-task path there would invalidate the shared
prefix for every task in the run — `WorktreeHandoffSuite` pins that it does not.

**Finding 2 — redundant SemanticDB refreshes.** `refreshSemanticDbBeforeDispatch`
already refreshes the index in `run.worktreePath`, memoised on a source hash, right
before the agent starts. Nothing said so, and evaluator-authored issue bodies routinely
carry a "run `scripts/refresh-semanticdb.sh` first if the index is stale" line, so
agents re-ran a full recompile on an already-fresh index. The mandate now states that
the index is fresh. This is a prompt claim, not an interlock; if it turns out agents
still re-run it, the honest fix is a wrapper that no-ops on a matching hash.

**Finding 3 — MCP reliability, not fixed here.** 6 of the 31 runs that reach
scala-semantic at all logged `tool call failed`: `set_workspace_root` ×7,
`find_symbol` ×5, `structure` ×1, `get_workspace_root` ×1. Agents did the right thing
per the mandate's last line — said so, then fell back to shell text tools — which is
precisely the spend Stage 3 exists to remove. Finding 1 removes the two most common
failing calls from the common path, but the underlying transport failures
(`agent-10235-codex`: "its transport closed during `structure` and could not be
restarted") are server-side and out of this repo's reach. `ScalaTextToolCallEvent`
already counts the fallback, so the payoff is measurable once run data accumulates.

Beyond the numbered tasks: Stage 5 never got a numbered breakdown, because its
audit turned out to be the deliverable. Its seven named steps were checked
against the code on 2026-07-31 and only two were ever done by a model. Both are
now fixed, so Stage 5 is closed by audit rather than by tasks:

| Stage 5 step | Who does it | Verdict |
|---|---|---|
| conflict detection | `Git.unresolvedConflictFiles` / `hasUnresolvedConflicts` | git-derived. A model only *resolves* conflicts, which is not deterministic |
| test running | repo's own hook via `Git.runProjectValidation` | deterministic — but until 2026-07-31 it ran outside the escalation loop, so its verdict killed tasks instead of escalating them. Fixed |
| lint / format | same hook | deterministic. The executor deliberately does not know a project's checks; the project supplies one executable gate |
| PR templating | `Impl.extractAgentFinalization` | model-written, and left that way: a commit title and PR body summarising an arbitrary diff are prose, not a deterministic function of it. Parsed out of the run that already happened — no extra call |
| metadata parsing | `TaskMetadata.parse` | pure string parsing |
| runner selection | `AgentInventory.selectRunnerFor` | measured, in code. The one model input is the evaluator's ability *names*, and the priced catalogue that invited it to pick a runner outright was removed |
| branch hygiene | executor (`Agent boundary:` forbids the agent all of it) | deterministic |

The second fix: the final answer contract asked the implementer to "List
validation commands you ran and whether they passed" — a deterministic question
the hook answers authoritatively, reported by a model, parsed by nothing, and
misleading exactly when the two disagree. Removed 2026-07-31; Workflow step 4
still requires the verification itself.

Four further Stage 5 leaks were closed earlier and directly: the implementer's
split instructions, the evaluator's verdict/preservation restating, the
runner-authored file list (now read from git), and the priced tool catalogue in
both evaluation prompts — replaced 2026-07-31 by
`AgentInventory.abilityVocabulary`, ~3.2k characters down to ~270.

No before/after payoff measurement exists yet — Stage 0 records the dimensions,
but nothing has been run long enough to compare.

### Why there is no payoff data (verified against the backend, 2026-07-31)

Checked rather than assumed, because "blocked on run data" and "silently
recording nothing" look identical from inside the code.

- **The pipeline works.** VictoriaMetrics is up and holds 562 datapoints across
  150 series. A probe emitting the exact attribute set of
  `VictoriaMetricsBackend.attributes` came back with every label intact,
  including the boolean `escalated` — so nothing is dropped at ingestion.
- **Every stored point predates the dimensions.** `Attribute("runner", …)`
  reached `master` in the PR #71 merge at 2026-07-30 13:19. The last event
  recorded anywhere was 11:04 the same day. Not one run has happened on a build
  that emits `phase`/`runner`, which is why `successRate` and `meanUsage` have
  never returned anything but `None`.
- **`metrics readiness` now says this out loud.** It reports events without a
  phase/runner and, per measured pair, how many more are needed for `minSample`.
  Against the live backend today it prints "50 event(s), 50 without a
  phase/runner". Before it existed, the only local view (`renderEvents`) showed
  none of the five dimensions, which is how two days of empty ones went unnoticed.
- **Runner-level dispatch was the larger waste.** 1343 agent dispatches, 271
  recorded events (14.6%). 1300 of those dispatches were aider, of which 1156
  produced no usage at all — aider's DeepSeek key returned `Insufficient
  Balance`, and the executor re-dispatched it ~1150 times. Already fixed by
  `3af6ad4` (07-30 12:45, `terminationReason` treats billing failures as fatal)
  plus the `hardExhausted` exclusion in `AgentInventory.parseTool`. Both landed
  after the wasted runs and, like the labels, have not yet met a live run.

The blocker is therefore a single executor run on current `master`, not code.

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
final case class PrefixKey(runner: String, model: Option[String], worktree: os.Path)
```

**Done when.** `NodeProfiles` builds one per node so cache spend can be attributed to
a `(runner, model, worktree)`.

**Do NOT.** Use `IOLocal`. This is explicit payload data so `TokenMetrics` can attribute
cache hits (plan §2 Stage 6).

**Amended 2026-07-31 — the `stablePrefixHash` field was removed.** As specified it was
a SHA-256 over the stable prompt layers `[0..2]`, and it was built that way. Nothing
ever cached on it, and it could not be made to: its only caller (`NodeProfiles`) passes
the constants `"system" | "repo" | tier`, so the digest was a restatement of `tier`,
which already sits beside it in `TaskTree.Attr`. What actually determines whether two
runs share a cached prefix is `(agent, model)` — the key `CachePeers`/T20 groups on.
A hash keying nothing is not free: it is a field readers must reason about, and one
that invites re-deriving the wrong grouping. Deleted along with `PrefixKey.of` and the
five hash tests; `NodeProfilesSuite` now asserts the same-phase grouping on `tier`.

**Amended 2026-07-31 — the remaining three fields now key something.** After the hash
went, `PrefixKey` was still read by nobody: `NodeProfiles` built it, `NodeProfile` and
`TaskTree.Attr` carried it, and neither `foldNode` nor `PlanEstimate.render` touched it.
The same was true of `Cost.estimatedPerNodeUsd` — computed, never printed, asserted only
by its own tests. They were one dead feature, not two: the per-node figure is the
shared-prefix allocation, and `PrefixKey` is what decides who shares.

`estimatedPerNodeUsd` divided a node's cost by its raw fan-out, i.e. assumed every child
reuses the parent's cached prefix. It now divides by the count of children whose
`PrefixKey` matches, so a fan-out across runners is priced as the serial work it is. That
also makes the estimator agree with runtime, where `CachePeers` grants the extended TTL
only to siblings sharing an `(agent, model)`. `PrefixKey` moved onto `Cost` (a child is
folded to a `Cost` before its parent is priced, so the key has to travel upward) and off
`Attr`, which held a duplicate. `PlanEstimate.render` prints `per-node=`; when it equals
`own=`, nothing in that fan-out shares a prefix.

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
- **Both paths are covered now.** The root batch is grouped in `markCachePeers`; a
  split's siblings are grouped in `collectPendingDependencies`, which is the only place
  that can see the sibling set. Both share `CachePeers.qualifying`.
- **Concurrency is not required, and is not consulted.** A 1-hour window is precisely
  what decouples readers from the writer, so neither path gates on `--parallel` and
  `ParallelArrows.MaxParallelism = 2` does not undercut the ≥3 threshold — a later peer
  reads the prefix, it does not have to read it simultaneously.

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