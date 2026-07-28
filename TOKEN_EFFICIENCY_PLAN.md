# Token Efficiency Plan

Plan for reducing token spend across the multi-agent task tree.

Executable decomposition into atomic, weak-runner-safe tasks:
[`TOKEN_EFFICIENCY_TASKS.md`](TOKEN_EFFICIENCY_TASKS.md).

## 0. Cost model — the premise this plan rests on

LLM APIs are **stateless**. Keeping a CLI process alive (`--recursive`) holds the
transcript in the CLI's memory, but every turn still ships the entire transcript to
the API and is billed for all of it. Process reuse does not make continuation free.

Savings come from three separate mechanisms, which must not be conflated:

1. **Prompt caching.** An identical token sequence starting at position 0 bills at
   ~10%. The cache key is `(account, model, exact token sequence)`. One differing
   token at position *k* invalidates everything from *k* onward.
2. **Model tiering.** A cheaper model costs 10–30× less per token. Dominates caching.
3. **Not spending tokens at all.** Fewer turns, less carried context, deterministic
   code instead of a model call. Dominates everything.

Ranked objective:

> Spend fewer tokens > spend them on a cheaper model > get them cache-discounted.

### Consequences

- **Caching is horizontal, not vertical.** Phase routing deliberately changes model
  per level (`plan` high tier, `test` cheapest), and model is part of the cache key.
  So a "path-to-node" cache mostly does not exist. Caching pays at **fan-out**: N
  siblings on one model sharing one prefix = N reads against one write.
- **Vendor preservation buys zero tokens.** Same vendor with a different model is a
  separate cache namespace. Vendor consistency matters for artifact format and budget
  accounting, not cost. Preserve *model across siblings*; let tier change down the path.
- **Cache write costs ~1.25×.** A prefix with a single reader is a net loss. Needs
  ≥2 readers to break even. The 1-hour TTL variant costs ~2× to write; break-even ~3
  reads, which is the right setting at a fan-out point.
- **Dependency ≠ context relevance.** `implement` depends on `plan`, but needs plan's
  *decision* (~200 tokens), not plan's *reasoning transcript* (~20k). Inheriting the
  transcript taxes every later turn. Dependency edges are the wrong default for
  context sharing.
- **Output tokens are billed twice.** ~5× input price when produced, then charged as
  input on every subsequent turn of that session. Verbose intermediate turns are the
  largest avoidable cost.
- **Turn count is roughly quadratic.** Each agentic turn re-sends the whole
  conversation. 20 turns → 8 turns is ~85% off.
- **The filesystem is free context.** The worktree costs 0 tokens until read.
  Artifact-on-disk handoff beats context inheritance in almost every case.
- **The verifier is free.** Compiler and test suite detect wrongness at zero token
  cost. Structure the system so cheap models attempt and deterministic tools judge,
  rather than expensive models deliberating to avoid being wrong.

## 1. Current state

- `AgentInventory.nextStrongerImplementor` (`AgentInventory.scala:182`) exists.
- It is wired at `Implementations.scala:860` in `routeRunnerFallback`, but triggers on
  `Throwable` only — i.e. the runner *crashed*.
- Escalation on **verification failure** (compile/test red) does not exist. This is the
  highest-value gap and a small diff.
- `PROJECT.md` currently selects tier by *predicting* required capability, and names
  under-powering as the primary failure mode. Prediction is unnecessary when a free
  verifier is available.

## 2. Stages

Ordered by value/effort. Each stage is independently shippable.

### Stage 0 — Measure first (prerequisite)

`TokenMetrics.scala` already records usage and ships to VictoriaMetrics. Add
dimensions per run:

`phase`, `runner`, `model`, `turnCount`, `inputTokens`, `cachedInputTokens`,
`outputTokens`, `escalated`, `outcome`.

Every stage below is a bet. Without turn count and cache-hit ratio, it is impossible
to tell which bet paid. Do not skip.

### Stage 1 — Escalation ladder (biggest win, smallest diff)

Generalize `routeRunnerFallback` from `Throwable` to a `VerificationResult`:

```
selectRunner(cheapest capable)
  -> run -> compile + test (deterministic, free)
       -> green: done
       -> red:   nextStrongerImplementor, fresh session, seeded with the error
                 artifact only (not the failed attempt's transcript)
```

- Touch: `Implementations.scala:860`, `AgentInventory.selectRunnerFor`, `RepairLoop`.
- Bias `selectRunnerFor` *down* a tier, now that failure is caught empirically rather
  than predicted.
- Cap ladder depth (2 escalations), then escalate to human.
- Update `PROJECT.md`: replace "aim low, stop at the capability floor" (prediction)
  with "start at the floor, escalate on red" (measurement).

**When to bias down — the break-even.** A wasted cheap attempt costs real tokens, so
biasing tiers down is not unconditionally correct. With `c` = cheap runner cost,
`s` = strong runner cost, `p` = cheap tier's success rate on this phase:

```
E[ladder]  = c + (1-p)·s
E[direct]  = s
ladder wins  <=>  c + (1-p)·s < s  <=>  c < p·s  <=>  p > c/s
```

**The break-even success rate is the price ratio.** If the cheap runner is 20× cheaper,
`p* = 5%` and the ladder wins almost unconditionally. If it is only 2× cheaper,
`p* = 50%` and biasing down loses money whenever the cheap tier succeeds less than half
the time.

`c` and `s` are already available: `AgentTool.inputUsdPerMTok` / `outputUsdPerMTok`
(`AgentInventory.scala:30-31`). The missing term is `p`, measured per
`(phase, runner)` — precisely what Stage 0's `escalated` / `outcome` / `runner`
dimensions produce. Once both exist, `selectRunnerFor` stops predicting capability and
instead picks the cheapest candidate satisfying `p > c/s`.

Three effects push the real break-even above the naive formula, so keep margin:

1. **Escalation carries residue.** The strong run is seeded with the error artifact, so
   it costs slightly more than a cold strong run.
2. **Dirty worktree.** A failed cheap attempt leaves partial edits. Hard-reset the
   worktree before escalating, or the strong runner inherits garbage and pays to
   untangle it.
3. **False green.** The cheap model passes tests without solving the problem — the
   worst outcome, since it "saves" tokens by shipping wrong code. Guard: the
   implementer must not be permitted to edit tests.

**Poisoned context:** never repair inside the failed session. The wrong code stays in
the transcript and the model keeps re-reading its own mistake. Fresh session, error
artifact only.

### Stage 2 — Turn-count reduction

Cost is ~O(turns²), so this is the largest per-run lever after tiering.

- Hard turn cap per leaf; exceeding it fails into escalation rather than grinding.
- Richer initial prompt: exact file paths and symbol names taken from the parent's
  artifact. Never "go find it."
- Audit tool ergonomics — anywhere an agent needs 3 calls to accomplish one thing.

### Stage 3 — Force `scala-semantic` MCP on subagents

The largest single component of Stage 2 for this repo.

- Inject a **mandatory** block into every Scala-touching subagent prompt: symbol,
  type, signature, hierarchy, implicit, reference and call-path questions go through
  `mcp__scala-semantic__*` — never `cat`, `rg`, or a full file read.
- Ensure `.semanticdb` is fresh before dispatch: run `scripts/refresh-semanticdb.sh`
  as a pre-step in the arrow, memoised on a source hash so it is not rerun per leaf.
- Ratio being bought: `document_outline` on a large file ≈ 300 tokens vs. reading
  `Implementations.scala` (64 KB ≈ 16k tokens).
- **Enforce, don't suggest.** Add a metric counting `rg`/`cat` invocations against
  `*.scala` in subagent transcripts. Unenforced prompt instructions get ignored;
  without the metric there is no way to know.

### Stage 4 — Context = pointers, not payload

- Audit every prompt-assembly site in `EvaluationArrows.scala` and
  `Implementations.scala` for pasted file bodies. Replace with paths, symbol names and
  diff stats.
- Codify the node output contract: a task's artifact is a **bounded** structured
  summary — decision, files touched, symbols, follow-ups. Minimal but sufficient for
  the child to proceed without the parent's transcript.
- Enforce the bound. An unbounded "summary" becomes a transcript.

### Stage 5 — Remove the model from deterministic steps

Every deterministic step currently performed by a model is 100% waste, permanently.
Audit the arrows for: conflict detection, test running, lint/format, PR templating,
metadata parsing, runner selection, branch hygiene.

### Stage 6 — Prefix ordering and sibling caching

Smallest lever, cheapest to obtain. Do it last.

Layer every assembled prompt most-stable-first, since caching is prefix-only:

```
[0] system + tool definitions      <- never changes
[1] repo map / conventions         <- per repo
[2] source-of-truth phase output   <- per task tree     <- cache breakpoint
[3] parent artifact summary        <- per branch
[4] this task's instruction        <- volatile
```

- Set the **1-hour** cache TTL on the shared prefix at fan-out points (~2× write,
  break-even ~3 reads).
- Fan out siblings from one cached prefix, same model, **launched together**.
  Staggering loses the window.
- Never share one live session across parallel siblings: it forces serialisation via a
  mutex (destroying the parallelism) and cross-pollutes their contexts.
- Live process reuse is worth it only inside a repair loop on a single runner.
- Skip `IOLocal`. Carry `PrefixKey(runner, model, worktree, stablePrefixHash)` in the
  arrow payload as data, so `TokenMetrics` can attribute cache hits. Ambient
  fiber-local state is invisible to metrics and to tests; it can be added later as a
  shim over the explicit version, but not the reverse.

Cats Effect stays responsible for what it is good at — `Resource` for process
lifecycle, `Mutex` per session, cancel-safe teardown, `uncancelable` around a single
turn (a turn cancelled midway leaves the session in an unknown state; mark it poisoned
rather than reusing it). Do not rebuild that layer.

## 3. Expected savings

| Stage | Rough | Confidence |
|---|---|---|
| 1 escalation ladder | 30–60% | high — the verifier is free |
| 2+3 turns + scala-semantic | 30–50% | high |
| 4 pointers not payload | 20–40% | medium |
| 5 de-LLM deterministic steps | varies, permanent | high |
| 6 prefix ordering + caching | 10–20% | medium — TTL/invalidation fragile |

## 4. Implementation discipline — use the right tools

### Use `scala-semantic` MCP, including while implementing this plan

Applies to this work, not only to the subagents it configures. For any question about
symbols, types, signatures, hierarchies, implicits, references or call paths in
`.scala` files, use `mcp__scala-semantic__*` rather than `cat`/`rg`/`sed`. It is both
more accurate (text search misses renames, re-exports, inferred and implicit uses, and
over-matches comments and strings) and dramatically cheaper.

- `structure` — where to start, what is central, dependency cycles
- `document_outline` — a file's API without reading it
- `find_usages` — every resolved reference, with `contextLines`
- `call_path` — whether A reaches B
- `resolve_implicits` / `trace_implicit_chain` — given resolution

Refresh with `scripts/refresh-semanticdb.sh` before relying on the index; restart the
MCP session if it still returns a stale outline.

### Prefer principled abstractions over ad-hoc traversal

The task tree is the core data structure of this plan, and several stages are
fundamentally *tree algebras*:

- Cost estimation = a fold (catamorphism) over the task tree.
- Phase decomposition = an unfold (anamorphism) from a task spec.
- Decompose-then-cost in one pass = a hylomorphism.
- Escalation rewrites a node in place and re-folds = a paramorphism / apomorphism.
- `PrefixKey` assignment is an accumulating top-down pass — a natural fold with an
  inherited attribute.

Writing these as hand-rolled recursion will produce five subtly different traversals
that drift. Prefer **recursion schemes**: define the tree as a pattern functor
`TaskF[A]` plus `Fix`/`Cofree`, and express each stage as an algebra. The traversal is
then written once; each stage supplies only its algebra.

- Library: **Droste** (`higherkindness/droste`) — supports Scala 3. Matryoshka is
  Scala 2 only and is not an option.
- `Cofree[TaskF, Attr]` is the natural carrier for annotating each node with its
  computed `PrefixKey`, tier, and cost estimate while preserving the tree shape.

The existing stack already leans this way — `arrowstep`, Cats, Kleisli arrows. Keep
new code in that idiom rather than introducing imperative traversal beside it.

### Make the cost model executable

Cost estimation should be a real function over the annotated tree, not a comment.
Once cost is an algebra, the scheduling questions become optimisation over that
algebra: where to fan out, which prefix to cache, whether to escalate. Stage 0's
metrics supply the coefficients; the fold turns them into a decision. Property-test
the algebra (munit is already in the build) — e.g. adding a sibling to a fan-out must
never increase estimated per-node cost.

### Follow the repo's Scala rules

`SCALA_SEMANTIC_RULES.md` and `scala-rules.md` apply. Compile before relying on
semantic tooling; shell tools remain correct for builds, tests, git, config and docs.
