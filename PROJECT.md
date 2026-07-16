## Scala Code Rules
@SCALA_SEMANTIC_RULES.md
@scala-rules.md)

## Phase-typed task decomposition

The evaluator (`evaluateTaskPrompt`, `main.scala`) may split a task into
phase-typed subtasks and route each *kind of work* to the cheapest **capable**
runner. Phase typing is optional and composes with the existing scope-based
split — it does not replace it. A task with no `Phase:` metadata runs exactly as
before.

Phases and their dependency order: `plan` → `source-of-truth` → `implement` →
`test`. Each emitted phase is a subtask tagged with a `Phase:` line in its task
metadata and carries its own ranked `preferred llms/models/efforts/versions`
list (primary = cheapest-capable, fallbacks = stronger, so
`nextStrongerImplementor` can escalate).

Routing is by **capability tier mapped to the live inventory**
(`agentInventory.promptBlock` / `.gh-tasks-llm-executor/agent-runners.json`),
never hardcoded vendor ids — the best leaf runner may be `claude/*`, `codex/*`,
`gemini/*`, `aider/*`, etc.

| Phase           | Capability tier          | Rationale                                        |
|-----------------|--------------------------|--------------------------------------------------|
| plan            | high                     | strongest reasoning; sets scope + acceptance     |
| source-of-truth | high → medium            | names the authoritative reference for later work |
| implement       | medium (task-dependent)  | narrow, well-specified leaves drop toward cheapest|
| test            | low → medium             | mechanical once the oracle is fixed              |

Guidance the evaluator applies:

- Bias toward **fewer** phases. A trivial task stays a single `implement` (or
  plain `ready`) — never a forced 4-phase fan-out.
- `source-of-truth` must **name** the authoritative reference (spec /
  definition-of-done, a pinned artifact + version, or the test oracle) in its
  acceptance criteria so later phases conform.
- Actively **narrow** `implement`/`test` leaves until the cheapest tier (e.g.
  Haiku) can run them. When a leaf resists narrowing, set a capability floor from
  the residual risk and pick the cheapest runner **at or above** the floor. Never
  down-tier below the floor for price alone — under-powering a leaf produces wrong
  code → repair/replay → net token loss.
- Cross-phase context rides through issue bodies + the dependency conclusion
  comment. If that is insufficient for a cheap `implement` model, the `plan` /
  `source-of-truth` subtasks must write their output back into the issue
  explicitly.
