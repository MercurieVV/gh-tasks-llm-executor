## Scala Code Rules
@SCALA_SEMANTIC_RULES.md
@scala-rules.md)

## Phase-typed task decomposition

The evaluator (`evaluateTaskPrompt`, `EvaluationArrows.scala`) may split a task into
phase-typed subtasks drawn from `{plan, source-of-truth, implement, test}`, each
tagged with a `Phase:` line in its `Task metadata` block and dependency-ordered
`plan -> source-of-truth -> implement -> test`. Phase decomposition composes with
scope split: a scope-piece may itself carry a `Phase:`. Bias toward FEWER phases —
a trivial, fully-specified task stays a single `implement` (or no split); never
force 4-phase overhead.

## Runner selection: start at the floor, escalate on red

Selection is **measured, not predicted**. Earlier guidance here said to predict the
capability a leaf needs and to treat under-powering as the primary failure mode.
That advice was written before the verifier was wired in, and it is now wrong in
both halves: prediction is unnecessary when a free verifier can answer the same
question empirically, and an under-powered leaf is no longer terminal because it
escalates.

`AgentInventory.selectRunnerFor` takes the cheapest available implementor and keeps
it only when its measured success rate beats the price ratio:

```
c = cheap runner cost, s = next stronger cost, p = observed success rate
E[ladder] = c + (1-p)·s   <   E[direct] = s   <=>   p > c/s
```

`AgentTool.breakEvenRateAgainst` supplies `c/s`;
`TokenMetricsBackend.successRate(phase, runner)` supplies `p`, per phase, from
recorded outcomes. Either being absent — an unpriced tool, or fewer than
`minSample = 20` recorded runs — falls back to the existing `Priority.score`
ordering unchanged. An unmeasured pair is never read as a 100% success rate.

`c` and `s` are **costs, not prices**. `AgentTool.costWith` multiplies each
tool's per-token price by the volume that `(phase, runner)` is measured to
consume (`TokenMetricsBackend.meanUsage`, same `minSample = 20`), falling back
to the assumed 20k in / 4k out when unmeasured. This matters because `c/s`
divides two of them: with assumed volumes on both sides the volumes cancel and
the ratio degenerates to list price, which is only the cost ratio if both
runners spend the same tokens on the same job. A smaller model that needs more
turns spends its discount on volume. Measurement is taken on **both** sides or
neither — a measured cheap runner compared against an assumed strong one would
favour whichever side happened to have a sample. The same rule picks *which*
runner is the cheap one: `cheapestImplementorToRun` orders candidates on
measured cost when every implementor is measured for that phase, and on the
assumed volumes otherwise.

Consequences worth knowing before changing this code:

- **Bias down, not up.** A wasted cheap attempt is now a bounded cost, so the
  20×-cheaper runner wins at `p > 5%`. Do not re-add capability prediction on top.
- **Escalation is the safety net.** The retried unit is
  `runTaskWithRunner.andThen(Impl.runProjectValidation)`, so the verifier runs
  INSIDE the loop — until 2026-07-31 it ran after it, and a failing compile or
  test raised past the ladder and killed the task instead of escalating it.
  Anything composed after the agent that can reject its work belongs inside the
  same loop, or the ladder cannot see the rejection.
  `BusinessLogicRetry.routeRunnerFallback`
  escalates on `VerificationResult.Red` and `Failed`, hard-resets the worktree
  first (the stronger runner must start from HEAD, not from half-finished edits),
  seeds the retry with the error artifact only — never the failed transcript — and
  stops at `MaxEscalationDepth = 2`, after which the task surfaces to a human.
- **The repair loops rotate too.** A red that arrives *after* publication — a
  rejected push, a failed CI check — is repaired rather than escalated: the
  branch is already pushed, so a hard reset is destructive. But the repair agent
  must still change runner between attempts. `repairRunnerSequence` precomputes
  `MaxRepairBuildCheckAttempts + 1` runners from `alternateImplementor`, staying
  on the last one when the inventory runs out. Until 2026-07-31 that loop re-ran
  the runner that had just failed, three times over.
- **The turn cap feeds the same path.** Exceeding `TurnCap` (default 25,
  overridable in `.gh-tasks-llm-executor/execution-limits.json`) raises
  `TurnCapExceeded`, which becomes a `Red` rather than a `Failed` — "could not
  finish" is not "the tool is broken".
- **Tests are not the implementer's to edit.** `TestEditGuard` rejects a run that
  modified, deleted or renamed an existing test file unless its phase is `test`,
  `plan` or `source-of-truth`. Adding a test is allowed. A task with no `Phase:` is
  guarded, not exempt. This is what keeps a green honest: without it, the cheapest
  path to a passing verifier is to weaken the verifier, and the resulting `"green"`
  would feed `successRate` and bias selection toward the runner that cheated.
  A task in this repo that legitimately must change a test needs `Phase: test`.

The tier table below is the **starting prior** for a phase with no measurements
yet, not a floor to be defended:

| Phase | Required capability | Starting tier |
|---|---|---|
| plan | strong reasoning / decomposition | high |
| source-of-truth | judgment on authority / spec | high–medium |
| implement | narrow, well-specified code change | medium (task-dependent); cheap only when genuinely trivial + fully specified |
| test | narrow verification | low–medium; cheapest capable |

Concrete runner ids come from whatever
`.gh-tasks-llm-executor/agent-runners.json` declares (generated by
`scripts/discover-agent-runners.scala`). Phase names are seeded into each runner's
`jobTypes`/`strengths`, and `priority` orders cheapest-capable first, so
`AgentInventory.selectRunner` / `GitHub.taskRunners` match tier/fit automatically.
Selection is model-agnostic — the cheapest leaf runner may be `codex/*` or
`gemini/*`, never hardcode `claude/*`.

## Writing a task issue the executor can actually be held to

`POSTMORTEM-2026-07-31-gh-task-execution.md` analyses a run that closed 44 issues
and merged 25 PRs while delivering about half the work. Every one of those tasks
passed its own acceptance criteria. Read it before writing task specs; the four
recurring defects it documents are cheap to avoid and invisible once merged:

- **Acceptance must constrain values, not shape.** "passes `phase`" was satisfied
  by passing the wrong variable of the right type. Name the value set.
- **Every scope item needs its own criterion.** An item with none is silently
  dropped and the task still closes green.
- **`## Files` is either enforced or deleted.** It was guessed, it was wrong, and
  succeeding required ignoring it.
- **A write side and a read side in two tasks need a third that crosses them.**
  Both halves can be individually correct and jointly useless.
