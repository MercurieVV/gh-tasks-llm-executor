# Postmortem — the Stage 0–6 run closed 44 issues and delivered about half of them

Date: 2026-07-31
Scope: the `#33` token-efficiency task tree (T01–T20), executed 2026-07-28 → 07-30
Related: `TOKEN_EFFICIENCY_TASKS.md`, `ADR-0001-prompt-cache-knobs.md`

## What happened

The executor ran to completion. By 2026-07-30 14:16 all 44 issues were closed and
25 PRs were merged, off 1338 agent dispatches. Nothing errored out, nothing was
left open, every merge was green.

From 2026-07-30 15:11 onward, every commit on `master` is a human re-doing the
same task numbers by hand: T04, T08, T09/T10/T11/T16, T13, T15, T19, T20.

So the failure is not "the executor did not run" and not "the executor could not
merge". It is that **a merged green PR was not evidence the task was done**, and
nothing in the loop could tell the difference. That is the thing to fix — the
run itself is repeatable, the acceptance signal is not trustworthy.

## The four ways a task closed without being done

### 1. Acceptance criteria stated structure, so structure is what was delivered

T04 (#37) required:

> Every `TokenMetricsEvent` construction site passes both `phase` and `runner`.
> `scala-cli test .` green.

The implementer passed `phase = metricsScope`. Both clauses hold. But
`metricsScope` carries scope values (`"implement"`, `"merge-repair"`), not the
`Phase:` vocabulary that `TokenMetricsBackend.successRate(phase, runner)` is
keyed on, so the break-even selector had nothing to read. Merged 07-29 20:33;
repaired by hand in `dedeafb` at 07-30 15:11, nineteen hours during which every
recorded event was unusable.

A criterion that a wrong variable of the right type can satisfy is not a
criterion. The fix is to name the value set, not the parameter:
`phase ∈ {plan, source-of-truth, implement, test}`.

### 2. A scope item with no matching acceptance criterion disappears in silence

T20 (#53) listed three scope items. The third:

> 3. Ensure siblings are launched together, not staggered.

Acceptance listed two assertions, both about the TTL flag. Items 1–2 were
implemented and tested; item 3 was not implemented at all, and nothing failed.
It was written by hand on 07-31 as `CachePeers.groupAdjacent` (`55804f1`),
twelve hours after the task closed.

The issue's own Notes/Risks section named the exact failure — "staggered sibling
launches lose the cache window, converting the 2x write premium into pure loss" —
and prose in the issue body stops nothing. Only an acceptance criterion does.

### 3. The `## Files` allowlist was wrong, and being wrong was free

T04 declared `Implementations.scala (recording sites only)`, followed by "Only
these files may be modified. Touching others means the task was misread." The
actual `TokenMetricsEvent` construction sites are in `AgentExecutor.scala`.

The implementer ignored the list and edited the right files — the correct call.
But that means the guard is decorative: violating it costs nothing, and obeying
it would have made the task impossible. A rule that must be broken to succeed is
worse than no rule, because it teaches the model to discount the whole section.

### 4. Write side and read side landed in separate tasks; nothing tested the round trip

T02 (#35) exported `phase`, `runner`, `turn_count`, `escalated` and `outcome` as
OTel attributes. No task owned reading them back, and the VictoriaMetrics reader
dropped all five, so `successRate` returned `None` on the default backend
permanently. Both tasks green, neither wrong on its own terms.

Decomposition creates these boundaries and does not create anything that spans
them.

## The dispatch layer also wasted most of the run

1153 of 1297 `aider` dispatches (89%) ended:

```
litellm.BadRequestError: DeepseekException - {"error":{"message":"Insufficient Balance"...
```

The same exhausted vendor was re-dispatched 1153 times. Fixed after the fact by
`3af6ad4` plus the `hardExhausted` exclusion in `AgentInventory`.

Three other safety mechanisms also post-date every merge in this run and have
therefore never been exercised against it:

- `a4c0b0f` — the verifier now runs *inside* the escalation loop. Before it, a
  failed compile or test raised past the ladder and killed the task instead of
  escalating it.
- `2cfd677` — the post-publication repair loop now rotates runners. Before it,
  the loop re-ran the runner that had just failed, three times over.
- `7ee6777` — `TestEditGuard`. Landed 07-30 18:18, after the last merge.

## What has to change before the next run

Ordered by leverage.

1. **Author the criterion tests in a `Phase: test` task that lands before
   `implement`.** `TestEditGuard` already forbids an implementer from modifying
   an existing test while allowing it to add one — which is exactly backwards
   against self-certification, because "write the test that judges you" is the
   permitted half. Ordering the test task first is what gives the guard teeth.
   This inverts the current `plan → source-of-truth → implement → test`
   dependency order for criterion tests specifically.
2. **Require 1:1 scope-to-acceptance.** The evaluator should reject its own
   decomposition when a scope item has no criterion. T20 item 3 died precisely
   in that gap.
3. **State acceptance over values, not over presence.** See failure 1.
4. **Emit a round-trip task whenever two tasks touch opposite ends of a
   serialisation boundary**, depending on both. See failure 4.
5. **Enforce `## Files` or delete the section.** Preferably derive it from
   ScalaSemantic `find_usages` at evaluation time instead of having the model
   guess it, since a guessed list is what made it wrong here.
6. **Keep the measurement dimensions visible during the run.** `renderEvents`
   showed none of the five until 07-31 and `metrics readiness` did not exist.
   1338 dispatches with no in-flight view of phase/runner/outcome is
   unauditable while it is still possible to intervene.

## Next action

One executor run on current `master`. Every remedy listed above landed after the
final dispatch of the run being analysed, so none of them — escalate-on-red
inside the loop, runner rotation, `TestEditGuard`, `hardExhausted`, labelled
metrics, the readiness view — has met reality yet. Until then this document
describes fixes that are believed to work, not fixes known to work.
