# ADR-0001 — Prompt-cache control is per-vendor, and mostly not ours

Status: accepted, 2026-07-30
Context: `TOKEN_EFFICIENCY_PLAN.md` §2 Stage 6 / T20

## Context

Stage 6 assumes prompt caching is something this executor configures. It mostly is
not. Every runner is invoked as a subprocess — `TaskRunner.command` builds an argv,
`AgentExecutor` runs it — and the vendor CLI constructs the actual API request. What
we can influence is limited to whatever flags and environment variables that CLI
exposes, which differ per vendor and are not discoverable from the plan.

The five installed CLIs were checked directly (`--help`, plus the binary's strings for
`codex`):

| Agent | Knob | Effect |
|---|---|---|
| `claude` | `ENABLE_PROMPT_CACHING_1H=1` (env) | extends the cache window from 5 minutes to 1 hour |
| `claude` | `--exclude-dynamic-system-prompt-sections` | moves cwd / env info / memory paths / git status out of the system prompt into the first user message |
| `aider` | `--cache-prompts` (`AIDER_CACHE_PROMPTS`) | enables prompt caching **at all**; the CLI default is off |
| `aider` | `--cache-keepalive-pings N` | pings every 5 min to keep a cache warm |
| `codex` | none | OpenAI caches server-side, automatically, with no client control |
| `gemini` | none | implicit caching is provider-side |
| `agy` | none | no cache-related flag exists |

Two of these were being left on the table.

## Decision

**Pass `--cache-prompts` to `aider`.** This is not an optimisation on top of caching —
it *is* caching. Without it every `aider` run re-sent the entire prefix at full price,
which is a larger miss than the 1-hour-versus-5-minute distinction T20 is about, and it
went unnoticed because the plan framed the whole area as "TTL tuning".

**Pass `--exclude-dynamic-system-prompt-sections` to `claude`.** Each task runs in its
own git worktree, so cwd and git status differ for *every* sibling. Those sections sit
in the system prompt, ahead of everything `Impl.taskPrompt` orders stable-first for
T19. Caching is prefix-only, so a shared prefix that begins after an already-divergent
segment is not shared at all — this flag is what makes the T19 ordering reachable.

**Leave `--cache-keepalive-pings` at 0.** It pays for an interactive session that goes
quiet; these runs are single-shot, so pings would be spend with no reader.

**Do not chase `codex`, `gemini` or `agy`.** They expose nothing. Marking their runners
as cache peers is a silent no-op, not a saving — `TaskRunner.invocationEnvironment`
returns an empty map for them and should be read as "nothing to do here", not as an
oversight.

## Consequences

Both flags are **opt-out, defaulting on**:

- `GH_TASKS_CLAUDE_STABLE_SYSTEM_PROMPT=0`
- `GH_TASKS_AIDER_CACHE_PROMPTS=0`

An unrecognised CLI flag is a hard failure, not a warning, and `scripts/remote-run.sh`
deploys this to machines whose CLI versions this repo does not control. The escape
hatch exists so an older `claude` there can be worked around without a code change.
`--exclude-dynamic-system-prompt-sections` is also documented as ignored when
`--system-prompt` is passed; we never pass it, so it applies.

The claude flag moves machine context from the system prompt into the first user
message. It does not remove that context, so an agent that relies on knowing its cwd
still gets it — but if a future change adds `--system-prompt`, this flag silently stops
doing anything, and the T19 ordering quietly stops paying off. That is the failure mode
to watch for.

Since 2026-07-31 it is watched by a test rather than by this paragraph:
`CacheFlagSuite` asserts that `claude`'s command line carries
`--exclude-dynamic-system-prompt-sections` and does **not** carry `--system-prompt`.
Adding `--system-prompt` remains a legitimate design choice — the test exists so that
choice is made deliberately, with this trade-off in view, instead of voiding the flag by
accident while every other test still passes.

## Revisit when

- A vendor adds a client-side cache control (`codex` is the likely first).
- `TaskRunner.command` stops shelling out to CLIs in favour of a direct API client, at
  which point `cache_control` breakpoints become ours to place and this ADR is
  superseded rather than amended.
