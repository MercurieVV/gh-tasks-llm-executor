#!/usr/bin/env bash
# One-liner web launcher: fetches every source file fresh off `master` (or
# GH_TASKS_REF) and runs scala-cli against them. scala-cli caches/updates the
# JVM deps itself; this script only needs to keep the source-file list current.
#
#   curl -fsSL https://raw.githubusercontent.com/MercurieVV/gh-tasks-llm-executor/master/scripts/remote-run.sh | bash -s -- --task=263
set -euo pipefail

REF="${GH_TASKS_REF:-master}"
BASE="https://raw.githubusercontent.com/MercurieVV/gh-tasks-llm-executor/${REF}"

FILES=(
  project-remote.scala
  main.scala
  BusinessLogic.scala
  Priority.scala
  src/main/scala/com/github/mercurievv/ghllm/agent/AgentExecutor.scala
  src/main/scala/com/github/mercurievv/ghllm/agent/AgentInventory.scala
  src/main/scala/com/github/mercurievv/ghllm/agent/AgentRunnersDiscovery.scala
  src/main/scala/com/github/mercurievv/ghllm/arrow/ArrowCapabilities.scala
  src/main/scala/com/github/mercurievv/ghllm/arrow/ArrowLogging.scala
  src/main/scala/com/github/mercurievv/ghllm/cli/Cli.scala
  src/main/scala/com/github/mercurievv/ghllm/arrow/EvaluationArrows.scala
  src/main/scala/com/github/mercurievv/ghllm/git/Git.scala
  src/main/scala/com/github/mercurievv/ghllm/git/github.scala
  src/main/scala/com/github/mercurievv/ghllm/arrow/Implementations.scala
  src/main/scala/com/github/mercurievv/ghllm/git/IssueClaim.scala
  src/main/scala/com/github/mercurievv/ghllm/arrow/ParallelArrows.scala
  src/main/scala/com/github/mercurievv/ghllm/cli/RunEnv.scala
  src/main/scala/com/github/mercurievv/ghllm/metrics/TaskLogger.scala
  src/main/scala/com/github/mercurievv/ghllm/metrics/TokenMetrics.scala
  src/main/scala/com/github/mercurievv/ghllm/metrics/TokenUsage.scala
  src/main/scala/com/github/mercurievv/ghllm/metrics/VendorBudgets.scala
  src/main/scala/com/github/mercurievv/ghllm/arrow/Wiring.scala
  src/main/scala/com/github/mercurievv/ghllm/cli/taskMetadata.scala
)

urls=()
for f in "${FILES[@]}"; do
  urls+=("$BASE/$f")
done

exec scala-cli run "${urls[@]}" -- "$@"
