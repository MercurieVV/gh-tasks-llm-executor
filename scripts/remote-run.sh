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
  ArrowLogging.scala
  main.scala
  Git.scala
  github.scala
  IssueClaim.scala
  AgentExecutor.scala
  AgentInventory.scala
  TaskLogger.scala
  BusinessLogic.scala
  taskMetadata.scala
)

urls=()
for f in "${FILES[@]}"; do
  urls+=("$BASE/$f")
done

exec scala-cli run "${urls[@]}" -- "$@"
