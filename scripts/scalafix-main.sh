#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

RULE_COORD="io.github.mercurievv:scala-purrism-scalafix_3:0.1.0"
IVY_RULE_JAR="$HOME/.ivy2/local/io.github.mercurievv/scala-purrism-scalafix_3/0.1.0/jars/scala-purrism-scalafix_3.jar"
M2_RULE_JAR="$HOME/.m2/repository/io/github/mercurievv/scala-purrism-scalafix_3/0.1.0/scala-purrism-scalafix_3-0.1.0.jar"

rtk scripts/refresh-semanticdb.sh

TOOL_CP="$(rtk coursier fetch -p -r m2Local "$RULE_COORD")"
if [[ -f "$M2_RULE_JAR" ]]; then
  TOOL_CP="${TOOL_CP//$IVY_RULE_JAR/$M2_RULE_JAR}"
fi

rtk coursier launch scalafix:0.14.7 -- \
  --config .scalafix.conf \
  --rules OrganizeImports \
  --rules DisableSyntax \
  --rules LeakingImplicitClassVal \
  --rules NoValInForComprehension \
  --rules class:fix.TypelevelPurrism \
  --tool-classpath "$TOOL_CP" \
  --sourceroot "$ROOT" \
  --semanticdb-targetroots "$ROOT/.semanticdb" \
  --scala-version 3.8.4 \
  --scalac-options -Wunused:imports \
  --files AgentExecutor.scala \
  --files AgentInventory.scala \
  --files ArrowLogging.scala \
  --files BusinessLogic.scala \
  --files Git.scala \
  --files IssueClaim.scala \
  --files TaskLogger.scala \
  --files github.scala \
  --files main.scala \
  --files taskMetadata.scala
