#!/usr/bin/env sh
set -eu

repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
cd "$repo_root"

rm -rf \
  .semanticdb \
  .semanticdb-main-only \
  .semanticdb-scripts \
  .semanticdb-target \
  .semanticdb-target-no-directive \
  semanticdb-abs-out \
  semanticdb-out

rm -f ./*.semanticdb

# --test is not optional. Without it scala-cli compiles the main scope only, so
# every *.test.scala file is missing from the index - and a missing file does
# not read as missing, it reads as "no usages". find_usages then under-reports
# every symbol whose only remaining callers are tests, which is exactly the
# evidence a "nothing uses this, delete it" decision rests on.
scala-cli compile . --test \
  --semanticdb \
  --semanticdb-sourceroot . \
  --semanticdb-targetroot .semanticdb \
  --server=false

indexed=$(find .semanticdb -name '*.semanticdb' | wc -l | tr -d ' ')
sources=$(find . -name '*.scala' \
  -not -path './.worktrees/*' \
  -not -path './.scala-build/*' \
  -not -path './.semanticdb/*' | wc -l | tr -d ' ')

printf 'SemanticDB refreshed in %s (%s indexed / %s sources)\n' \
  "$repo_root/.semanticdb" "$indexed" "$sources"

if [ "$indexed" -lt "$sources" ]; then
  printf 'WARNING: %s source file(s) are not in the index; semantic answers about them will be silently incomplete.\n' \
    "$((sources - indexed))" >&2
fi

printf 'Now call the ScalaSemantic MCP tool refresh_workspace - the server caches the index in memory and will keep serving the old one otherwise.\n'
