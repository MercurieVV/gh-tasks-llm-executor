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

scala-cli compile . \
  --semanticdb \
  --semanticdb-sourceroot . \
  --semanticdb-targetroot .semanticdb \
  --server=false

printf 'SemanticDB refreshed in %s\n' "$repo_root/.semanticdb"
