#!/usr/bin/env bash
# Run scalafix over the project.
#
# `scala-cli fix` passes no file list to scalafix, so scalafix walks the whole
# working directory. Files listed as `//> using exclude` in project.scala are
# never compiled, so they have no SemanticDB and fail with
# "error: SemanticDB not found: <file>". Mirror those excludes into scalafix.
#
# Globs are resolved relative to the current directory: `scripts/**` matches,
# `glob:**/scripts/**` does not.
#
# Usage:
#   ./scalafix.sh                 apply the rules enabled in .scalafix.conf
#   ./scalafix.sh --list          print the enabled rules, then exit
#   ./scalafix.sh --per-rule      dry-run each enabled rule on its own and report
#                                 which ones would rewrite something
#   ./scalafix.sh <args...>       any other args go to `scala-cli fix`
#                                 (e.g. --check, --scalafix-arg=--verbose)
set -euo pipefail

cd "$(dirname "${BASH_SOURCE[0]}")"

# Excludes, taken from project.scala so there is one list to maintain.
exclude_args=()
while IFS= read -r path; do
  [[ -z "$path" ]] && continue
  # Directories need a trailing /** to cover their contents.
  [[ -d "$path" ]] && path="${path%/}/**"
  exclude_args+=(--scalafix-arg=--exclude --scalafix-arg="$path")
done < <(sed -n 's|^//> using exclude *||p' project.scala)

# Rules enabled in .scalafix.conf: the `rules = [ ... ]` block, minus `//` comments.
enabled_rules() {
  sed -n '/^rules *= *\[/,/^]/p' .scalafix.conf |
    sed 's|//.*||' |
    tr -d ' ,' |
    grep -vE '^(rules=\[|\])$' |
    grep .
}

# Compiler warnings/hints from the build step drown out scalafix's own output.
drop_build_noise() {
  # scala-cli colours its log prefixes, so strip ANSI escapes before matching.
  sed $'s/\033\\[[0-9;]*m//g' |
    grep -vE '^\[(warn|hint)\]|^ +\^|^WARNING:|^Some utilized features|^ - |^Please bear in mind|^If you encounter' || true
}

case "${1:-}" in
--list)
  enabled_rules
  exit 0
  ;;
--per-rule)
  status=0
  while IFS= read -r rule; do
    echo "=== $rule"
    if scala-cli --power fix . --scalafix "${exclude_args[@]}" \
      --scalafix-rules="$rule" --check 2>&1 | drop_build_noise |
      grep -vE '^Running |^Built-in rules completed|^Skipping,' | grep . ; then
      status=1
    else
      echo "no changes"
    fi
  done < <(enabled_rules)
  exit "$status"
  ;;
esac

exec scala-cli --power fix . --scalafix "${exclude_args[@]}" "$@"
