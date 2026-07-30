# Scala Semantic Rules

For Scala source questions, use ScalaSemantic MCP tools before shell text tools. Preferably compile code before usage, then more ScalaSemantic functions can be used with better results.

Do not use `cat`, `sed`, `rg`, or similar tools to inspect `.scala` files for symbol, type, signature, hierarchy, implicit, reference, or call-path questions when ScalaSemantic tools are available.

Use shell for builds, tests, git, config, docs, scripts, and non-Scala text work.

## Scala CLI SemanticDB

For one-off compilation, pass Scala CLI's SemanticDB flags on the command line:

```bash
scripts/refresh-semanticdb.sh
```

This script removes stale generated SemanticDB outputs and recompiles into
`.semanticdb/META-INF/semanticdb`, the location used by ScalaSemantic for this
script-style repository. If compiling manually, include an explicit target root:

```bash
scala-cli compile . --semanticdb --semanticdb-sourceroot . --semanticdb-targetroot .semanticdb --server=false
```

For persistent per-file or per-project configuration, use Scala CLI directives:

```scala
//> using semanticdb
//> using semanticdbSourceroot .
```

Do not use raw scalac flags for this. Avoid:

```scala
//> using options -Ysemanticdb
//> using options -sourceroot:..
```

## Refreshing is two steps, and skipping the second is silent

`scripts/refresh-semanticdb.sh` writes new `.semanticdb` files. The MCP server
caches the index in memory and keeps serving the old one until told otherwise,
so **always follow the script with the `refresh_workspace` MCP tool**. A session
restart also works but is not required — `refresh_workspace` is the cheap fix.

This matters because a stale index does not fail, it lies plausibly:
`document_outline` returns an outline with line numbers off by however much the
file has moved and members that no longer exist, and `find_symbol` returns
`count: 0` for a symbol added since the last compile — which reads exactly like
"this symbol does not exist".

The same trap applies to coverage. `scala-cli compile .` is **main-scope only**,
so without `--test` every `*.test.scala` is absent from the index and
`find_usages` under-reports any symbol whose remaining callers are tests. The
script passes `--test` and prints an indexed/sources count; if it warns that
files are missing, treat "no usages" answers as unproven until they are indexed.
`scripts/*.scala` and `project-remote.scala` are standalone scala-cli scripts
outside the build and are expected to stay unindexed.

Verify generation directly with: `find .semanticdb -name '*.semanticdb' | wc -l`
