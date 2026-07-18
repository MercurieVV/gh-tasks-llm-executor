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

If SemanticDB tools report an empty index, verify generation with:
`find .semanticdb -name '*.semanticdb'`

If ScalaSemantic still returns an old outline after refresh, the running MCP
server has cached the previous root index. Restart the MCP/client session so it
loads the refreshed `.semanticdb` files.
