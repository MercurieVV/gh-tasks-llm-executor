// Directives-only file for the raw-URL scala-cli launch (see README.md).
// project.scala's `//> using exclude ...` directives are directory-scoped
// and break scala-cli when every input is a remote URL, so this file
// mirrors only the scala-version/dependency directives it needs.
//> using scala 3.8.4
//> using dep org.typelevel::cats-core:2.13.0
//> using dep org.typelevel::cats-effect:3.7.0
//> using dep io.github.mercurievv::arrowstep:0.1.1
//> using dep com.lihaoyi::os-lib:0.11.8
//> using dep com.lihaoyi::ujson:4.4.3
