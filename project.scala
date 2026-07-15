//> using scala 3.8.4
//> using options -Wunused:imports
// //> using options -Werror
//> using exclude Setup.scala
//> using exclude project-remote.scala
//> using exclude scripts
//> using exclude docs
//> using exclude mdoc-docs
//> using exclude website
//> using exclude build.mill
//> using exclude out
//> using exclude .scala-build
//> using exclude .bsp
//> using dep org.typelevel::cats-core:2.13.0
//> using dep org.typelevel::cats-effect:3.7.0
//> using dep io.github.mercurievv::arrowstep:0.1.1
//> using dep com.lihaoyi::os-lib:0.11.8
//> using dep com.lihaoyi::ujson:4.4.3
//> using test.dep org.scalameta::munit:1.3.3
//> using test.dep org.typelevel::munit-cats-effect:2.2.0
//> using test.dep org.typelevel::shapeless3-deriving:3.6.0
//> using test.resourceDir app/test/resources
// Stainless is intentionally not enabled yet: the compiler plugin coordinate is not
// published for Scala 3.8.4, so formal verification stays non-blocking.
