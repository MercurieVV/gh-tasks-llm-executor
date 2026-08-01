// Main
//> using scala 3.8.4
//> using options -Wunused:imports -Xmax-inlines:2048
//> using javaOptions -Dotel.java.global-autoconfigure.enabled=true

//> using dependency com.lihaoyi::os-lib:0.11.8
//> using dependency com.lihaoyi::ujson:4.4.3
//> using dependency io.github.mercurievv.minuscles::fields_names:0.1.0
//> using dependency io.github.mercurievv.minuscles::shapeless3-typeclasses:0.1.0
//> using dependency io.github.mercurievv::arrowstep:0.1.1
//> using dependency io.higherkindness::droste-core:0.10.0
//> using dependency io.opentelemetry:opentelemetry-exporter-otlp:1.64.0
//> using dependency io.opentelemetry:opentelemetry-sdk-extension-autoconfigure:1.64.0
//> using dependency org.typelevel::cats-core:2.13.0
//> using dependency org.typelevel::cats-effect:3.7.0
//> using dependency org.typelevel::otel4s-oteljava:1.0.1

//> using scalafixDependency io.github.mercurievv:scala-purrism-scalafix_3:0.6.2
//> using resourceDirs resources

//> using exclude .bsp
//> using exclude .scala-build
//> using exclude Setup.scala
//> using exclude build.mill
//> using exclude docs
//> using exclude mdoc-docs
//> using exclude out
//> using exclude project-remote.scala
//> using exclude scripts
//> using exclude src/main/scala/com/github/mercurievv/ghllm/arrow/Wiring.test.scala
//> using exclude website

// Test
//> using test.dependency io.higherkindness::droste-core:0.10.0
//> using test.dependency org.scalameta::munit-scalacheck:1.3.0
//> using test.dependency org.scalameta::munit:1.3.3
//> using test.dependency org.typelevel::munit-cats-effect:2.2.0
//> using test.dependency org.typelevel::shapeless3-deriving:3.6.0

