package com.github.mercurievv.ghllm.metrics

class ScalaTextToolCallCountSuite extends munit.FunSuite:

  test("counts only shell text-tool calls that target Scala sources"):
    val transcript =
      """Starting run.
        |  $ cat src/main/scala/com/example/Foo.scala
        |    {"type":"tool_use","command": "rg -n TaskRunner src/main/scala/Bar.scala"}
        |  sed -i '' 's/a/b/' Models.scala
        |  cat README.md
        |  rg TokenMetrics docs/notes.md
        |Done.
        |""".stripMargin

    assertEquals(TaskLogger.scalaTextToolCallCount(transcript), 3L)

  test("an empty transcript counts zero"):
    assertEquals(TaskLogger.scalaTextToolCallCount(""), 0L)

  test("prose mentioning a Scala file is not a tool call"):
    // The metric drives a judgement about runner behaviour, so a runner merely
    // discussing a file must not inflate it.
    val transcript =
      """I will edit Models.scala next, using document_outline rather than cat.
        |The grep-like search in ripgrep would have read Models.scala directly.
        |""".stripMargin

    assertEquals(TaskLogger.scalaTextToolCallCount(transcript), 0L)
