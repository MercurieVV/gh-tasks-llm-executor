package com.github.mercurievv.ghllm.arrow

import com.github.mercurievv.ghllm.*

class TestEditGuardSuite extends munit.FunSuite:

  private def body(phase: String) = IssueBody(s"Task metadata:\nPhase: $phase")

  test("the repo's own test conventions are all recognised"):
    // Every one of these is a real path shape in this repository. Missing any
    // of them is a hole in the guard, not a cosmetic gap.
    List(
      "src/main/scala/com/github/mercurievv/ghllm/task/NodeProfiles.test.scala",
      "src/test/scala/com/github/mercurievv/ghllm/arrow/FanOutCacheTest.scala",
      "TaskTreeTest.test.scala",
      "Foo.test.scala",
      "FooSuite.scala",
      "FooSpec.scala"
    ).foreach(path => assert(TestEditGuard.isTestFile(path), path))

  test("production sources are not mistaken for tests"):
    List(
      "src/main/scala/com/github/mercurievv/ghllm/task/NodeProfiles.scala",
      "Implementations.scala",
      "TaskTree.scala",
      "scripts/remote-run.sh",
      "README.md"
    ).foreach(path => assert(!TestEditGuard.isTestFile(path), path))

  test("path matching survives case and separator variation"):
    assert(TestEditGuard.isTestFile("SRC/TEST/scala/Foo.scala"))
    assert(TestEditGuard.isTestFile("src\\test\\scala\\Foo.scala"))
    assert(TestEditGuard.isTestFile("  Foo.test.scala  "))

  test("an implement run that modifies an existing test is a violation"):
    assertEquals(
      TestEditGuard.violations(Some("implement"), List("Foo.scala", "Foo.test.scala")),
      List("Foo.test.scala")
    )

  test("a task with no declared phase is still guarded"):
    // Unsplit tasks are the common case; exempting them would leave the guard
    // covering only the runs that were already labelled.
    assertEquals(TestEditGuard.violations(None, List("Foo.test.scala")), List("Foo.test.scala"))

  test("the test phase owns the tests and is exempt"):
    assertEquals(TestEditGuard.violations(Some("test"), List("Foo.test.scala")), Nil)
    assertEquals(TestEditGuard.violations(Some(" TEST "), List("Foo.test.scala")), Nil)

  test("plan and source-of-truth are exempt"):
    assertEquals(TestEditGuard.violations(Some("plan"), List("Foo.test.scala")), Nil)
    assertEquals(TestEditGuard.violations(Some("source-of-truth"), List("Foo.test.scala")), Nil)

  test("a run that only touches production code passes"):
    assertEquals(TestEditGuard.violations(Some("implement"), List("Foo.scala", "Bar.scala")), Nil)

  test("violations are deduplicated and ordered so the message is stable"):
    assertEquals(
      TestEditGuard.violations(Some("implement"), List("b.test.scala", "a.test.scala", "b.test.scala")),
      List("a.test.scala", "b.test.scala")
    )

  test("phase is read from task metadata, normalised"):
    assertEquals(TestEditGuard.phaseOf(body("Implement")), Some("implement"))
    assertEquals(TestEditGuard.phaseOf(body("  test  ")), Some("test"))
    assertEquals(TestEditGuard.phaseOf(IssueBody("no metadata here")), None)

  test("the report names every offending file"):
    val report = TestEditGuard.report(Some("implement"), List("a.test.scala", "b.test.scala"))

    assert(report.contains("a.test.scala"), report)
    assert(report.contains("b.test.scala"), report)
    assert(report.contains("implement"), report)
