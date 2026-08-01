package com.github.mercurievv.ghllm.arrow

class AcceptanceCoverageSuite extends munit.FunSuite:

  // The shape T20 (#53) actually had: three scope items, two criteria, and the
  // uncovered item named only in prose. It closed green with item 3 missing.
  private val t20 =
    """## Scope
      |1. Add the TTL flag to the cache config.
      |2. Thread it through the dispatch call.
      |3. Ensure siblings are launched together, not staggered.
      |
      |## Acceptance criteria
      |- `CacheConfig.extendedTtl` is true when the flag is set.
      |- The dispatch passes `extendedTtl` unchanged.
      |
      |## Notes/Risks
      |- Staggered sibling launches lose the cache window.
      |""".stripMargin

  test("fewer criteria than scope items is a shortfall, and the report names the items"):
    val report = AcceptanceCoverage.shortfall(t20)
    assert(report.isDefined)
    assert(report.exists(_.contains("3 scope item(s) but only 2")))
    assert(report.exists(_.contains("Ensure siblings are launched together")))

  test("prose in Notes/Risks does not count as a criterion"):
    assertEquals(AcceptanceCoverage.of(t20).map(_.acceptance.size), Some(2))

  test("one criterion per scope item covers"):
    val body =
      """## Scope
        |- Add the flag.
        |- Thread it through.
        |
        |## Acceptance criteria
        |- `CacheConfig.extendedTtl` is one of {true, false} and defaults to false.
        |- `dispatch` receives the same value `CacheConfig` holds.
        |""".stripMargin
    assertEquals(AcceptanceCoverage.shortfall(body), None)

  test("more criteria than scope items is fine"):
    val body =
      """Scope:
        |- One change.
        |
        |Acceptance criteria:
        |- Criterion A.
        |- Criterion B.
        |""".stripMargin
    assertEquals(AcceptanceCoverage.shortfall(body), None)

  test("a scope section with no acceptance section at all is the worst case, not an exemption"):
    val body =
      """## Scope
        |- One change.
        |- Another change.
        |""".stripMargin
    assert(AcceptanceCoverage.shortfall(body).exists(_.contains("- (none)")))

  test("no scope section is out of contract, not in violation"):
    assertEquals(AcceptanceCoverage.shortfall("Just make the thing work.\n"), None)
    assertEquals(AcceptanceCoverage.of("Just make the thing work.\n"), None)

  test("bold and colon headings are recognised alongside markdown headings"):
    val bold =
      """**Scope**
        |- One change.
        |
        |**Acceptance criteria**
        |- One criterion.
        |""".stripMargin
    assertEquals(AcceptanceCoverage.shortfall(bold), None)
    assertEquals(AcceptanceCoverage.of(bold).map(_.scope.size), Some(1))

  test("Done when counts as acceptance"):
    val body =
      """## Scope
        |- One change.
        |
        |## Done when
        |- The named test passes.
        |""".stripMargin
    assertEquals(AcceptanceCoverage.shortfall(body), None)

  test("a section ends at the next heading"):
    val body =
      """## Scope
        |- One change.
        |
        |## Notes/Risks
        |- Not scope.
        |- Also not scope.
        |
        |## Acceptance criteria
        |- One criterion.
        |""".stripMargin
    assertEquals(AcceptanceCoverage.of(body).map(_.scope.size), Some(1))
    assertEquals(AcceptanceCoverage.shortfall(body), None)

  // Otherwise the cheapest way past the check is to indent, which is the same
  // failure mode as weakening a test to make the verifier green.
  test("nested bullets elaborate an item, they do not add criteria"):
    val body =
      """## Scope
        |- One change.
        |- Another change.
        |
        |## Acceptance criteria
        |- One criterion.
        |  - a detail of it.
        |  - another detail.
        |""".stripMargin
    assert(AcceptanceCoverage.shortfall(body).isDefined)
