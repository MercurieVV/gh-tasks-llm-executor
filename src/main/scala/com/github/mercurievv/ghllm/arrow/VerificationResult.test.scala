package com.github.mercurievv.ghllm.arrow

class VerificationResultSuite extends munit.FunSuite:
  test("isGreen is true only for Green"):
    assert(VerificationResult.Green.isGreen)
    assert(!VerificationResult.Red("summary", "detail").isGreen)
    assert(!VerificationResult.Failed(RuntimeException("error")).isGreen)

  test("escalationSeed is absent only for Green"):
    assertEquals(VerificationResult.Green.escalationSeed, None)
    assertEquals(
      VerificationResult.Red("summary", "detail").escalationSeed,
      Some("summary\n\ndetail")
    )
    assertEquals(
      VerificationResult.Failed(RuntimeException("error")).escalationSeed,
      Some("error")
    )

  test("asThrowable round-trips the original throwable for Failed"):
    val original = RuntimeException("boom")
    assertEquals(VerificationResult.Failed(original).asThrowable, original)
    assertEquals(
      VerificationResult.Red("summary", "detail").asThrowable.getMessage,
      "summary\ndetail"
    )
