package arrowstep.example

class ExampleSpec extends munit.FunSuite:
  test("example package compiles"):
    assertEquals(Example.getClass.getPackageName, "arrowstep.example")
