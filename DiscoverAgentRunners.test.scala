class DiscoverAgentRunnersSuite extends munit.FunSuite:
  test("discovery is offline and emits the pinned raw-price fields"):
    val script = os.read(os.pwd / "scripts" / "discover-agent-runners.scala")
    List("http://", "https://", "curl", "java.net", "requests").foreach { token =>
      assert(!script.toLowerCase.contains(token), s"network token found: $token")
    }

    val json = ujson.read(
      os.read(os.pwd / ".gh-tasks-llm-executor" / "agent-runners.json")
    )
    json("tools").arr.foreach { tool =>
      val fields = tool.obj
      assert(fields.contains("inputUsdPerMTok"))
      assert(fields.contains("outputUsdPerMTok"))
      assert(fields.contains("source"))
      assert(fields.contains("asOfDate"))
    }
