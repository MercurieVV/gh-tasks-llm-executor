package com.github.mercurievv.ghllm.agent

class AgentExecutorSuite extends munit.FunSuite:
  test("billing failures are fatal even when an agent exits successfully"):
    val output =
      """litellm.BadRequestError: DeepseekException - {"error":{"message":"Insufficient
        |Balance","type":"unknown_error","param":null,"code":"invalid_request_error"}}
        |""".stripMargin

    assertEquals(
      AgentExecutor[cats.effect.IO].terminationReason(output).map(_.toLowerCase.contains("insufficient")),
      Some(true)
    )
