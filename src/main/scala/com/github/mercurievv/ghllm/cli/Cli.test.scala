package com.github.mercurievv.ghllm.cli

import com.github.mercurievv.ghllm.*

class PredictAndRunCommandSuite extends munit.FunSuite:

  test("predict-and-run parses a task and defaults its prices"):
    val command = Cli.parsePredictAndRunCommand(List("predict-and-run", "--task=42"), os.pwd)

    assertEquals(command.map(_.task), Some(TaskNumber(42)))
    assertEquals(
      command.map(_.costModel.inputUsdPerMillionTokens),
      Some(Cli.DefaultInputUsdPerMillionTokens)
    )

  test("prices are overridable, same flags as estimate"):
    val command = Cli.parsePredictAndRunCommand(
      List("predict-and-run", "--task=1", "--input-usd-per-mtok=0.5", "--output-usd-per-mtok", "7"),
      os.pwd
    )

    assertEquals(command.map(_.costModel.inputUsdPerMillionTokens), Some(0.5))
    assertEquals(command.map(_.costModel.outputUsdPerMillionTokens), Some(7.0))

  test("predict-and-run without a task is not a predict-and-run command"):
    assertEquals(Cli.parsePredictAndRunCommand(List("predict-and-run"), os.pwd), None)

  test("does not shadow plain estimate, and vice versa"):
    assertEquals(Cli.parsePredictAndRunCommand(List("estimate", "--task=1"), os.pwd), None)
    assertEquals(Cli.parseEstimateCommand(List("predict-and-run", "--task=1"), os.pwd), None)

  test("other invocations are untouched"):
    assertEquals(Cli.parsePredictAndRunCommand(List("--task=1"), os.pwd), None)
    assertEquals(Cli.parsePredictAndRunCommand(List("metrics", "--task=1"), os.pwd), None)

  test("predict-and-run args never leak into the agent's argv"):
    assertEquals(Cli.removeScriptArgs(List("predict-and-run", "--task=1", "--input-usd-per-mtok=2")), Nil)
