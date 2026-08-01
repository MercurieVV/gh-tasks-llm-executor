package com.github.mercurievv.ghllm.agent

import com.github.mercurievv.ghllm.arrow.VerificationResult

class TurnCapSuite extends munit.FunSuite:

  test("a runner reporting 26 turns against the default cap yields Red"):
    val breach = TurnCap.exceeded(Some(26), TurnCap.Default)
    assertEquals(breach.map(_.turnCount), Some(26))
    VerificationResult.fromThrowable(breach.get) match
      case VerificationResult.Red(summary, detail) =>
        assertEquals(summary, "turn cap exceeded")
        assert(detail.contains("26"))
        assert(detail.contains("25"))
      case other => fail(s"expected Red, got $other")

  test("a run at or under the cap is not a breach"):
    assertEquals(TurnCap.exceeded(Some(25), 25), None)
    assertEquals(TurnCap.exceeded(Some(1), 25), None)

  test("an unreported turn count is never a breach"):
    // Only claude reports num_turns; other runners must not be failed for it.
    assertEquals(TurnCap.exceeded(None, 25), None)

  test("the cap comes from config when present"):
    val root = os.temp.dir(prefix = "turn-cap")
    assertEquals(TurnCap.load(root), TurnCap.Default)
    os.write.over(root / TurnCap.RelativePath, """{"turnCap": 8}""", createFolders = true)
    assertEquals(TurnCap.load(root), 8)

  test("a malformed or non-positive cap falls back to the default"):
    val root = os.temp.dir(prefix = "turn-cap-bad")
    os.write.over(root / TurnCap.RelativePath, """{"turnCap": 0}""", createFolders = true)
    assertEquals(TurnCap.load(root), TurnCap.Default)
    os.write.over(root / TurnCap.RelativePath, "not json")
    assertEquals(TurnCap.load(root), TurnCap.Default)

class RepairTurnCapSuite extends munit.FunSuite:
  test("a repair gets a multiple of the leaf cap by default"):
    val root = os.temp.dir(prefix = "repair-turn-cap")
    assertEquals(TurnCap.loadRepair(root), TurnCap.Default * TurnCap.RepairMultiplier)

  test("a stated leaf cap scales the repair cap with it"):
    val root = os.temp.dir(prefix = "repair-turn-cap-scaled")
    os.write.over(root / TurnCap.RelativePath, """{"turnCap": 10}""", createFolders = true)
    assertEquals(TurnCap.loadRepair(root), 30)

  test("a stated repair cap wins over the multiple"):
    val root = os.temp.dir(prefix = "repair-turn-cap-explicit")
    os.write.over(root / TurnCap.RelativePath, """{"turnCap": 10, "repairTurnCap": 12}""", createFolders = true)
    assertEquals(TurnCap.loadRepair(root), 12)
    assertEquals(TurnCap.load(root), 10)

  test("the four-file merge repair that motivated this is no longer a breach"):
    val root = os.temp.dir(prefix = "repair-turn-cap-observed")
    assertEquals(TurnCap.exceeded(Some(75), TurnCap.loadRepair(root)), None)
