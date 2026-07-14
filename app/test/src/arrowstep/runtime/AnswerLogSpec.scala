package arrowstep.runtime

import arrowstep.core.Answers
import cats.effect.IO

final class AnswerLogSpec extends munit.CatsEffectSuite:

  test("write then read round-trips answers through .agents/answers.json") {
    withTempDir { root =>
      val answers = Answers(Map("lang" -> "scala", "build" -> "mill"))

      for
        _ <- AnswerLog.write[IO](root, answers)
        read <- AnswerLog.read[IO](root)
        file <- IO(os.read(root / ".agents" / "answers.json"))
      yield
        assertEquals(read.toMap, answers.toMap)
        assertEquals(file, """{"build":"mill","lang":"scala"}""")
    }
  }

  test("absent answer log reads as empty answers") {
    withTempDir { root =>
      AnswerLog.read[IO](root).map(read => assertEquals(read.toMap, Map.empty[String, String]))
    }
  }

  test("malformed answer log reads as empty answers") {
    withTempDir { root =>
      for
        _ <- IO(os.makeDir.all(root / ".agents"))
        _ <- IO(os.write(root / ".agents" / "answers.json", "{"))
        read <- AnswerLog.read[IO](root)
      yield assertEquals(read.toMap, Map.empty[String, String])
    }
  }

  test("merge and upsert keep later answers") {
    val base = Answers(Map("lang" -> "scala", "build" -> "sbt"))
    val merged = AnswerLog.merge(base, Answers(Map("build" -> "mill")))
    val updated = AnswerLog.upsert(merged, "test", "munit")

    assertEquals(updated.toMap, Map("lang" -> "scala", "build" -> "mill", "test" -> "munit"))
  }

  test("retainActive removes stale question answers and keeps cached values") {
    val answers = Answers(Map("lang" -> "scala", "old" -> "stale", "_cache.scala-version" -> "3.8.4"))
    val retained = AnswerLog.retainActive(answers, Set("lang"))

    assertEquals(retained.toMap, Map("lang" -> "scala", "_cache.scala-version" -> "3.8.4"))
  }

  test("pruneStale writes retained active answers back to disk") {
    withTempDir { root =>
      for
        _ <- AnswerLog.write[IO](root, Answers(Map("lang" -> "scala", "old" -> "stale")))
        retained <- AnswerLog.pruneStale[IO](root, Set("lang"))
        read <- AnswerLog.read[IO](root)
      yield
        assertEquals(retained.toMap, Map("lang" -> "scala"))
        assertEquals(read.toMap, Map("lang" -> "scala"))
    }
  }

  test("reset clears the persisted answer log") {
    withTempDir { root =>
      for
        _ <- AnswerLog.write[IO](root, Answers(Map("lang" -> "scala")))
        _ <- AnswerLog.reset[IO](root)
        read <- AnswerLog.read[IO](root)
      yield assertEquals(read.toMap, Map.empty[String, String])
    }
  }

  private def withTempDir(test: os.Path => IO[Unit]): IO[Unit] =
    IO(os.temp.dir(prefix = "arrowstep-answer-log-")).bracket(test)(root => IO(os.remove.all(root)))
