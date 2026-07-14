package arrowstep.runtime

import arrowstep.core.{Answers, AskInput, Dialogue, Flow, ProgramSays, Question, QuestionKind, Validator}
import cats.effect.IO
import cats.effect.Ref

final class RuntimeSpec extends munit.CatsEffectSuite:

  private val question =
    Question("lang", "Language?", QuestionKind.Choice(List("scala", "java")), None, None, None)

  private val input =
    AskInput(List(question), Some("Project setup"))

  test("SessionStore persists purpose-scoped session ids under .agents/sessions.json") {
    withTempDir { root =>
      val setup =
        for
          _ <- SessionStore.put[IO](root, AgentPurpose("setup"), SessionId("session-1"))
          _ <- SessionStore.put[IO](root, AgentPurpose("review"), SessionId("session-2"))
          sessions <- SessionStore.read[IO](root)
          file <- IO(os.read(root / ".agents" / "sessions.json"))
        yield
          assertEquals(sessions.get(AgentPurpose("setup")), Some(SessionId("session-1")))
          assertEquals(sessions.get(AgentPurpose("review")), Some(SessionId("session-2")))
          assertEquals(file, """{"review":"session-2","setup":"session-1"}""")

      setup
    }
  }

  test("SessionStore treats absent and malformed files as empty sessions") {
    withTempDir { root =>
      for
        absent <- SessionStore.read[IO](root)
        _ <- IO(os.write(root / ".agents" / "sessions.json", "{", createFolders = true))
        malformed <- SessionStore.read[IO](root)
      yield
        assertEquals(absent, Map.empty[AgentPurpose, SessionId])
        assertEquals(malformed, Map.empty[AgentPurpose, SessionId])
    }
  }

  test("AdapterRegistry ships claude and gemini defaults") {
    val registry = AdapterRegistry.default

    assertEquals(registry.get("claude").map(_.fresh.headOption), Some(Some("claude")))
    assertEquals(registry.get("gemini").map(_.resume.headOption), Some(Some("gemini")))
  }

  test("AdapterRegistry overlays .agents/adapters.json on top of defaults") {
    withTempDir { root =>
      val configured =
        """{"claude":{"new":["custom-claude","{prompt}"],"resume":["custom-claude","--resume","{session}","{prompt}"]},"aider":{"new":["aider","--message","{prompt}"],"resume":["aider","--message","{prompt}"]}}"""

      for
        _ <- IO(os.write(root / ".agents" / "adapters.json", configured, createFolders = true))
        registry <- AdapterRegistry.load[IO](root)
      yield
        assertEquals(registry.get("claude").map(_.fresh), Some(List("custom-claude", "{prompt}")))
        assertEquals(registry.get("gemini").map(_.fresh.headOption), Some(Some("gemini")))
        assertEquals(registry.get("aider").map(_.resume), Some(List("aider", "--message", "{prompt}")))
    }
  }

  test("Cached.string computes once and stores the value in the answer log") {
    withTempDir { root =>
      for
        counter <- Ref[IO].of(0)
        first <- Cached.string[IO](root, CacheKey("scala-version"))(
          counter.updateAndGet(_ + 1).map(i => "3.8." + i.toString)
        )
        second <- Cached.string[IO](root, CacheKey("scala-version"))(
          counter.updateAndGet(_ + 1).map(i => "3.8." + i.toString)
        )
        count <- counter.get
        answers <- AnswerLog.read[IO](root)
      yield
        assertEquals(first, "3.8.1")
        assertEquals(second, "3.8.1")
        assertEquals(count, 1)
        assertEquals(answers.get("_cache.scala-version"), Some("3.8.1"))
    }
  }

  test("Cached.flow exposes cached effects as a Flow step") {
    withTempDir { root =>
      val cached = Cached.flow[IO, String](root, CacheKey("module-name")) { module =>
        IO.pure(module.toUpperCase)
      }

      for
        first <- cached.run("core")
        second <- cached.run("ignored")
      yield
        assertEquals(first, "CORE")
        assertEquals(second, "CORE")
    }
  }

  test("ReplayAsk returns the answer log when every requested question is answered") {
    withTempDir { root =>
      val answers = Answers(Map("lang" -> "scala", "extra" -> "kept"))

      for
        _ <- AnswerLog.write[IO](root, answers)
        replayed <- ReplayAsk[IO](root)(input)
      yield assertEquals(replayed.toMap, answers.toMap)
    }
  }

  test("ReplayAsk raises NeedInput with only missing questions") {
    withTempDir { root =>
      val build = Question("build", "Build tool?", QuestionKind.Choice(List("mill", "sbt")), None, None, None)
      val fullInput = input.copy(questions = List(question, build))

      for
        _ <- AnswerLog.write[IO](root, Answers(Map("lang" -> "scala")))
        result <- ReplayAsk[IO](root)(fullInput).attempt
      yield
        val says = result match
          case Left(need: ReplayNeedInput) => need.programSays
          case Left(other)                 => ProgramSays.Done(other.getMessage)
          case Right(_)                    => ProgramSays.Done("answered")

        assertEquals(says, ProgramSays.NeedInput(Some("Project setup"), List(build)))
    }
  }

  test("ReplayAsk can drive askUntilValid from a complete replay log") {
    withTempDir { root =>
      for
        _ <- AnswerLog.write[IO](root, Answers(Map("lang" -> "scala")))
        valid <- Dialogue.askUntilValid(ReplayAsk[IO](root), Validator.basic[IO]).run(input)
      yield assertEquals(valid.toMap, Map("lang" -> "scala"))
    }
  }

  test("ReplayAsk validating flow rejects invalid logged answers with offending questions") {
    withTempDir { root =>
      for
        _ <- AnswerLog.write[IO](root, Answers(Map("lang" -> "ruby")))
        result <- ReplayAsk.askUntilValid[IO](root, Validator.basic[IO]).run(input).attempt
      yield
        val says = result match
          case Left(rejected: ReplayRejected) => rejected.programSays
          case Left(other)                    => ProgramSays.Done(other.getMessage)
          case Right(_)                       => ProgramSays.Done("accepted")

        assertEquals(
          says,
          ProgramSays.Rejected(
            List(arrowstep.core.Problem("lang", "'ruby' not in [scala, java]")),
            List(question.copy(context = Some("'ruby' not in [scala, java]")))
          )
        )
    }
  }

  test("ReplayDeterminism captures identical question sequences across replay runs") {
    withTempDir { root =>
      for
        check <- ReplayDeterminism.check[IO, AskInput, arrowstep.core.ValidAnswers](
          root,
          Answers(Map("lang" -> "scala")),
          input
        )(ask => Dialogue.askUntilValid(ask, Validator.basic[IO]))
      yield
        assert(check.deterministic)
        assertEquals(check.differingIndex, None)
        assertEquals(check.first.questionSequences, List(List(question)))
        assertEquals(check.second.questionSequences, List(List(question)))
    }
  }

  test("ReplayDeterminism reports the first changed question sequence") {
    withTempDir { root =>
      val build = Question("build", "Build tool?", QuestionKind.Choice(List("mill", "sbt")), None, None, None)

      for
        counter <- Ref[IO].of(0)
        check <- ReplayDeterminism.check[IO, Unit, Answers](
          root,
          Answers(Map("lang" -> "scala", "build" -> "mill")),
          ()
        ) { ask =>
          Flow.apply { (_: Unit) =>
            counter.updateAndGet(_ + 1).flatMap { run =>
              val selected = if run == 1 then question else build
              ask(AskInput(List(selected), None))
            }
          }
        }
      yield
        assert(!check.deterministic)
        assertEquals(check.differingIndex, Some(0))
        assertEquals(check.first.questionSequences, List(List(question)))
        assertEquals(check.second.questionSequences, List(List(build)))
    }
  }

  test("AgentArgs parses supported Phase 1 flags") {
    val parsed = AgentArgs.parse(
      List(
        "--agent",
        "--answers",
        """{"lang":"scala"}""",
          "--fresh",
          "--reset",
          "--panes",
          "--resume-session",
          "session-1",
          "--adapter",
        "claude"
      )
    )

    assertEquals(
      parsed,
      Right(
        AgentArgs(
            agent = true,
              inlineAnswers = Some(Answers(Map("lang" -> "scala"))),
              fresh = true,
              reset = true,
              panes = true,
              resumeSession = Some(SessionId("session-1")),
              adapter = Some("claude")
          )
      )
    )
  }

  test("AgentArgs.parseKnown leaves consumer arguments after runtime flags") {
    val parsed = AgentArgs.parseKnown(
      List("--agent", "--reset", "--answers", """{"lang":"scala"}""", ".", "--consumer-flag")
    )

    assertEquals(
      parsed,
      Right(
        AgentArgs.Parsed(
          AgentArgs(
            agent = true,
            inlineAnswers = Some(Answers(Map("lang" -> "scala"))),
            fresh = false,
            reset = true,
            panes = false,
            resumeSession = None,
            adapter = None
          ),
          List(".", "--consumer-flag")
        )
      )
    )
  }

  test("AgentArgs.parseKnown still validates runtime flag values") {
    assertEquals(AgentArgs.parseKnown(List("--answers")), Left("missing value for --answers"))
    assertEquals(AgentArgs.parseKnown(List("--answers", "{")), Left("invalid JSON for --answers"))
    assertEquals(AgentArgs.parseKnown(List("--resume-session")), Left("missing value for --resume-session"))
    assertEquals(AgentArgs.parseKnown(List("--adapter")), Left("missing value for --adapter"))
  }

  test("AgentArgs.parseKnown consumes runtime flags after consumer arguments") {
    val parsed = AgentArgs.parseKnown(List("--agent", ".", "--answers", """{"lang":"scala"}"""))

    assertEquals(
      parsed.map(parsed => parsed.args.inlineAnswers -> parsed.rest),
      Right(Some(Answers(Map("lang" -> "scala"))) -> List("."))
    )
  }

  test("AgentMain persists inline --answers before running the program") {
    withTempDir { root =>
      for
        outcome <- AgentMain.run[IO](List("--answers", """{"lang":"scala"}"""), root) { _ =>
          IO.pure(ProgramSays.Done(ujson.Obj("ok" -> true)))
        }
        answers <- AnswerLog.read[IO](root)
      yield
        assertEquals(answers.toMap, Map("lang" -> "scala"))
        assertEquals(outcome.exitCode, 0)
        assertEquals(outcome.stdout, """{"status":"done","result":{"ok":true}}""")
        assertEquals(outcome.stderr, "")
    }
  }

  test("AgentMain resets the answer log before merging inline --answers") {
    withTempDir { root =>
      for
        _ <- AnswerLog.write[IO](root, Answers(Map("old" -> "stale")))
        outcome <- AgentMain.run[IO](List("--reset", "--answers", """{"lang":"scala"}"""), root) { _ =>
          IO.pure(ProgramSays.Done(ujson.Obj("ok" -> true)))
        }
        answers <- AnswerLog.read[IO](root)
      yield
        assertEquals(answers.toMap, Map("lang" -> "scala"))
        assertEquals(outcome.exitCode, 0)
    }
  }

  test("ReplayHygiene prunes answers for questions no longer asked by the replayed flow") {
    withTempDir { root =>
      for
        _ <- AnswerLog.write[IO](
          root,
          Answers(Map("lang" -> "scala", "old" -> "stale", "_cache.scala-version" -> "3.8.4"))
        )
        result <- ReplayHygiene.run[IO, AskInput, arrowstep.core.ValidAnswers](root, input) { ask =>
          Dialogue.askUntilValid(ask, Validator.basic[IO])
        }
        read <- AnswerLog.read[IO](root)
      yield
        assert(result.result.isRight)
        assertEquals(result.retainedAnswers.toMap, Map("lang" -> "scala", "_cache.scala-version" -> "3.8.4"))
        assertEquals(read.toMap, Map("lang" -> "scala", "_cache.scala-version" -> "3.8.4"))
    }
  }

  test("AgentMain renders ReplayNeedInput as protocol stdout and exit code 2") {
    val missing = ReplayNeedInput(input)

    AgentMain.run[IO](IO.raiseError(missing)).map { outcome =>
      assertEquals(outcome.exitCode, 2)
      assertEquals(outcome.stderr, "")
      assertEquals(
        outcome.stdout,
        """{"status":"need-input","context":"Project setup","questions":[{"id":"lang","text":"Language?","kind":"choice","allowed":["scala","java"],"default":null,"current":null,"context":null}]}"""
      )
    }
  }

  test("AgentMain renders replay validation failures as Rejected protocol stdout") {
    withTempDir { root =>
      for
        _ <- AnswerLog.write[IO](root, Answers(Map("lang" -> "ruby")))
        outcome <- AgentMain.run[IO](
          ReplayAsk
            .askUntilValid[IO](root, Validator.basic[IO])
            .run(input)
            .map(_ => ProgramSays.Done(ujson.Obj("ok" -> true)))
        )
      yield
        assertEquals(outcome.exitCode, 2)
        assertEquals(outcome.stderr, "")
        assertEquals(
          outcome.stdout,
          """{"status":"rejected","problems":[{"questionId":"lang","message":"'ruby' not in [scala, java]"}],"questions":[{"id":"lang","text":"Language?","kind":"choice","allowed":["scala","java"],"default":null,"current":null,"context":"'ruby' not in [scala, java]"}]}"""
        )
    }
  }

  test("AgentOutputPrefix prefixes each agent stderr line without changing log text") {
    val prefix = AgentOutputPrefix.from(
      AgentAdapter("claude", List("claude"), List("claude")),
      AgentPurpose("2")
    )

    assertEquals(AgentOutputPrefix.prefix("first\nsecond\n", prefix, colored = false), "[claude#2] first\n[claude#2] second\n")
    assert(AgentOutputPrefix.prefix("first\n", prefix, colored = true).contains("[claude#2] "))
  }

  test("LiveAsk default process streams stderr into the raw purpose log") {
    withTempDir { root =>
      val adapter = AgentAdapter(
        "shell",
        List("/bin/sh", "-c", """printf 'first\nsecond\n' >&2; printf '{"lang":"scala"}'"""),
        List("/bin/sh", "-c", """printf '{"lang":"scala"}'""")
      )

      for
        answers <- LiveAsk[IO](LiveAskConfig(adapter, AgentPurpose("setup"), root, fresh = true, resumeSession = None, panes = false))(
          input
        )
        log <- IO(os.read(root / ".agents" / "logs" / "setup.log"))
      yield
        assertEquals(answers.toMap, Map("lang" -> "scala"))
        assertEquals(log, "first\nsecond\n")
    }
  }

  test("TmuxPanes builds split-window tail command only when requested inside tmux") {
    val logFile = os.root / "path with spaces" / "setup's.log"

    assertEquals(TmuxPanes.command(enabled = false, Some("tmux-session"), logFile), None)
    assertEquals(TmuxPanes.command(enabled = true, None, logFile), None)
    assertEquals(
      TmuxPanes.command(enabled = true, Some("tmux-session"), logFile),
      Some(List("tmux", "split-window", "-h", "tail -f '/path with spaces/setup'\"'\"'s.log'"))
    )
  }

  test("AgentLogs rotates bounded purpose logs before appending") {
    withTempDir { root =>
      val logFile = AgentLogs.file(root, AgentPurpose("setup"))

      for
        _ <- IO(os.write(logFile, "current", createFolders = true))
        _ <- IO(os.write(os.Path(logFile.toString + ".1"), "older"))
        _ <- AgentLogs.rotate[IO](logFile, maxBytes = 1L, keep = 2)
        currentExists <- IO(os.exists(logFile))
        first <- IO(os.read(os.Path(logFile.toString + ".1")))
        second <- IO(os.read(os.Path(logFile.toString + ".2")))
      yield
        assert(!currentExists)
        assertEquals(first, "current")
        assertEquals(second, "older")
    }
  }

  test("LiveAsk runs the fresh adapter command and persists answers plus returned session id") {
    withTempDir { root =>
      val adapter = AgentAdapter("test-agent", List("agent", "{prompt}"), List("agent", "--resume", "{session}", "{prompt}"))

      for
        seen <- Ref[IO].of(List.empty[List[String]])
        prefixes <- Ref[IO].of(List.empty[AgentOutputPrefix])
        ask = LiveAsk.withProcess[IO](
          LiveAskConfig(adapter, AgentPurpose("setup"), root, fresh = false, resumeSession = None, panes = false)
        ) {
          case (command, _, logFile, prefix) =>
            seen.update(command :: _) *>
              prefixes.update(prefix :: _) *>
              IO(assertEquals(logFile.relativeTo(root).toString, ".agents/logs/setup.log")) *>
              IO.pure(AgentProcessResult(0, """{"answers":{"lang":"scala"},"sessionId":"session-1"}""", "log"))
        }
        answers <- ask(input)
        commands <- seen.get
        observedPrefixes <- prefixes.get
        sessions <- SessionStore.read[IO](root)
        stored <- AnswerLog.read[IO](root)
      yield
        assertEquals(answers.toMap, Map("lang" -> "scala"))
        assertEquals(commands.map(_.headOption), List(Some("agent")))
        assert(commands.headOption.flatMap(_.lastOption).exists(_.contains("Language?")))
        assertEquals(observedPrefixes.map(_.label), List("test-agent#setup"))
        assertEquals(sessions.get(AgentPurpose("setup")), Some(SessionId("session-1")))
        assertEquals(stored.toMap, Map("lang" -> "scala"))
    }
  }

  test("LiveAsk resumes the stored session unless fresh is requested") {
    withTempDir { root =>
      val adapter = AgentAdapter("test-agent", List("agent", "new", "{prompt}"), List("agent", "resume", "{session}"))

      for
        _ <- SessionStore.put[IO](root, AgentPurpose("setup"), SessionId("stored-session"))
        seen <- Ref[IO].of(List.empty[List[String]])
          resumed = LiveAsk.withProcess[IO](
            LiveAskConfig(adapter, AgentPurpose("setup"), root, fresh = false, resumeSession = None, panes = false)
          ) {
            case (command, _, _, _) =>
              seen.update(command :: _) *> IO.pure(AgentProcessResult(0, """{"lang":"scala"}""", ""))
          }
          fresh = LiveAsk.withProcess[IO](
            LiveAskConfig(adapter, AgentPurpose("setup"), root, fresh = true, resumeSession = None, panes = false)
          ) {
            case (command, _, _, _) =>
              seen.update(command :: _) *> IO.pure(AgentProcessResult(0, """{"lang":"scala"}""", ""))
          }
        _ <- resumed(input)
        _ <- fresh(input)
        commands <- seen.get
      yield
        val ordered = commands.reverse
        assertEquals(ordered.headOption.map(_.take(3)), Some(List("agent", "resume", "stored-session")))
        assertEquals(ordered.drop(1).headOption.map(_.take(2)), Some(List("agent", "new")))
    }
  }

  private def withTempDir(test: os.Path => IO[Unit]): IO[Unit] =
    IO(os.temp.dir(prefix = "arrowstep-runtime-")).bracket(test)(root => IO(os.remove.all(root)))
