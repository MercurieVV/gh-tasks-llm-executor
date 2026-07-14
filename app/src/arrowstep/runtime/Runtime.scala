package arrowstep.runtime

import arrowstep.core.{Answers, Ask, AskInput, Dialogue, Flow, Problem, ProgramSays, Question, Validator, ValidAnswers}
import cats.effect.Sync
import cats.syntax.all.*

import java.nio.file.Files
import java.nio.file.StandardCopyOption
import scala.jdk.CollectionConverters.*
import scala.util.Try

final case class SessionId(value: String)
final case class AgentPurpose(value: String)

final case class AgentAdapter(name: String, fresh: List[String], resume: List[String])

object AgentAdapter:

  val Claude: AgentAdapter =
    AgentAdapter(
      name = "claude",
      fresh = List("claude", "-p", "--output-format", "json", "{prompt}"),
      resume = List("claude", "-p", "--resume", "{session}", "--output-format", "json", "{prompt}")
    )

  val Gemini: AgentAdapter =
    AgentAdapter(
      name = "gemini",
      fresh = List("gemini", "-p", "--output-format", "json", "{prompt}"),
      resume = List("gemini", "-p", "--resume", "{session}", "--output-format", "json", "{prompt}")
    )

final case class AdapterRegistry(adapters: Map[String, AgentAdapter]):
  def get(name: String): Option[AgentAdapter] =
    adapters.get(name)

  def updated(adapter: AgentAdapter): AdapterRegistry =
    copy(adapters = adapters.updated(adapter.name, adapter))

object AdapterRegistry:

  private val AgentsDir = ".agents"
  private val AdaptersFile = "adapters.json"

  val default: AdapterRegistry =
    AdapterRegistry(List(AgentAdapter.Claude, AgentAdapter.Gemini).map(a => a.name -> a).toMap)

  def load[F[_]: Sync]: F[AdapterRegistry] =
    Sync[F].delay(os.pwd).flatMap(load[F])

  def load[F[_]: Sync](root: os.Path): F[AdapterRegistry] =
    Sync[F].delay {
      val file = root / AgentsDir / AdaptersFile
      val configured =
        if os.exists(file) then parse(os.read(file)).getOrElse(AdapterRegistry(Map.empty))
        else AdapterRegistry(Map.empty)

      configured.adapters.values.foldLeft(default)(_.updated(_))
    }

  private def parse(raw: String): Option[AdapterRegistry] =
    Try(ujson.read(raw)).toOption.flatMap {
      case ujson.Obj(values) =>
        values.toList
          .traverse { case (name, value) => parseAdapter(name, value).map(adapter => name -> adapter) }
          .map(entries => AdapterRegistry(entries.toMap))
      case _ => None
    }

  private def parseAdapter(name: String, value: ujson.Value): Option[AgentAdapter] =
    value match
      case ujson.Obj(fields) =>
        for
          fresh <- fields.get("new").flatMap(parseCommand)
          resume <- fields.get("resume").flatMap(parseCommand)
        yield AgentAdapter(name, fresh, resume)
      case _ => None

  private def parseCommand(value: ujson.Value): Option[List[String]] =
    value match
      case ujson.Arr(items) =>
        items.toList.traverse {
          case ujson.Str(part) => Some(part)
          case _               => None
        }
      case _ => None

object SessionStore:

  private val AgentsDir = ".agents"
  private val SessionsFile = "sessions.json"

  def read[F[_]: Sync]: F[Map[AgentPurpose, SessionId]] =
    Sync[F].delay(os.pwd).flatMap(read[F])

  def read[F[_]: Sync](root: os.Path): F[Map[AgentPurpose, SessionId]] =
    Sync[F].delay {
      val file = path(root)
      if os.exists(file) then parse(os.read(file)).getOrElse(Map.empty)
      else Map.empty
    }

  def get[F[_]: Sync](root: os.Path, purpose: AgentPurpose): F[Option[SessionId]] =
    read[F](root).map(_.get(purpose))

  def put[F[_]: Sync](root: os.Path, purpose: AgentPurpose, session: SessionId): F[Unit] =
    read[F](root).flatMap(sessions => write[F](root, sessions.updated(purpose, session)))

  def write[F[_]: Sync](root: os.Path, sessions: Map[AgentPurpose, SessionId]): F[Unit] =
    Sync[F].delay {
      os.write.over(path(root), render(sessions), createFolders = true)
    }

  private def path(root: os.Path): os.Path =
    root / AgentsDir / SessionsFile

  private def parse(raw: String): Option[Map[AgentPurpose, SessionId]] =
    Try(ujson.read(raw)).toOption.flatMap {
      case ujson.Obj(values) =>
        values.toList
          .traverse {
            case (purpose, ujson.Str(session)) => Some(AgentPurpose(purpose) -> SessionId(session))
            case _                            => None
          }
          .map(_.toMap)
      case _ => None
    }

  private def render(sessions: Map[AgentPurpose, SessionId]): String =
    ujson.Obj.from(
      sessions.toList
        .sortBy { case (purpose, _) => purpose.value }
        .map { case (purpose, session) => purpose.value -> ujson.Str(session.value) }
    ).render()

final case class LiveAskConfig(
    adapter: AgentAdapter,
    purpose: AgentPurpose,
    root: os.Path,
    fresh: Boolean,
    resumeSession: Option[SessionId],
    panes: Boolean
)

final case class AgentProcessResult(exitCode: Int, stdout: String, stderr: String)

final case class AgentOutputPrefix(label: String, color: Option[String])

object AgentOutputPrefix:
  private val Reset = "\u001b[0m"
  private val Colors =
    List("\u001b[36m", "\u001b[35m", "\u001b[32m", "\u001b[33m", "\u001b[34m", "\u001b[31m")

  def from(adapter: AgentAdapter, purpose: AgentPurpose): AgentOutputPrefix =
    AgentOutputPrefix(adapter.name + "#" + purpose.value, colorFor(adapter.name + purpose.value))

  def prefix(text: String, prefix: AgentOutputPrefix, colored: Boolean): String =
    if text.isEmpty then ""
    else
      val renderedPrefix = render(prefix, colored)
      val trailingNewline = text.endsWith("\n")
      val lines = text.linesIterator.toList
      val body = lines.map(line => renderedPrefix + line).mkString("\n")
      if trailingNewline then body + "\n" else body

  private def render(prefix: AgentOutputPrefix, colored: Boolean): String =
    val raw = "[" + prefix.label + "] "
    if colored then prefix.color.fold(raw)(color => color + raw + Reset) else raw

  private def colorFor(value: String): Option[String] =
    Colors.lift(value.toList.map(_.toInt).sum.abs % Colors.size)

object TmuxPanes:
  def open[F[_]: Sync](enabled: Boolean, root: os.Path, logFile: os.Path): F[Unit] =
    command(enabled, sys.env.get("TMUX"), logFile).fold(Sync[F].unit) { tmuxCommand =>
      Sync[F].blocking {
        os.proc(tmuxCommand).call(cwd = root, stdout = os.Pipe, stderr = os.Pipe, check = false)
      }.void
    }

  def command(enabled: Boolean, tmux: Option[String], logFile: os.Path): Option[List[String]] =
    Option.when(enabled)(tmux).flatten.map(_ =>
      List("tmux", "split-window", "-h", "tail -f " + shellQuote(logFile.toString))
    )

  private def shellQuote(value: String): String =
    "'" + value.replace("'", "'\"'\"'") + "'"

object AgentLogs:
  private val AgentsDir = ".agents"
  private val LogsDir = "logs"
  private val MaxBytes = 1024L * 1024L
  private val Keep = 5

  def file(root: os.Path, purpose: AgentPurpose): os.Path =
    root / AgentsDir / LogsDir / (purpose.value + ".log")

  def rotate[F[_]: Sync](logFile: os.Path): F[Unit] =
    rotate[F](logFile, MaxBytes, Keep)

  def rotate[F[_]: Sync](logFile: os.Path, maxBytes: Long, keep: Int): F[Unit] =
    Sync[F].blocking {
      if os.exists(logFile) && os.size(logFile) >= maxBytes then
        if keep <= 0 then os.remove(logFile)
        else
          val oldest = rotated(logFile, keep)
          if os.exists(oldest) then os.remove(oldest)
          List.range(1, keep).reverse.foreach { index =>
            val current = rotated(logFile, index)
            if os.exists(current) then move(current, rotated(logFile, index + 1))
          }
          move(logFile, rotated(logFile, 1))
    }

  private def rotated(logFile: os.Path, index: Int): os.Path =
    os.Path(logFile.toString + "." + index.toString)

  private def move(from: os.Path, to: os.Path): Unit =
    Files.move(from.toNIO, to.toNIO, StandardCopyOption.REPLACE_EXISTING)

final case class ReplayNeedInput(input: AskInput)
    extends RuntimeException("Replay answer log is missing answers for the requested questions"):
  def programSays: ProgramSays[Nothing] =
    ProgramSays.NeedInput(input.context, input.questions)

final case class ReplayRejected(problems: List[Problem], input: AskInput)
    extends RuntimeException("Replay answer log contains invalid answers for the requested questions"):
  def programSays: ProgramSays[Nothing] =
    ProgramSays.Rejected(problems, input.questions)

final class ReplayAsk[F[_]: Sync](root: os.Path) extends Ask[F]:

  def apply(input: AskInput): F[Answers] =
    AnswerLog.read[F](root).flatMap { log =>
      val missing = ReplayAsk.missing(input.questions, log)
      if missing.isEmpty then Sync[F].pure(log)
      else Sync[F].raiseError(ReplayNeedInput(input.copy(questions = missing)))
    }

object ReplayAsk:

  def apply[F[_]: Sync]: F[ReplayAsk[F]] =
    Sync[F].delay(os.pwd).map(apply[F])

  def apply[F[_]: Sync](root: os.Path): ReplayAsk[F] =
    new ReplayAsk[F](root)

  def askUntilValid[F[_]: Sync](validator: Validator[F]): F[Flow[F, AskInput, ValidAnswers]] =
    Sync[F].delay(os.pwd).map(root => askUntilValid(root, validator))

  def askUntilValid[F[_]: Sync](root: os.Path, validator: Validator[F]): Flow[F, AskInput, ValidAnswers] =
    Flow.apply { (input: AskInput) =>
      AnswerLog.read[F](root).flatMap { log =>
        val missingQuestions = missing(input.questions, log)
        if missingQuestions.nonEmpty then Sync[F].raiseError(ReplayNeedInput(input.copy(questions = missingQuestions)))
        else
          validator.validate(input.questions, log).flatMap {
            case Right(valid) => Sync[F].pure(valid)
            case Left(problems) =>
              Sync[F].raiseError(ReplayRejected(problems, Dialogue.reAsk(input, problems)))
          }
      }
    }

  private def missing(questions: List[Question], answers: Answers): List[Question] =
    questions.filter(q => answers.get(q.id).isEmpty)

final class LiveAsk[F[_]: Sync](
    config: LiveAskConfig,
    process: (List[String], os.Path, os.Path, AgentOutputPrefix) => F[AgentProcessResult]
) extends Ask[F]:

  def apply(input: AskInput): F[Answers] =
    for
      prompt <- Sync[F].pure(LiveAsk.prompt(input))
      session <- LiveAsk.session[F](config)
      command <- Sync[F].fromEither(LiveAsk.command(config.adapter, prompt, session).leftMap(new RuntimeException(_)))
      logFile <- Sync[F].pure(AgentLogs.file(config.root, config.purpose))
      prefix <- Sync[F].pure(AgentOutputPrefix.from(config.adapter, config.purpose))
      _ <- AgentLogs.rotate[F](logFile)
      _ <- TmuxPanes.open[F](config.panes, config.root, logFile)
      result <- process(command, config.root, logFile, prefix)
      response <- Sync[F].fromEither(LiveAsk.response(result))
      _ <- AnswerLog.read[F](config.root).flatMap(existing =>
        AnswerLog.write[F](config.root, AnswerLog.merge(existing, response.answers))
      )
      _ <- response.sessionId.fold(Sync[F].unit)(SessionStore.put[F](config.root, config.purpose, _))
    yield response.answers

object LiveAsk:
  private val PromptToken = "{prompt}"
  private val SessionToken = "{session}"
  private final case class Response(answers: Answers, sessionId: Option[SessionId])

  def apply[F[_]: Sync](config: LiveAskConfig): LiveAsk[F] =
    new LiveAsk[F](config, runProcess[F])

  def withProcess[F[_]: Sync](
      config: LiveAskConfig
  )(process: (List[String], os.Path, os.Path, AgentOutputPrefix) => F[AgentProcessResult]): LiveAsk[F] =
    new LiveAsk[F](config, process)

  private def session[F[_]: Sync](config: LiveAskConfig): F[Option[SessionId]] =
    config.resumeSession match
      case some @ Some(_) => Sync[F].pure(some)
      case None           => if config.fresh then Sync[F].pure(None) else SessionStore.get[F](config.root, config.purpose)

  private def command(adapter: AgentAdapter, prompt: String, session: Option[SessionId]): Either[String, List[String]] =
    val template = session.fold(adapter.fresh)(_ => adapter.resume)
    val rendered = template.map(part => expand(part, prompt, session))
    Either.cond(rendered.nonEmpty, rendered, "agent adapter command must not be empty")

  private def expand(part: String, prompt: String, session: Option[SessionId]): String =
    part
      .replace(PromptToken, prompt)
      .replace(SessionToken, session.fold("")(_.value))

  private def response(result: AgentProcessResult): Either[Throwable, Response] =
    Either.cond(result.exitCode === 0, result, new RuntimeException("agent process exited with " + result.exitCode.toString))
      .flatMap(result => parseResponse(result.stdout).left.map(new RuntimeException(_)))

  private def parseResponse(raw: String): Either[String, Response] =
    finalJson(raw).flatMap { json =>
      Try(ujson.read(json)).toEither.left.map(_ => "invalid agent response JSON").flatMap {
        case ujson.Obj(fields) if fields.contains("answers") =>
          fields.get("answers").flatMap(value => AnswerLog.parse(value.render())) match
            case Some(answers) => Right(Response(answers, sessionId(fields)))
            case None          => Left("answers must be a JSON object with string values")
        case value =>
          AnswerLog.parse(value.render()).map(Response(_, None)).toRight("agent response must be answers JSON")
      }
    }

  private def finalJson(raw: String): Either[String, String] =
    raw.linesIterator.toList.reverse.find(_.trim.nonEmpty).map(_.trim).toRight("agent response was empty")

  private def sessionId(fields: collection.Map[String, ujson.Value]): Option[SessionId] =
    stringValue(fields.get("sessionId")).orElse(stringValue(fields.get("session_id"))).map(SessionId(_))

  private def stringValue(value: Option[ujson.Value]): Option[String] =
    value.collect { case ujson.Str(text) => text }

  private def prompt(input: AskInput): String =
    val context = input.context.fold("")(value => "Context:\n" + value + "\n\n")
    val questions = input.questions.map(questionPrompt).mkString("\n")
    context + "Answer these questions. Return exactly one JSON object whose keys are question ids and values are strings.\n" + questions

  private def questionPrompt(question: Question): String =
    val kind = question.kind match
      case arrowstep.core.QuestionKind.FreeText        => "free-text"
      case arrowstep.core.QuestionKind.Choice(allowed) => "choice: " + allowed.mkString(", ")
    "- " + question.id + ": " + question.text + " (" + kind + ")"

  private def runProcess[F[_]: Sync](
      command: List[String],
      root: os.Path,
      logFile: os.Path,
      prefix: AgentOutputPrefix
  ): F[AgentProcessResult] =
    Sync[F].blocking {
      val stderrLines = new java.util.concurrent.ConcurrentLinkedQueue[String]()
      val stderrSink = os.ProcessOutput.Readlines { line =>
        val text = line + "\n"
        stderrLines.add(text)
        os.write.append(logFile, text, createFolders = true)
        Console.err.print(AgentOutputPrefix.prefix(text, prefix, colored = true))
      }
      val result = os.proc(command).call(cwd = root, stdout = os.Pipe, stderr = stderrSink, check = false)
      AgentProcessResult(result.exitCode, result.out.text(), stderrLines.iterator().asScala.mkString)
    }

object Runtime:
  val defaultAdapters: AdapterRegistry =
    AdapterRegistry.default

final case class AgentArgs(
    agent: Boolean,
    inlineAnswers: Option[Answers],
    fresh: Boolean,
    reset: Boolean,
    panes: Boolean,
    resumeSession: Option[SessionId],
    adapter: Option[String]
)

object AgentArgs:

  final case class Parsed(args: AgentArgs, rest: List[String])

  val empty: AgentArgs =
    AgentArgs(
      agent = false,
      inlineAnswers = None,
      fresh = false,
      reset = false,
      panes = false,
      resumeSession = None,
      adapter = None
    )

  def parse(args: List[String]): Either[String, AgentArgs] =
    parseLoop(args, empty, strict = true).map(_.args)

  def parseKnown(args: List[String]): Either[String, Parsed] =
    parseLoop(args, empty, strict = false)

  private def parseLoop(args: List[String], parsed: AgentArgs, strict: Boolean): Either[String, Parsed] =
    args match
      case Nil => Right(Parsed(parsed, Nil))
      case "--agent" :: tail =>
        parseLoop(tail, parsed.copy(agent = true), strict)
      case "--fresh" :: tail =>
        parseLoop(tail, parsed.copy(fresh = true), strict)
      case "--reset" :: tail =>
        parseLoop(tail, parsed.copy(reset = true), strict)
      case "--panes" :: tail =>
        parseLoop(tail, parsed.copy(panes = true), strict)
      case "--answers" :: raw :: tail =>
        AnswerLog.parse(raw) match
          case Some(answers) => parseLoop(tail, parsed.copy(inlineAnswers = Some(answers)), strict)
          case None          => Left("invalid JSON for --answers")
      case "--answers" :: Nil =>
        Left("missing value for --answers")
      case "--resume-session" :: value :: tail =>
        parseLoop(tail, parsed.copy(resumeSession = Some(SessionId(value))), strict)
      case "--resume-session" :: Nil =>
        Left("missing value for --resume-session")
      case "--adapter" :: value :: tail =>
        parseLoop(tail, parsed.copy(adapter = Some(value)), strict)
      case "--adapter" :: Nil =>
        Left("missing value for --adapter")
      case other :: tail =>
        if strict then Left("unknown argument: " + other)
        else parseLoop(tail, parsed, strict).map(rest => rest.copy(rest = other :: rest.rest))

object AgentMain:

  final case class Outcome(stdout: String, stderr: String, exitCode: Int)

  def run[F[_]: Sync](
      args: List[String],
      root: os.Path
  )(program: AgentArgs => F[ProgramSays[ujson.Value]]): F[Outcome] =
    AgentArgs.parse(args) match
      case Left(message) =>
        Sync[F].pure(Outcome("", message, 2))
      case Right(parsed) =>
        prepareAnswerLog(root, parsed) *> run(program(parsed))

  def run[F[_]: Sync](program: F[ProgramSays[ujson.Value]]): F[Outcome] =
    program
        .map(render)
        .recover { case need: ReplayNeedInput =>
          render(need.programSays)
        }
        .recover { case rejected: ReplayRejected =>
          render(rejected.programSays)
        }

  def render(programSays: ProgramSays[ujson.Value]): Outcome =
    Outcome(
      stdout = ProtocolJson.render(programSays),
      stderr = "",
      exitCode = programSays.exitCode
    )

  private def prepareAnswerLog[F[_]: Sync](root: os.Path, args: AgentArgs): F[Unit] =
    val reset = if args.reset then AnswerLog.reset[F](root) else Sync[F].unit
    reset *> persistInlineAnswers(root, args)

  private def persistInlineAnswers[F[_]: Sync](root: os.Path, args: AgentArgs): F[Unit] =
    args.inlineAnswers.fold(Sync[F].unit) { inline =>
      for
        existing <- AnswerLog.read[F](root)
        _ <- AnswerLog.write[F](root, AnswerLog.merge(existing, inline))
      yield ()
    }
