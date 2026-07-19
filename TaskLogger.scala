import cats.data.Kleisli
import cats.effect.kernel.Sync
import cats.syntax.all.*
import java.time.Instant
import java.util.concurrent.atomic.AtomicReference
import java.util.regex.Pattern
import scala.collection.immutable.VectorMap
import scala.util.Try

object TaskLogger:

  enum LogLevel:
    case Normal, Verbose

  final case class ConsoleConfig(
      logLevel: LogLevel,
      stickyPattern: Option[Pattern],
      terminalWidth: Int,
      isTty: Boolean
  )

  final case class LoadedConsoleConfig(
      config: ConsoleConfig,
      warning: Option[String]
  )

  object ConsoleConfig:
    private val LogLevelVariable = "GH_TASKS_LOG_LEVEL"
    private val StickyRegexVariable = "GH_TASKS_STICKY_REGEX"
    private val TestColumnsVariable = "GH_TASKS_TERM_COLUMNS"
    private val ColumnsVariable = "COLUMNS"
    private val DefaultTerminalWidth = 80

    def load(
        environment: collection.Map[String, String],
        isTty: Boolean
    ): LoadedConsoleConfig =
      val logLevel = environment
        .get(LogLevelVariable)
        .map(value => asciiLower(value.trim)) match
        case Some("verbose") => LogLevel.Verbose
        case _               => LogLevel.Normal
      val stickyPattern = configuredStickyPattern(environment)
      val terminalWidth = environment
        .get(TestColumnsVariable)
        .flatMap(parsePositiveDecimal)
        .orElse(
          environment.get(ColumnsVariable).flatMap(parsePositiveDecimal)
        )
        .getOrElse(DefaultTerminalWidth)
      stickyPattern match
        case Right(pattern) =>
          LoadedConsoleConfig(
            ConsoleConfig(logLevel, pattern, terminalWidth, isTty),
            None
          )
        case Left(reason) =>
          LoadedConsoleConfig(
            ConsoleConfig(logLevel, None, terminalWidth, isTty),
            Some(s"TaskLogger: $StickyRegexVariable disabled: $reason")
          )

    private def configuredStickyPattern(
        environment: collection.Map[String, String]
    ): Either[String, Option[Pattern]] =
      environment
        .get(StickyRegexVariable)
        .filterNot(_.trim.isEmpty) match
        case None => Right(None)
        case Some(expression) =>
          Try(Pattern.compile(expression)).toEither match
            case Left(_) => Left("invalid Java regular expression")
            case Right(pattern) if pattern.matcher("").groupCount() < 1 =>
              Left("regular expression must contain capture group 1")
            case Right(pattern) => Right(Some(pattern))

    private def parsePositiveDecimal(value: String): Option[Int] =
      val trimmed = value.trim
      Option
        .when(
          trimmed.nonEmpty && trimmed.forall(char => char >= '0' && char <= '9')
        )(trimmed)
        .flatMap(_.toIntOption)
        .filter(_ > 0)

    private def asciiLower(value: String): String =
      value.map {
        case char if char >= 'A' && char <= 'Z' => (char + 32).toChar
        case char                               => char
      }

  final case class ConsoleState(
      stickyValues: VectorMap[String, String],
      paintedRows: Int
  )

  object ConsoleState:
    val empty: ConsoleState = ConsoleState(VectorMap.empty, paintedRows = 0)

  final case class ConsoleUpdate(output: String, state: ConsoleState)

  final class ConsoleRenderer(terminalWidth: Int, lineSeparator: String):
    private val ControlSequenceIntroducer =
      Pattern.compile("(?:\u001b\\[|\u009b)[0-?]*[ -/]*[@-~]")
    private val OperatingSystemCommand = Pattern.compile(
      "(?:\u001b\\]|\u009d)(?:(?!\u0007|\u009c|\u001b\\\\)[\\s\\S])*(?:\u0007|\u009c|\u001b\\\\|$)"
    )
    private val Escape = '\u001b'.toInt
    private val Ellipsis = "…"
    private val CursorSequence = "\u001b["

    def stickyKey(pattern: Pattern, rawLine: String): Option[String] =
      val matcher = pattern.matcher(rawLine)
      if matcher.groupCount() >= 1 && matcher.find() then
        Option(matcher.group(1)).filter(_.nonEmpty)
      else None

    def scrolling(state: ConsoleState, value: String): ConsoleUpdate =
      ConsoleUpdate(
        clearPaintedRows(state.paintedRows) + value + lineSeparator +
          paint(state.stickyValues),
        state.copy(paintedRows = state.stickyValues.size)
      )

    def sticky(
        state: ConsoleState,
        key: String,
        value: String
    ): ConsoleUpdate =
      val nextValues =
        state.stickyValues.updated(key, sanitizedStickyValue(value))
      ConsoleUpdate(
        clearPaintedRows(state.paintedRows) + paint(nextValues),
        ConsoleState(nextValues, paintedRows = nextValues.size)
      )

    def sanitizedStickyValue(value: String): String =
      val tabsExpanded = value.replace('\t', ' ')
      val withoutAnsi = OperatingSystemCommand
        .matcher(tabsExpanded)
        .replaceAll("")
      val withoutCsi = ControlSequenceIntroducer
        .matcher(withoutAnsi)
        .replaceAll("")
      val withoutControls = withoutCsi
        .codePoints()
        .toArray
        .iterator
        .filterNot(codePoint =>
          codePoint == Escape || Character.isISOControl(codePoint)
        )
        .map(codePoint => new String(Character.toChars(codePoint)))
        .mkString
      truncate(withoutControls)

    private def truncate(value: String): String =
      val budget = terminalWidth - 1
      val codePoints = value.codePoints().toArray
      if budget <= 0 then ""
      else if codePoints.length <= budget then value
      else if budget == 1 then Ellipsis
      else new String(codePoints, 0, budget - 1) + Ellipsis

    private def clearPaintedRows(rows: Int): String =
      if rows <= 0 then ""
      else
        val cleared = (0 until rows).map { index =>
          val moveDown =
            if index < rows - 1 then s"${CursorSequence}1B" else ""
          s"\r${CursorSequence}2K$moveDown"
        }.mkString
        val returnToFirst =
          if rows > 1 then s"$CursorSequence${rows - 1}A" else ""
        s"$CursorSequence${rows}A$cleared$returnToFirst"

    private def paint(values: VectorMap[String, String]): String =
      values.valuesIterator
        .map(value => s"\r${CursorSequence}2K$value$lineSeparator")
        .mkString

  private enum ConsoleClass:
    case Normal, Trace
    case AgentStream(stream: String, rawLine: String)

  private val writeLock = new Object

  private val consoleConfig =
    val loaded = ConsoleConfig.load(sys.env, System.console() != null)
    writeLock.synchronized {
      loaded.warning.foreach { warning =>
        System.err.print(warning + System.lineSeparator())
        System.err.flush()
      }
    }
    loaded.config

  private val consoleRenderer =
    ConsoleRenderer(consoleConfig.terminalWidth, System.lineSeparator())

  private val consoleState = AtomicReference(ConsoleState.empty)

  def progress[F[_]: Sync, A](message: A => String): Kleisli[F, A, A] =
    Kleisli(a => script[F](message(a)).as(a))

  def trace[F[_]: Sync, A](message: A => String): Kleisli[F, A, A] =
    Kleisli(a => trace[F](message(a)).as(a))

  def script[F[_]: Sync](message: String): F[Unit] =
    log("script", message, ConsoleClass.Normal)

  def llm[F[_]: Sync](message: String): F[Unit] =
    log("llm", message, ConsoleClass.Normal)

  def trace[F[_]: Sync](message: String): F[Unit] =
    log("trace", message, ConsoleClass.Trace)

  def unsafeTrace(message: String): Unit =
    write("trace", message, ConsoleClass.Trace)

  def unsafeLlm(message: String): Unit =
    write("llm", message, ConsoleClass.Normal)

  def unsafeAgentOutput(stream: String, line: String): Unit =
    write("llm", s"agent $stream: $line", ConsoleClass.AgentStream(stream, line))

  def unsafeWriteArtifact(relativePath: os.RelPath, message: String): Unit =
    val file = logDirectory / relativePath
    os.makeDir.all(file / os.up)
    os.write.over(file, message)

  def unsafeAppendArtifact(relativePath: os.RelPath, message: String): Unit =
    val file = logDirectory / relativePath
    os.makeDir.all(file / os.up)
    os.write.append(file, message)

  private def log[F[_]: Sync](
      source: String,
      message: String,
      consoleClass: ConsoleClass
  ): F[Unit] =
    Sync[F].blocking(write(source, message, consoleClass))

  private def write(
      source: String,
      message: String,
      consoleClass: ConsoleClass
  ): Unit =
    writeLock.synchronized {
      val logFile = logDirectory / "executor.log"
      os.makeDir.all(logDirectory)
      val prefixed = message.linesIterator
        .map(line => s"${Instant.now()} [$source] $line")
        .mkString(System.lineSeparator())
      os.write.append(logFile, prefixed + System.lineSeparator())
      consoleOutput(consoleClass, prefixed).foreach { output =>
        System.err.print(output)
        System.err.flush()
      }
    }

  private def consoleOutput(
      consoleClass: ConsoleClass,
      prefixed: String
  ): Option[String] =
    consoleConfig.logLevel match
      case LogLevel.Verbose => Some(prefixed + System.lineSeparator())
      case LogLevel.Normal =>
        consoleClass match
          case ConsoleClass.Normal =>
            if stickyTtyEnabled then
              Some(applyConsoleUpdate(consoleRenderer.scrolling(_, prefixed)))
            else Some(prefixed + System.lineSeparator())
          case ConsoleClass.Trace => None
          case ConsoleClass.AgentStream(stream, rawLine) =>
            val key = consoleConfig.stickyPattern
              .flatMap(pattern => consoleRenderer.stickyKey(pattern, rawLine))
              .orElse(Some(stream))
            key.map { key =>
              if consoleConfig.isTty then
                applyConsoleUpdate(
                  consoleRenderer.sticky(_, key, prefixed)
                )
              else prefixed + System.lineSeparator()
            }

  private def applyConsoleUpdate(
      update: ConsoleState => ConsoleUpdate
  ): String =
    val rendered = update(consoleState.get())
    consoleState.set(rendered.state)
    rendered.output

  private def stickyTtyEnabled: Boolean =
    consoleConfig.isTty && consoleConfig.stickyPattern.nonEmpty

  private def logDirectory: os.Path =
    os.pwd / ".gh-tasks-llm-executor" / "logs"
