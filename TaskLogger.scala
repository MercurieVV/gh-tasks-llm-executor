import cats.data.Kleisli
import cats.effect.kernel.Sync
import cats.syntax.all.*
import java.time.Instant
import java.util.Locale

object TaskLogger:

  private enum ConsoleClass:
    case Normal, Trace, AgentStream

  private enum LogLevel:
    case Normal, Verbose

  private val logLevel =
    sys.env
      .get("GH_TASKS_LOG_LEVEL")
      .map(_.trim.toLowerCase(Locale.ROOT)) match
      case Some("verbose") => LogLevel.Verbose
      case _               => LogLevel.Normal

  private val writeLock = new Object

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
    write("llm", s"agent $stream: $line", ConsoleClass.AgentStream)

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
      if shouldPrintToConsole(consoleClass) then System.err.println(prefixed)
    }

  private def shouldPrintToConsole(consoleClass: ConsoleClass): Boolean =
    logLevel == LogLevel.Verbose || consoleClass == ConsoleClass.Normal

  private def logDirectory: os.Path =
    os.pwd / ".gh-tasks-llm-executor" / "logs"
