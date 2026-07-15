import cats.effect.kernel.Sync
import java.time.Instant

object TaskLogger:

  def script[F[_]: Sync](message: String): F[Unit] =
    log("script", message)

  def llm[F[_]: Sync](message: String): F[Unit] =
    log("llm", message)

  def trace[F[_]: Sync](message: String): F[Unit] =
    log("trace", message)

  def unsafeTrace(message: String): Unit =
    write("trace", message)

  def unsafeLlm(message: String): Unit =
    write("llm", message)

  private def log[F[_]: Sync](source: String, message: String): F[Unit] =
    Sync[F].blocking(write(source, message))

  private def write(source: String, message: String): Unit =
    val logDir = os.pwd / ".gh-tasks-llm-executor" / "logs"
    val logFile = logDir / "executor.log"
    os.makeDir.all(logDir)
    val prefixed = message.linesIterator
      .map(line => s"${Instant.now()} [$source] $line")
      .mkString(System.lineSeparator())
    os.write.append(logFile, prefixed + System.lineSeparator())
