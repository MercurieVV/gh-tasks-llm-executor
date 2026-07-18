import cats.data.Kleisli
import cats.effect.kernel.Sync
import cats.syntax.all.*
import java.time.Instant

object TaskLogger:

  def progress[F[_]: Sync, A](message: A => String): Kleisli[F, A, A] =
    Kleisli(a => script[F](message(a)).as(a))

  def trace[F[_]: Sync, A](message: A => String): Kleisli[F, A, A] =
    Kleisli(a => trace[F](message(a)).as(a))

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

  def unsafeWriteArtifact(relativePath: os.RelPath, message: String): Unit =
    val file = logDirectory / relativePath
    os.makeDir.all(file / os.up)
    os.write.over(file, message)

  def unsafeAppendArtifact(relativePath: os.RelPath, message: String): Unit =
    val file = logDirectory / relativePath
    os.makeDir.all(file / os.up)
    os.write.append(file, message)

  private def log[F[_]: Sync](source: String, message: String): F[Unit] =
    Sync[F].blocking(write(source, message))

  private def write(source: String, message: String): Unit =
    val logFile = logDirectory / "executor.log"
    os.makeDir.all(logDirectory)
    val prefixed = message.linesIterator
      .map(line => s"${Instant.now()} [$source] $line")
      .mkString(System.lineSeparator())
    os.write.append(logFile, prefixed + System.lineSeparator())
    System.err.println(prefixed)

  private def logDirectory: os.Path =
    os.pwd / ".gh-tasks-llm-executor" / "logs"
