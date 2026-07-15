import cats.effect.kernel.Sync

object TaskLogger:

  def script[F[_]: Sync](message: String): F[Unit] =
    log("script", message)

  def llm[F[_]: Sync](message: String): F[Unit] =
    log("llm", message)

  def trace[F[_]: Sync](message: String): F[Unit] =
    log("trace", message)

  private def log[F[_]: Sync](source: String, message: String): F[Unit] =
    Sync[F].delay {
      val prefixed = message.linesIterator
        .map(line => s"[$source] $line")
        .mkString(System.lineSeparator())
      Console.err.println(prefixed)
    }
