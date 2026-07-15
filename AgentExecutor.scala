import cats.effect.kernel.Sync
import cats.syntax.all.*

final class AgentExecutor[F[_]](using F: Sync[F]):

  def run(runner: TaskRunner, prompt: String, cwd: os.Path): F[String] =
    F.blocking {
      val result = os
        .proc(runner.command(prompt))
        .call(cwd = cwd, stdout = os.Pipe, stderr = os.Pipe, check = false)
      val stdout = result.out.text()
      val stderr = result.err.text()
      val output = stdout + stderr
      if output.nonEmpty then Console.err.print(output)
      Either.cond(
        result.exitCode === 0,
        output,
        new RuntimeException(
          s"${runner.agent} exited with ${result.exitCode}"
        )
      )
    }.rethrow

object AgentExecutor:
  def apply[F[_]: Sync]: AgentExecutor[F] = new AgentExecutor[F]
