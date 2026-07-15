import cats.effect.kernel.Sync
import cats.syntax.all.*

final class AgentExecutor[F[_]](using F: Sync[F]):

  def run(runner: TaskRunner, prompt: String, cwd: os.Path): F[String] =
    for
      _ <- TaskLogger.llm(
        s"Starting agent execution with ${runner.display} in $cwd"
      )
      result <- F.blocking {
        os
          .proc(runner.command(prompt))
          .call(cwd = cwd, stdout = os.Pipe, stderr = os.Pipe, check = false)
      }
      stdout <- F.blocking(result.out.text())
      stderr <- F.blocking(result.err.text())
      output = stdout + stderr
      _ <-
        if output.nonEmpty then TaskLogger.llm(output.trim)
        else TaskLogger.llm("Agent produced no output.")
      _ <- TaskLogger.llm(
        s"Agent execution finished with exit code ${result.exitCode}."
      )
      value <- Either
        .cond(
          result.exitCode === 0,
          output,
          new RuntimeException(
            s"${runner.agent} exited with ${result.exitCode}"
          )
        )
        .liftTo[F]
    yield value

object AgentExecutor:
  def apply[F[_]: Sync]: AgentExecutor[F] = new AgentExecutor[F]
