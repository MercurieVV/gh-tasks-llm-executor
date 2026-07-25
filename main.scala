import arrowstep.runtime.AgentMain
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.Ref

/** Body text of a GitHub issue after task metadata has been merged in. */
opaque type IssueBody = String
object IssueBody:
  def apply(value: String): IssueBody = value
  extension (self: IssueBody) def value: String = self

/** Captured terminal output from an executed agent run. */
opaque type AgentOutput = String
object AgentOutput:
  def apply(value: String): AgentOutput = value
  extension (self: AgentOutput) def value: String = self

/** Entry point. Parses arguments, allocates the run-scoped environment, and applies the already-assembled program to
  * it.
  *
  * The program itself is a value built once by `Wiring.businessLogic` out of the arrows in `BusinessLogic.scala`;
  * nothing is composed at run time.
  */
object Main extends IOApp:
  def run(args: List[String]): IO[ExitCode] =
    val input = AppInput(
      os.pwd,
      Cli.parseTaskNumber(args),
      Cli.parseRecursiveFlag(args),
      Cli.parseParallelFlag(args)
    )
    AgentMain
      .run[IO](Cli.removeScriptArgs(args), os.pwd)(_ =>
        Ref[IO]
          .of(Map.empty[TaskNumber, Issue])
          .flatMap(openIssues =>
            Wiring
              .businessLogic[IO]
              .program
              .run(input)
              .run(RunEnv(openIssues))
          )
      )
      .flatMap { outcome =>
        IO.print(outcome.stdout) *>
          IO.pure(ExitCode(outcome.exitCode))
      }
