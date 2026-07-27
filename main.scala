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
    Cli.parseMetricsCommand(args, os.pwd) match
      case Some(command) =>
        IO.blocking(renderMetrics(command)).flatMap(IO.println).as(ExitCode.Success)
      case None =>
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

  private def renderMetrics(command: Cli.MetricsCommand): String =
    val backend = command.backend match
      case TokenMetrics.BackendKind.VictoriaMetrics =>
        TokenMetrics.VictoriaMetricsBackend(command.victoriaUrl)
      case TokenMetrics.BackendKind.Jsonl =>
        TokenMetrics.JsonlTokenMetricsBackend(command.path)
    command.view match
      case Cli.MetricsView.Events =>
        TokenMetrics.renderEvents(backend.query(command.query))
      case Cli.MetricsView.Summary =>
        TokenMetrics.renderSummary(backend.summary(command.query))
      case Cli.MetricsView.Json =>
        ujson.write(
          ujson.Obj(
            "events" -> backend.query(command.query).map { event =>
              ujson.Obj(
                "timestampMillis" -> event.timestampMillis,
                "vendor" -> event.vendor.toString.toLowerCase,
                "taskNumber" -> event.taskNumber.map(number => ujson.Num(number.value)).getOrElse(ujson.Null),
                "model" -> event.model.map(ujson.Str(_)).getOrElse(ujson.Null),
                "scope" -> event.scope,
                "input" -> event.usage.input,
                "output" -> event.usage.output,
                "cacheRead" -> event.usage.cacheRead,
                "cacheWrite" -> event.usage.cacheWrite,
                "total" -> event.usage.total
              )
            }
          )
        )
