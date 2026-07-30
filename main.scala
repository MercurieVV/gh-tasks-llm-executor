package com.github.mercurievv.ghllm

import com.github.mercurievv.ghllm.*
import com.github.mercurievv.ghllm.agent.*
import com.github.mercurievv.ghllm.arrow.*
import com.github.mercurievv.ghllm.cli.*
import com.github.mercurievv.ghllm.git.*
import com.github.mercurievv.ghllm.metrics.*
import com.github.mercurievv.ghllm.task.*

import arrowstep.runtime.AgentMain
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.Ref
import cats.syntax.all.*

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
        Cli.parseEstimateCommand(args, os.pwd) match
          case Some(command) => runEstimate(command)
          case None          => runProgram(args)

  private def runProgram(args: List[String]): IO[ExitCode] =
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

  /** Prices a plan without running it. Read-only: it fetches issues and folds
    * the same dependency tree the executor would walk, and stops there.
    */
  private def runEstimate(command: Cli.EstimateCommand): IO[ExitCode] =
    val root = os.pwd
    for
      context <- Impl.resolveContext[IO].run(AppInput(root, Some(command.task), Recursive(true), ParallelExecution(false)))
      rawIssues <- GitHub.fetchIssues[IO](root)
      issues <- rawIssues.traverse(Impl.effectiveIssue[IO](root, _))
      target = issues.find(_.number === command.task)
      exit <- target match
        case None =>
          IO.println(s"Task #${command.task.value} is not an open issue.").as(ExitCode.Error)
        case Some(issue) =>
          for
            openIssues <- Ref[IO].of(issues.map(task => task.number -> task).toMap)
            backend <- IO.blocking(metricsBackend(command.backend, command.path, command.victoriaUrl))
            events <- IO.blocking(backend.query(TokenMetrics.TokenMetricsQuery(limit = None)))
            annotated <- PlanEstimate
              .annotate[IO](
                TaskNode(context, issue),
                command.costModel,
                NodeProfiles.fromEvents(events, root)
              )
              .run(RunEnv(openIssues))
            _ <- IO.println(PlanEstimate.render(annotated, command.costModel))
          yield ExitCode.Success
    yield exit

  private def metricsBackend(
      kind: TokenMetrics.BackendKind,
      path: os.Path,
      victoriaUrl: String
  ): TokenMetrics.TokenMetricsBackend =
    kind match
      case TokenMetrics.BackendKind.VictoriaMetrics => TokenMetrics.VictoriaMetricsBackend(victoriaUrl)
      case TokenMetrics.BackendKind.Jsonl           => TokenMetrics.JsonlTokenMetricsBackend(path)

  private def renderMetrics(command: Cli.MetricsCommand): String =
    val backend = metricsBackend(command.backend, command.path, command.victoriaUrl)
    command.view match
      case Cli.MetricsView.Events =>
        TokenMetrics.renderEvents(backend.query(command.query))
      case Cli.MetricsView.Summary =>
        TokenMetrics.renderSummary(backend.summary(command.query))
      case Cli.MetricsView.Readiness =>
        TokenMetrics.renderReadiness(backend.query(command.query))
      case Cli.MetricsView.Json =>
        // Delegated, not restated: this branch used to carry its own copy of the
        // encoding, and that copy silently lacked every field added after it.
        ujson.write(
          ujson.Obj("events" -> backend.query(command.query).map(TokenMetrics.eventJson))
        )
