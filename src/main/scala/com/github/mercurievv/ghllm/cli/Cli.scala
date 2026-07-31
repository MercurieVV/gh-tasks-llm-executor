package com.github.mercurievv.ghllm.cli

import com.github.mercurievv.ghllm.*
import com.github.mercurievv.ghllm.agent.*
import com.github.mercurievv.ghllm.arrow.*
import com.github.mercurievv.ghllm.cli.*
import com.github.mercurievv.ghllm.git.*
import com.github.mercurievv.ghllm.metrics.*

import cats.syntax.all.*

import scala.annotation.tailrec
import scala.util.Try

/** Command-line surface: raw `args` in, typed program input out. */
object Cli:
  enum MetricsView:
    case Events, Summary, Json, Readiness

  final case class MetricsCommand(
      path: os.Path,
      backend: TokenMetrics.BackendKind,
      victoriaUrl: String,
      query: TokenMetrics.TokenMetricsQuery,
      view: MetricsView
  )

  /** `estimate --task=N` — prices a plan before running it.
    *
    * Prices are flags rather than a built-in table: a wrong hardcoded price
    * would still print a confident dollar figure. The rendered report repeats
    * the prices it used.
    */
  final case class EstimateCommand(
      task: TaskNumber,
      path: os.Path,
      backend: TokenMetrics.BackendKind,
      victoriaUrl: String,
      costModel: TaskTree.CostModel
  )

  val DefaultInputUsdPerMillionTokens = 3.0
  val DefaultOutputUsdPerMillionTokens = 15.0

  def parseEstimateCommand(args: List[String], root: os.Path = os.pwd): Option[EstimateCommand] =
    args match
      case "estimate" :: rest =>
        parseTaskNumber(rest).map { task =>
          EstimateCommand(
            task = task,
            path = optionValue(rest, "--path")
              .map(value => os.Path(value, root))
              .getOrElse(TokenMetrics.JsonlTokenMetricsBackend.defaultPath(root)),
            backend = optionValue(rest, "--backend")
              .flatMap(TokenMetrics.BackendKind.parse)
              .orElse(
                sys.env.get("GH_TASKS_TOKEN_METRICS_BACKEND").flatMap(TokenMetrics.BackendKind.parse)
              )
              .getOrElse(TokenMetrics.BackendKind.VictoriaMetrics),
            victoriaUrl = optionValue(rest, "--victoria-url")
              .orElse(sys.env.get("GH_TASKS_METRICS_VICTORIA_URL"))
              .getOrElse(TokenMetrics.VictoriaMetricsBackend.defaultBaseUrl),
            costModel = TaskTree.CostModel(
              inputUsdPerMillionTokens = positiveDouble(rest, "--input-usd-per-mtok")
                .getOrElse(DefaultInputUsdPerMillionTokens),
              outputUsdPerMillionTokens = positiveDouble(rest, "--output-usd-per-mtok")
                .getOrElse(DefaultOutputUsdPerMillionTokens)
            )
          )
        }
      case _ => None

  /** `predict-and-run --task=N` — same options as `estimate`, but actually runs the task's dependency tree to
    * closure afterward and reports the same cost model's estimate against what the run actually billed. See
    * `PredictedVsActualExecution`: unlike `estimate`, this is not read-only.
    */
  final case class PredictAndRunCommand(
      task: TaskNumber,
      path: os.Path,
      backend: TokenMetrics.BackendKind,
      victoriaUrl: String,
      costModel: TaskTree.CostModel
  )

  def parsePredictAndRunCommand(args: List[String], root: os.Path = os.pwd): Option[PredictAndRunCommand] =
    args match
      case "predict-and-run" :: rest =>
        parseEstimateCommand("estimate" :: rest, root).map { estimate =>
          PredictAndRunCommand(
            task = estimate.task,
            path = estimate.path,
            backend = estimate.backend,
            victoriaUrl = estimate.victoriaUrl,
            costModel = estimate.costModel
          )
        }
      case _ => None

  private def positiveDouble(args: List[String], name: String): Option[Double] =
    optionValue(args, name).flatMap(_.toDoubleOption).filter(value => value.isFinite && value >= 0.0)

  def parseTaskNumber(args: List[String]): Option[TaskNumber] =
    args
      .collectFirst {
        case value if value.startsWith("--task=") =>
          value.stripPrefix("--task=")
        case value if value.startsWith("--issue=") =>
          value.stripPrefix("--issue=")
      }
      .orElse {
        args.sliding(2).collectFirst { case List("--task" | "--issue", value) =>
          value
        }
      }
      .flatMap(_.trim.stripPrefix("#").toIntOption)
      .map(TaskNumber(_))

  def parseRecursiveFlag(args: List[String]): Recursive =
    Recursive(args.contains("--recursive"))

  def parseParallelFlag(args: List[String]): ParallelExecution =
    ParallelExecution(args.contains("--parallel"))

  def parseMetricsCommand(args: List[String], root: os.Path = os.pwd): Option[MetricsCommand] =
    args match
      case "metrics" :: rest =>
        Some(parseMetricsOptions(rest, root))
      case _ => None

  def removeScriptArgs(args: List[String]): List[String] =
    @tailrec
    def loop(
        remaining: List[String],
        clean: List[String]
    ): List[String] =
      remaining match
        case Nil => clean.reverse
        case ("--executor" | "--llm" | "--agent" | "--model" | "--task" | "--issue") :: _ :: tail =>
          loop(tail, clean)
        case flag :: Nil
            if flag === "--executor" || flag === "--llm" ||
              flag === "--agent" || flag === "--model" ||
              flag === "--task" || flag === "--issue" =>
          loop(Nil, clean)
        case flag :: tail if flag.startsWith("--task=") || flag.startsWith("--issue=") =>
          loop(tail, clean)
        case "--recursive" :: tail =>
          loop(tail, clean)
        case "--parallel" :: tail =>
          loop(tail, clean)
        case ("metrics" | "estimate" | "predict-and-run") :: _ =>
          loop(Nil, clean)
        case head :: tail =>
          loop(tail, head :: clean)

    loop(args, Nil)

  private def parseMetricsOptions(args: List[String], root: os.Path): MetricsCommand =
    val view =
      if args.contains("--json") then MetricsView.Json
      else if args.contains("summary") || args.contains("--summary") then MetricsView.Summary
      else if args.contains("readiness") || args.contains("--readiness") then MetricsView.Readiness
      else MetricsView.Events

    val path = optionValue(args, "--path")
      .map(value => os.Path(value, root))
      .getOrElse(TokenMetrics.JsonlTokenMetricsBackend.defaultPath(root))
    val backend = optionValue(args, "--backend")
      .flatMap(TokenMetrics.BackendKind.parse)
      .orElse(sys.env.get("GH_TASKS_TOKEN_METRICS_BACKEND").flatMap(TokenMetrics.BackendKind.parse))
      .getOrElse(TokenMetrics.BackendKind.VictoriaMetrics)
    val victoriaUrl = optionValue(args, "--victoria-url")
      .orElse(sys.env.get("GH_TASKS_METRICS_VICTORIA_URL"))
      .getOrElse(TokenMetrics.VictoriaMetricsBackend.defaultBaseUrl)

    MetricsCommand(
      path = path,
      backend = backend,
      victoriaUrl = victoriaUrl,
      query = TokenMetrics.TokenMetricsQuery(
        vendor = optionValue(args, "--vendor").flatMap(TokenMetrics.parseVendor),
        taskNumber = optionValue(args, "--task").orElse(optionValue(args, "--issue")).flatMap(parseNumber),
        sinceMillis = optionValue(args, "--since").flatMap(_.toLongOption),
        untilMillis = optionValue(args, "--until").flatMap(_.toLongOption),
        limit = optionValue(args, "--limit").flatMap(_.toIntOption).orElse(Some(50))
      ),
      view = view
    )

  private def optionValue(args: List[String], name: String): Option[String] =
    args
      .collectFirst {
        case value if value.startsWith(name + "=") =>
          value.stripPrefix(name + "=")
      }
      .orElse {
        args.sliding(2).collectFirst {
          case List(flag, value) if flag === name =>
            value
        }
      }

  private def parseNumber(value: String): Option[TaskNumber] =
    value.trim.stripPrefix("#").toIntOption.map(TaskNumber(_))

  def envLong(name: String, fallback: Long): Long =
    sys.env
      .get(name)
      .flatMap(value => Try(value.trim.toLong).toOption)
      .filter(_ > 0)
      .getOrElse(fallback)

  def envBoolean(name: String, fallback: Boolean): Boolean =
    sys.env
      .get(name)
      .map(value => Set("1", "true", "yes", "on").contains(value.trim.toLowerCase))
      .getOrElse(fallback)

  def fetchOriginEnabled: Boolean = envBoolean("GH_TASKS_FETCH_ORIGIN", false)
