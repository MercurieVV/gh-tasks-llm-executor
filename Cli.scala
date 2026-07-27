import cats.syntax.all.*

import scala.annotation.tailrec
import scala.util.Try

/** Command-line surface: raw `args` in, typed program input out. */
object Cli:
  enum MetricsView:
    case Events, Summary, Json

  final case class MetricsCommand(
      path: os.Path,
      query: TokenMetrics.TokenMetricsQuery,
      view: MetricsView
  )

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
        case "metrics" :: _ =>
          loop(Nil, clean)
        case head :: tail =>
          loop(tail, head :: clean)

    loop(args, Nil)

  private def parseMetricsOptions(args: List[String], root: os.Path): MetricsCommand =
    val view =
      if args.contains("--json") then MetricsView.Json
      else if args.contains("summary") || args.contains("--summary") then MetricsView.Summary
      else MetricsView.Events

    val path = optionValue(args, "--path")
      .map(value => os.Path(value, root))
      .getOrElse(TokenMetrics.JsonlTokenMetricsBackend.defaultPath(root))

    MetricsCommand(
      path = path,
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
