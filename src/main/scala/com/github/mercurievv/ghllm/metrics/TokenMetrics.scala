package com.github.mercurievv.ghllm.metrics

import com.github.mercurievv.ghllm.*
import com.github.mercurievv.ghllm.agent.*
import com.github.mercurievv.ghllm.arrow.*
import com.github.mercurievv.ghllm.cli.*
import com.github.mercurievv.ghllm.git.*
import com.github.mercurievv.ghllm.metrics.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.oteljava.OtelJava

import java.net.HttpURLConnection
import java.net.URI
import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import scala.util.Try

object TokenMetrics:
  final case class TokenMetricsEvent(
      timestampMillis: Long,
      vendor: TokenUsage.Vendor,
      usage: TokenUsage.TokenSnapshot,
      taskNumber: Option[TaskNumber],
      model: Option[String],
      scope: String,
      phase: Option[String] = None,
      runner: Option[String] = None,
      turnCount: Option[Int] = None,
      escalated: Boolean = false,
      outcome: Option[String] = None
  )

  final case class TokenMetricsQuery(
      vendor: Option[TokenUsage.Vendor] = None,
      taskNumber: Option[TaskNumber] = None,
      sinceMillis: Option[Long] = None,
      untilMillis: Option[Long] = None,
      limit: Option[Int] = Some(50)
  ):
    def matches(event: TokenMetricsEvent): Boolean =
      vendor.forall(_ == event.vendor) &&
        taskNumber.forall(expected => event.taskNumber.exists(_.value == expected.value)) &&
        sinceMillis.forall(event.timestampMillis >= _) &&
        untilMillis.forall(event.timestampMillis <= _)

  trait TokenMetricsBackend:
    def destination: String
    def record(event: TokenMetricsEvent): Unit
    def query(query: TokenMetricsQuery): List[TokenMetricsEvent]

    def summary(query: TokenMetricsQuery): TokenUsage.TokenSnapshot =
      this.query(query.copy(limit = None)).foldLeft(TokenUsage.TokenSnapshot.Zero)(_ + _.usage)

    def cacheHitRatio(query: TokenMetricsQuery): Double =
      val s = summary(query)
      val denom = s.input + s.cacheRead
      if denom == 0L then 0.0 else s.cacheRead.toDouble / denom.toDouble

    // Observed p: fraction of recorded runs for this (phase, runner) whose
    // outcome was "green". None when the sample is smaller than `minSample`.
    def successRate(phase: String, runner: String, minSample: Int = 20): Option[Double] =
      val allEvents = this.query(TokenMetricsQuery(limit = None))
      val relevant = allEvents.filter(e =>
        e.phase.contains(phase) && e.runner.contains(runner) && e.outcome.isDefined
      )
      val total = relevant.size
      if total < minSample then None
      else
        val greenCount = relevant.count(_.outcome.contains("green"))
        Some(greenCount.toDouble / total.toDouble)

  final class JsonlTokenMetricsBackend(path: os.Path) extends TokenMetricsBackend:
    def destination: String = path.toString

    def record(event: TokenMetricsEvent): Unit =
      os.makeDir.all(path / os.up)
      os.write.append(path, writeEvent(event) + System.lineSeparator(), createFolders = true)

    def query(query: TokenMetricsQuery): List[TokenMetricsEvent] =
      val events =
        if os.exists(path) then Try(os.read.lines(path)).getOrElse(Nil).flatMap(readEvent)
        else Nil
      val matching = events.filter(query.matches)
      query.limit.fold(matching)(limit => matching.takeRight(limit.max(0))).toList

  object JsonlTokenMetricsBackend:
    def defaultPath(root: os.Path = os.pwd): os.Path =
      root / ".gh-tasks-llm-executor" / "token-metrics.jsonl"

  final class VictoriaMetricsBackend(baseUrl: String = VictoriaMetricsBackend.defaultBaseUrl)
      extends TokenMetricsBackend:
    def destination: String = baseUrl

    def record(event: TokenMetricsEvent): Unit =
      Try(VictoriaMetricsBackend.record(event, baseUrl).unsafeRunSync()).fold(_ => (), identity)

    def query(query: TokenMetricsQuery): List[TokenMetricsEvent] =
      VictoriaMetricsBackend.query(query, baseUrl)

  object VictoriaMetricsBackend:
    val MetricName = "gh_tasks_llm_executor_token_usage"
    val PrometheusMetricName = MetricName + "_total"
    val ScalaTextToolCallsMetricName = "scala_text_tool_calls"

    def defaultBaseUrl: String =
      sys.env.get("GH_TASKS_METRICS_VICTORIA_URL").getOrElse("http://localhost:8428")

    def defaultOtlpMetricsEndpoint(baseUrl: String = defaultBaseUrl): String =
      stripTrailingSlash(baseUrl) + "/opentelemetry/v1/metrics"

    private def record(event: TokenMetricsEvent, baseUrl: String): IO[Unit] =
      IO.delay(configureOtel(baseUrl)) *>
        OtelJava.autoConfigured[IO]().use { otel =>
          for
            meter <- otel.meterProvider.get("gh-tasks-llm-executor")
            counter <- meter
              .counter[Long](MetricName)
              .withUnit("{token}")
              .withDescription("Token usage reported by local agent runs")
              .create
            _ <- List(
              "input" -> event.usage.input,
              "output" -> event.usage.output,
              "cache_read" -> event.usage.cacheRead,
              "cache_write" -> event.usage.cacheWrite,
              "total" -> event.usage.total
            ).traverse_ { case (kind, value) =>
              if value > 0 then counter.add(value, attributes(event, kind))
              else IO.unit
            }
          yield ()
        }

    private[metrics] def recordScalaTextToolCalls(
        count: Long,
        runner: String,
        phase: String,
        baseUrl: String
    ): IO[Unit] =
      IO.delay(configureOtel(baseUrl)) *>
        OtelJava.autoConfigured[IO]().use { otel =>
          for
            meter <- otel.meterProvider.get("gh-tasks-llm-executor")
            counter <- meter
              .counter[Long](ScalaTextToolCallsMetricName)
              .withUnit("{call}")
              .withDescription("Shell text-tool calls that target Scala source")
              .create
            _ <- counter.add(
              count,
              List(
                Attribute("runner", runner),
                Attribute("phase", phase)
              )
            )
          yield ()
        }

    private def configureOtel(baseUrl: String): Unit =
      setPropertyIfMissing("otel.service.name", "gh-tasks-llm-executor")
      setPropertyIfMissing("otel.metrics.exporter", "otlp")
      setPropertyIfMissing("otel.traces.exporter", "none")
      setPropertyIfMissing("otel.logs.exporter", "none")
      setPropertyIfMissing("otel.exporter.otlp.metrics.endpoint", defaultOtlpMetricsEndpoint(baseUrl))
      setPropertyIfMissing("otel.exporter.otlp.metrics.protocol", "http/protobuf")
      setPropertyIfMissing("otel.metric.export.interval", "1000")

    private def setPropertyIfMissing(name: String, value: String): Unit =
      if sys.props.get(name).isEmpty && sys.env.get(envName(name)).isEmpty then System.setProperty(name, value)

    private def envName(property: String): String =
      property.toUpperCase.replace('.', '_').replace('-', '_')

    private def attributes(event: TokenMetricsEvent, tokenType: String): List[Attribute[?]] =
      List(
        Some(Attribute("vendor", event.vendor.toString.toLowerCase)),
        Some(Attribute("scope", event.scope)),
        Some(Attribute("token_type", tokenType)),
        event.taskNumber.map(number => Attribute("task", number.value.toLong)),
        event.model.map(model => Attribute("model", model)),
        event.phase.map(p => Attribute("phase", p)),
        event.runner.map(r => Attribute("runner", r)),
        event.turnCount.map(tc => Attribute("turn_count", tc.toLong)),
        Some(Attribute("escalated", event.escalated)),
        event.outcome.map(o => Attribute("outcome", o))
      ).flatten

    private def query(query: TokenMetricsQuery, baseUrl: String): List[TokenMetricsEvent] =
      val selector = buildSelector(query)
      val params = List(
        "match[]" -> selector,
        "start" -> query.sinceMillis.getOrElse(0L).toString,
        "end" -> query.untilMillis.getOrElse(System.currentTimeMillis()).toString
      )
      val response = httpGet(stripTrailingSlash(baseUrl) + "/api/v1/export", params)
      val events = response.toList
        .flatMap(_.linesIterator.toList)
        .flatMap(readExportSeries)
        .groupMapReduce(_.key)(_.event)((left, right) => left.merge(right))
        .values
        .toList
        .sortBy(_.timestampMillis)
      query.limit.fold(events)(limit => events.takeRight(limit.max(0)))

    private final case class ExportPointKey(
        timestampMillis: Long,
        vendor: TokenUsage.Vendor,
        taskNumber: Option[TaskNumber],
        model: Option[String],
        scope: String
    )

    private final case class ExportPoint(key: ExportPointKey, event: TokenMetricsEvent)

    extension (left: TokenMetricsEvent)
      private def merge(right: TokenMetricsEvent): TokenMetricsEvent =
        left.copy(usage = left.usage + right.usage)

    private def readExportSeries(line: String): List[ExportPoint] =
      val parsed =
        for
          json <- Try(ujson.read(line)).toOption
          obj <- json.objOpt
          metric <- obj.get("metric").flatMap(_.objOpt)
          vendor <- metric.get("vendor").flatMap(_.strOpt).flatMap(parseVendor)
          tokenType <- metric.get("token_type").flatMap(_.strOpt)
          scope <- metric.get("scope").flatMap(_.strOpt)
          values <- obj.get("values").flatMap(_.arrOpt).map(_.toList)
          timestamps <- obj.get("timestamps").flatMap(_.arrOpt).map(_.toList)
        yield values.zip(timestamps).flatMap { case (valueJson, timestampJson) =>
          for
            value <- readLong(valueJson)
            timestamp <- readLong(timestampJson)
          yield
            val taskNumber = metric.get("task").flatMap(readLong).map(value => TaskNumber(value.toInt))
            val model = metric.get("model").flatMap(_.strOpt)
            val key = ExportPointKey(timestamp, vendor, taskNumber, model, scope)
            ExportPoint(
              key,
              TokenMetricsEvent(timestamp, vendor, snapshot(tokenType, value), taskNumber, model, scope)
            )
        }
      parsed.getOrElse(Nil)

    private def snapshot(tokenType: String, value: Long): TokenUsage.TokenSnapshot =
      tokenType match
        case "input"  => TokenUsage.TokenSnapshot(input = value, output = 0, cacheRead = 0, cacheWrite = 0, total = 0)
        case "output" => TokenUsage.TokenSnapshot(input = 0, output = value, cacheRead = 0, cacheWrite = 0, total = 0)
        case "cache_read" =>
          TokenUsage.TokenSnapshot(input = 0, output = 0, cacheRead = value, cacheWrite = 0, total = 0)
        case "cache_write" =>
          TokenUsage.TokenSnapshot(input = 0, output = 0, cacheRead = 0, cacheWrite = value, total = 0)
        case "total" => TokenUsage.TokenSnapshot(input = 0, output = 0, cacheRead = 0, cacheWrite = 0, total = value)
        case _       => TokenUsage.TokenSnapshot.Zero

    private def buildSelector(query: TokenMetricsQuery): String =
      val labels = List(
        Some(s"""__name__=~"($MetricName|$PrometheusMetricName)""""),
        query.vendor.map(vendor => s"""vendor="${vendor.toString.toLowerCase}""""),
        query.taskNumber.map(task => s"""task="${task.value}"""")
      ).flatten
      "{" + labels.mkString(",") + "}"

    private def httpGet(url: String, params: List[(String, String)]): Option[String] =
      val encoded = params.map { case (name, value) => encode(name) + "=" + encode(value) }.mkString("&")
      val connection = URI.create(url + "?" + encoded).toURL.openConnection().asInstanceOf[HttpURLConnection]
      Try {
        connection.setRequestMethod("GET")
        connection.setConnectTimeout(1000)
        connection.setReadTimeout(3000)
        val stream =
          if connection.getResponseCode >= 400 then connection.getErrorStream
          else connection.getInputStream
        try scala.io.Source.fromInputStream(stream, "UTF-8").mkString
        finally stream.close()
      }.toOption

    private def encode(value: String): String =
      URLEncoder.encode(value, StandardCharsets.UTF_8)

    private def stripTrailingSlash(value: String): String =
      value.stripSuffix("/")

  enum BackendKind:
    case VictoriaMetrics, Jsonl

  object BackendKind:
    def parse(value: String): Option[BackendKind] =
      value.trim.toLowerCase match
        case "victoria" | "victoriametrics" | "victoria-metrics" => Some(VictoriaMetrics)
        case "jsonl" | "file"                                    => Some(Jsonl)
        case _                                                   => None

  def recordScalaTextToolCalls(
      transcript: String,
      runner: String,
      phase: String,
      baseUrl: String = VictoriaMetricsBackend.defaultBaseUrl
  ): Long =
    val count = TaskLogger.scalaTextToolCallCount(transcript)
    if count > 0 then
      Try(
        VictoriaMetricsBackend
          .recordScalaTextToolCalls(count, runner, phase, baseUrl)
          .unsafeRunSync()
      ).fold(_ => (), identity)
    count

  def defaultBackend(root: os.Path = os.pwd): TokenMetricsBackend =
    backendFromEnv(root)

  def backendFromEnv(root: os.Path = os.pwd): TokenMetricsBackend =
    sys.env
      .get("GH_TASKS_TOKEN_METRICS_BACKEND")
      .flatMap(BackendKind.parse)
      .fold[TokenMetricsBackend](VictoriaMetricsBackend()) {
        case BackendKind.VictoriaMetrics => VictoriaMetricsBackend()
        case BackendKind.Jsonl           => JsonlTokenMetricsBackend(JsonlTokenMetricsBackend.defaultPath(root))
      }

  def backend(kind: BackendKind, root: os.Path, victoriaUrl: String): TokenMetricsBackend =
    kind match
      case BackendKind.VictoriaMetrics => VictoriaMetricsBackend(victoriaUrl)
      case BackendKind.Jsonl           => JsonlTokenMetricsBackend(JsonlTokenMetricsBackend.defaultPath(root))

  def defaultRootForWorktree(cwd: os.Path): os.Path =
    val parent = cwd / os.up
    if parent.last == ".worktrees" then parent / os.up else cwd

  def renderSummary(snapshot: TokenUsage.TokenSnapshot): String =
    List(
      s"input=${snapshot.input}",
      s"output=${snapshot.output}",
      s"cacheRead=${snapshot.cacheRead}",
      s"cacheWrite=${snapshot.cacheWrite}",
      s"total=${snapshot.total}"
    ).mkString(" ")

  def renderEvents(events: List[TokenMetricsEvent]): String =
    if events.isEmpty then "No token metrics found."
    else
      val header = "timestampMillis vendor task model scope input output cacheRead cacheWrite total"
      val rows = events.map { event =>
        List(
          event.timestampMillis.toString,
          event.vendor.toString.toLowerCase,
          event.taskNumber.map(_.value.toString).getOrElse("-"),
          event.model.getOrElse("-"),
          event.scope,
          event.usage.input.toString,
          event.usage.output.toString,
          event.usage.cacheRead.toString,
          event.usage.cacheWrite.toString,
          event.usage.total.toString
        ).mkString(" ")
      }
      (header :: rows).mkString(System.lineSeparator())

  private def writeEvent(event: TokenMetricsEvent): String =
    ujson.write(
      ujson.Obj(
        "timestampMillis" -> ujson.Num(event.timestampMillis.toDouble),
        "vendor" -> event.vendor.toString.toLowerCase,
        "taskNumber" -> event.taskNumber.map(number => ujson.Num(number.value)).getOrElse(ujson.Null),
        "model" -> event.model.map(ujson.Str(_)).getOrElse(ujson.Null),
        "scope" -> event.scope,
        "phase" -> event.phase.fold(ujson.Null: ujson.Value)(ujson.Str(_)),
        "runner" -> event.runner.fold(ujson.Null: ujson.Value)(ujson.Str(_)),
        "turnCount" -> event.turnCount.map(count => ujson.Num(count.toDouble)).getOrElse(ujson.Null),
        "escalated" -> ujson.Bool(event.escalated),
        "outcome" -> event.outcome.map(ujson.Str(_)).getOrElse(ujson.Null),
        "usage" -> ujson.Obj(
          "input" -> ujson.Num(event.usage.input.toDouble),
          "output" -> ujson.Num(event.usage.output.toDouble),
          "cacheRead" -> ujson.Num(event.usage.cacheRead.toDouble),
          "cacheWrite" -> ujson.Num(event.usage.cacheWrite.toDouble),
          "total" -> ujson.Num(event.usage.total.toDouble)
        )
      )
    )

  private def readEvent(line: String): Option[TokenMetricsEvent] =
    for
      json <- Try(ujson.read(line)).toOption
      obj <- json.objOpt
      timestampMillis <- obj.get("timestampMillis").flatMap(readLong)
      vendor <- obj.get("vendor").flatMap(_.strOpt).flatMap(parseVendor)
      usage <- obj.get("usage").flatMap(readSnapshot)
      scope <- obj.get("scope").flatMap(_.strOpt)
    yield TokenMetricsEvent(
      timestampMillis = timestampMillis,
      vendor = vendor,
      usage = usage,
      taskNumber = obj.get("taskNumber").flatMap(readLong).map(value => TaskNumber(value.toInt)),
      model = obj.get("model").flatMap(_.strOpt),
      scope = scope,
      phase = obj.get("phase").flatMap(_.strOpt),
      runner = obj.get("runner").flatMap(_.strOpt),
      turnCount = obj.get("turnCount").flatMap(readLong).map(_.toInt),
      escalated = obj.get("escalated").flatMap(_.boolOpt).getOrElse(false),
      outcome = obj.get("outcome").flatMap(_.strOpt)
    )

  private def readSnapshot(json: ujson.Value): Option[TokenUsage.TokenSnapshot] =
    for
      obj <- json.objOpt
      input <- obj.get("input").flatMap(readLong)
      output <- obj.get("output").flatMap(readLong)
      cacheRead <- obj.get("cacheRead").flatMap(readLong)
      cacheWrite <- obj.get("cacheWrite").flatMap(readLong)
      total <- obj.get("total").flatMap(readLong)
    yield TokenUsage.TokenSnapshot(input, output, cacheRead, cacheWrite, total)

  private def readLong(json: ujson.Value): Option[Long] =
    json.numOpt.map(_.toLong).orElse(json.strOpt.flatMap(_.toLongOption))

  def parseVendor(value: String): Option[TokenUsage.Vendor] =
    TokenUsage.Vendor.values.find(_.toString.equalsIgnoreCase(value.trim))
