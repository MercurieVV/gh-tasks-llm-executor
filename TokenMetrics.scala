import scala.util.Try

object TokenMetrics:
  final case class TokenMetricsEvent(
      timestampMillis: Long,
      vendor: TokenUsage.Vendor,
      usage: TokenUsage.TokenSnapshot,
      taskNumber: Option[TaskNumber],
      model: Option[String],
      scope: String
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
    def record(event: TokenMetricsEvent): Unit
    def query(query: TokenMetricsQuery): List[TokenMetricsEvent]

    def summary(query: TokenMetricsQuery): TokenUsage.TokenSnapshot =
      this.query(query.copy(limit = None)).foldLeft(TokenUsage.TokenSnapshot.Zero)(_ + _.usage)

  final class JsonlTokenMetricsBackend(path: os.Path) extends TokenMetricsBackend:
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
      scope = scope
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
