import scala.util.Try
import java.util.concurrent.atomic.AtomicReference

// Vendor-agnostic "tokens used since session start" probe. Two source
// strategies, mirroring what's actually available per vendor (see
// VendorBudgets for the same split, applied there to quota fractions instead
// of raw counts):
//
//   - claude/codex: LOG-DERIVED. Both CLIs write an append-only local
//     transcript, so `current()` re-reads it fresh every call - always
//     correct, even if the process restarts mid-session.
//   - gemini/deepseek: SELF-ACCUMULATING. Neither exposes a queryable
//     cumulative counter (gemini/agy's session db stores an opaque protobuf
//     blob with no token field; deepseek has no session concept at all -
//     only per-call `usage` in the API response). The caller must feed each
//     observed `usage` into `record()` as it parses responses; `current()`
//     just returns the running total. Resets on process restart - there's no
//     external source of truth to recompute it from.
//
// Callers that want per-action token cost call `measure` around the action:
// it snapshots before/after and returns the delta, leaving what to do with
// that delta (log it, push it to a metrics backend, ...) to the caller.
object TokenUsage:

  enum Vendor:
    case Claude, Codex, Gemini, Deepseek

  final case class TokenSnapshot(
      input: Long,
      output: Long,
      cacheRead: Long,
      cacheWrite: Long,
      total: Long
  ):
    def -(other: TokenSnapshot): TokenSnapshot =
      TokenSnapshot(
        input = input - other.input,
        output = output - other.output,
        cacheRead = cacheRead - other.cacheRead,
        cacheWrite = cacheWrite - other.cacheWrite,
        total = total - other.total
      )
    def +(other: TokenSnapshot): TokenSnapshot =
      TokenSnapshot(
        input = input + other.input,
        output = output + other.output,
        cacheRead = cacheRead + other.cacheRead,
        cacheWrite = cacheWrite + other.cacheWrite,
        total = total + other.total
      )

  object TokenSnapshot:
    val Zero: TokenSnapshot = TokenSnapshot(0, 0, 0, 0, 0)

  trait TokenUsageSource:
    // Cumulative usage since session start, as of right now. None if the
    // source can't be located (no session file yet, nothing accumulated).
    def current(): Option[TokenSnapshot]

  // Runs `action`, returns its result plus the token delta consumed by it
  // (None if snapshot unavailable before or after - e.g. no session file yet).
  def measure[A](source: TokenUsageSource)(action: => A): (A, Option[TokenSnapshot]) =
    val before = source.current()
    val result = action
    val after = source.current()
    val delta = for b <- before; a <- after yield a - b
    (result, delta)

  private def field(json: ujson.Value, key: String): Option[ujson.Value] =
    json.objOpt.flatMap(_.get(key))

  private def latestByMtime(dir: os.Path, ext: String): Option[os.Path] =
    if !os.exists(dir) then None
    else Try(os.walk(dir).filter(p => os.isFile(p) && p.ext == ext).maxByOption(os.mtime)).toOption.flatten

  // claude: sums the `usage` block of every line in the current session's
  // transcript. Each line is one turn's usage (not itself cumulative), so
  // summing across all lines gives "since session start".
  final class ClaudeTokenUsageSource(cwd: os.Path = os.pwd) extends TokenUsageSource:
    private def projectDir: os.Path =
      os.home / ".claude" / "projects" / cwd.toString.replaceAll("[/.]", "-")

    private def sessionFile: Option[os.Path] = latestByMtime(projectDir, "jsonl")

    def current(): Option[TokenSnapshot] =
      sessionFile.flatMap { path =>
        val lines = Try(os.read.lines(path)).getOrElse(Nil)
        val snapshots = lines.flatMap { line =>
          for
            json <- Try(ujson.read(line)).toOption
            usage <- field(json, "message").flatMap(field(_, "usage"))
            inputTokens <- field(usage, "input_tokens").flatMap(_.numOpt)
            outputTokens <- field(usage, "output_tokens").flatMap(_.numOpt)
          yield TokenSnapshot(
            input = inputTokens.toLong,
            output = outputTokens.toLong,
            cacheRead = field(usage, "cache_read_input_tokens").flatMap(_.numOpt).getOrElse(0.0).toLong,
            cacheWrite = field(usage, "cache_creation_input_tokens").flatMap(_.numOpt).getOrElse(0.0).toLong,
            total = 0L
          )
        }
        if snapshots.isEmpty then None
        else
          val summed = snapshots.foldLeft(TokenSnapshot.Zero)(_ + _)
          Some(summed.copy(total = summed.input + summed.output + summed.cacheRead + summed.cacheWrite))
      }

  // codex: the last `token_count` event already carries a cumulative
  // `total_token_usage` for the session, no summing needed.
  final class CodexTokenUsageSource(sessionsDir: os.Path = os.home / ".codex" / "sessions") extends TokenUsageSource:
    private def sessionFile: Option[os.Path] = latestByMtime(sessionsDir, "jsonl")

    def current(): Option[TokenSnapshot] =
      sessionFile.flatMap { path =>
        Try(os.read.lines(path)).getOrElse(Nil).reverseIterator
          .flatMap(line => Try(ujson.read(line)).toOption)
          .flatMap(parseLine)
          .nextOption()
      }

    private def parseLine(json: ujson.Value): Option[TokenSnapshot] =
      for
        payload <- field(json, "payload")
        payloadType <- field(payload, "type").flatMap(_.strOpt)
        if payloadType == "token_count"
        info <- field(payload, "info")
        usage <- field(info, "total_token_usage")
        inputTokens <- field(usage, "input_tokens").flatMap(_.numOpt)
        outputTokens <- field(usage, "output_tokens").flatMap(_.numOpt)
        totalTokens <- field(usage, "total_tokens").flatMap(_.numOpt)
      yield TokenSnapshot(
        input = inputTokens.toLong,
        output = outputTokens.toLong,
        cacheRead = field(usage, "cached_input_tokens").flatMap(_.numOpt).getOrElse(0.0).toLong,
        cacheWrite = field(usage, "cache_write_input_tokens").flatMap(_.numOpt).getOrElse(0.0).toLong,
        total = totalTokens.toLong
      )

  // gemini/agy and deepseek: no external transcript to derive from. Caller
  // must call `record` with each response's usage as it observes them;
  // `current` just reports the running total. Thread-safe (CAS loop) so it's
  // safe to share one instance across concurrent calls within a process.
  final class AccumulatingTokenUsageSource extends TokenUsageSource:
    private val total = new AtomicReference[TokenSnapshot](TokenSnapshot.Zero)

    def record(usage: TokenSnapshot): Unit =
      total.updateAndGet(_ + usage)
      ()

    def current(): Option[TokenSnapshot] =
      Some(total.get())