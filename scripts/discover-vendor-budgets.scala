//> using scala 3.8.4
//> using dep com.lihaoyi::os-lib:0.11.8
//> using dep com.lihaoyi::ujson:4.4.3

import scala.util.Try

// One entry per vendor. usedFraction is always 0..1, normalized so
// AgentInventory can read it without knowing each vendor's native units
// (percent, tokens, EUR balance, ...). `estimate=true` marks vendors with no
// authoritative server-side quota signal (self-imposed cap over observed
// spend), vs codex/gemini which report real server-side numbers.
final case class VendorBudget(
    vendor: String,
    usedFraction: Double,
    estimate: Boolean,
    scope: String,
    source: String,
    resetAtEpochSeconds: Option[Long] = None,
    extra: Map[String, ujson.Value] = Map.empty
)

def commandOutput(command: Seq[String]): Option[String] =
  Try(
    os.proc(command).call(stdout = os.Pipe, stderr = os.Pipe, check = false).out.text().trim
  ).toOption.filter(_.nonEmpty)

def envDouble(name: String, fallback: Double): Double =
  sys.env.get(name).flatMap(value => Try(value.toDouble).toOption).getOrElse(fallback)

def field(json: ujson.Value, key: String): Option[ujson.Value] =
  json.objOpt.flatMap(_.get(key))

// --- codex: authoritative, from the CLI's own session transcripts ---
// Every turn appends a token_count event carrying the server's rate-limit
// state (used_percent/window/reset), so no network call is needed at all.
def readCodexBudget(): Option[VendorBudget] =
  val sessionsDir = os.home / ".codex" / "sessions"
  if !os.exists(sessionsDir) then None
  else
    val recentFiles = Try(
      os.walk(sessionsDir)
        .filter(p => os.isFile(p) && p.ext == "jsonl")
        .sortBy(p => -os.mtime(p))
        .take(5)
    ).getOrElse(Nil)
    recentFiles.iterator.flatMap(findLastRateLimit).nextOption()

private def findLastRateLimit(path: os.Path): Option[VendorBudget] =
  Try(os.read.lines(path)).toOption.flatMap { lines =>
    lines.reverseIterator
      .flatMap(line => Try(ujson.read(line)).toOption)
      .flatMap(parseCodexLine)
      .nextOption()
  }

private def parseCodexLine(json: ujson.Value): Option[VendorBudget] =
  for
    payload <- field(json, "payload")
    payloadType <- field(payload, "type").flatMap(_.strOpt)
    if payloadType == "token_count"
    rateLimits <- field(payload, "rate_limits")
    primary <- field(rateLimits, "primary")
    usedPercent <- field(primary, "used_percent").flatMap(_.numOpt)
  yield
    val windowMinutes = field(primary, "window_minutes").flatMap(_.numOpt)
    val resetsAt = field(primary, "resets_at").flatMap(_.numOpt).map(_.toLong)
    VendorBudget(
      vendor = "codex",
      usedFraction = math.min(1.0, math.max(0.0, usedPercent / 100.0)),
      estimate = false,
      scope = windowMinutes.map(m => s"${m.toLong}-minute-window").getOrElse("rate-limit-window"),
      source = "local codex session transcript (rate_limits)",
      resetAtEpochSeconds = resetsAt,
      extra = Map("planType" -> field(rateLimits, "plan_type").getOrElse(ujson.Null))
    )

// --- claude: no authoritative server-side signal, so this is a self-imposed
// cap over observed session cost via ccusage (reads local transcripts, no
// network). Marked estimate=true so it never carries as much weight as a
// vendor-reported number would. ---
def readClaudeBudget(): Option[VendorBudget] =
  val capUsd = envDouble("AGENT_RUNNER_CLAUDE_SESSION_BUDGET_USD", 25.0)
  for
    raw <- commandOutput(Seq("npx", "--yes", "ccusage@latest", "blocks", "--active", "--json"))
    json <- Try(ujson.read(raw)).toOption
    blocks <- field(json, "blocks").flatMap(_.arrOpt)
    block <- blocks.headOption
    costUsd <- field(block, "costUSD").flatMap(_.numOpt)
  yield
    val remainingMinutes =
      field(block, "projection").flatMap(field(_, "remainingMinutes")).flatMap(_.numOpt)
    VendorBudget(
      vendor = "claude",
      usedFraction = math.min(1.0, math.max(0.0, costUsd / capUsd)),
      estimate = true,
      scope = "5h-session (self-imposed cost cap, no vendor quota API)",
      source = s"ccusage blocks --active (cap=$$$capUsd/session)",
      extra = Map(
        "sessionCostUsd" -> ujson.Num(costUsd),
        "remainingMinutes" -> remainingMinutes.fold[ujson.Value](ujson.Null)(ujson.Num(_))
      )
    )

// --- gemini: authoritative, via gcloud quota surface (Vertex-backed
// projects only; plain AI Studio keys have no equivalent CLI signal). ---
def readGeminiBudget(): Option[VendorBudget] =
  for
    project <- sys.env.get("GOOGLE_CLOUD_PROJECT").filter(_.nonEmpty)
    raw <- commandOutput(
      Seq(
        "gcloud",
        "alpha",
        "services",
        "quota",
        "list",
        "--service=aiplatform.googleapis.com",
        s"--consumer=projects/$project",
        "--format=json"
      )
    )
    json <- Try(ujson.read(raw)).toOption
    entries <- json.arrOpt
    fractions = entries.toList.flatMap(entryUsedFraction)
    if fractions.nonEmpty
  yield VendorBudget(
    vendor = "gemini",
    usedFraction = fractions.max,
    estimate = false,
    scope = "gcp-quota (tightest metric)",
    source = s"gcloud alpha services quota list (project=$project)"
  )

private def entryUsedFraction(entry: ujson.Value): Option[Double] =
  for
    limit <- field(entry, "limit").flatMap(_.numOpt).filter(_ > 0)
    usage <- field(entry, "usage").flatMap(_.numOpt)
  yield math.min(1.0, math.max(0.0, usage / limit))

// --- deepseek: real prepaid balance from the vendor, spend tracked against
// a self-imposed monthly cap (DeepSeek has no concept of "your" monthly
// budget - only a running wallet total). Carries the previous run's
// month-start snapshot forward across invocations. ---
def readDeepseekBudget(previous: Option[ujson.Value]): Option[VendorBudget] =
  val capEur = envDouble("AGENT_RUNNER_DEEPSEEK_MONTHLY_BUDGET_EUR", 20.0)
  val monthKey = java.time.YearMonth.now().toString
  for
    apiKey <- sys.env.get("DEEPSEEK_API_KEY").filter(_.nonEmpty)
    raw <- commandOutput(
      Seq("curl", "-s", "https://api.deepseek.com/user/balance", "-H", s"Authorization: Bearer $apiKey")
    )
    json <- Try(ujson.read(raw)).toOption
    infos <- field(json, "balance_infos").flatMap(_.arrOpt)
    eur = infos.find(info => field(info, "currency").flatMap(_.strOpt).contains("EUR")).orElse(infos.headOption)
    eurValue <- eur
    currentBalance <- field(eurValue, "total_balance").flatMap(_.strOpt).flatMap(s => Try(s.toDouble).toOption)
  yield
    val (monthStartBalance, isNewMonth) = previous
      .flatMap { prev =>
        val storedMonthKey = field(prev, "monthKey").flatMap(_.strOpt)
        val storedBalance = field(prev, "monthStartBalanceEur").flatMap(_.numOpt)
        (storedMonthKey, storedBalance) match
          case (Some(mk), Some(bal)) if mk == monthKey => Some((bal, false))
          case _                                       => None
      }
      .getOrElse((currentBalance, true))
    val spent = math.max(0.0, monthStartBalance - currentBalance)
    val usedFraction =
      if currentBalance <= 0.0 then 1.0
      else math.min(1.0, spent / capEur)
    VendorBudget(
      vendor = "deepseek",
      usedFraction = usedFraction,
      estimate = false,
      scope = s"monthly-self-cap ($monthKey, cap=$capEur EUR)",
      source = "deepseek /user/balance + self-imposed monthly cap",
      extra = Map(
        "monthKey" -> ujson.Str(monthKey),
        "monthStartBalanceEur" -> ujson.Num(monthStartBalance),
        "currentBalanceEur" -> ujson.Num(currentBalance),
        "resetForNewMonth" -> ujson.Bool(isNewMonth)
      )
    )

def toJson(budget: VendorBudget): ujson.Obj =
  ujson.Obj(
    "vendor" -> budget.vendor,
    "usedFraction" -> budget.usedFraction,
    "estimate" -> budget.estimate,
    "scope" -> budget.scope,
    "source" -> budget.source,
    "resetAtEpochSeconds" -> budget.resetAtEpochSeconds.fold[ujson.Value](ujson.Null)(ujson.Num(_)),
    "extra" -> ujson.Obj.from(budget.extra)
  )

// Relative to a repo root, same convention as AgentInventory's config paths.
val VendorBudgetsRelativePath: os.RelPath = os.rel / ".gh-tasks-llm-executor" / "vendor-budgets.json"

// How long ago the on-disk snapshot was generated, if it exists and parses.
def vendorBudgetsAgeMillis(root: os.Path): Option[Long] =
  Try {
    val json = ujson.read(os.read(root / VendorBudgetsRelativePath))
    val generatedAt = field(json, "generatedAtEpochMillis")
      .flatMap(value =>
        value.strOpt
          .flatMap(text => Try(text.toLong).toOption)
          .orElse(value.numOpt.map(_.toLong))
      )
      .getOrElse(0L)
    System.currentTimeMillis() - generatedAt
  }.toOption

// Never throws - a probe failure degrades to "no budgets collected" rather
// than blocking whatever called it. (Note: this file is under `scripts/`,
// which `project.scala` excludes from the main build, so this logic is
// duplicated - not shared - in the root-level VendorBudgets.scala that
// Implementations.scala's resolveContext actually calls. Keep both in sync
// by hand if the probe logic changes.)
def collectAndWriteVendorBudgets(root: os.Path): List[VendorBudget] =
  val outputPath = root / VendorBudgetsRelativePath

  val previous: Option[ujson.Value] = Try(ujson.read(os.read(outputPath))).toOption
  val previousDeepseekExtra: Option[ujson.Value] =
    previous
      .flatMap(v => field(v, "budgets"))
      .flatMap(_.arrOpt)
      .flatMap(_.find(b => field(b, "vendor").flatMap(_.strOpt).contains("deepseek")))
      .flatMap(b => field(b, "extra"))

  val budgets = Try(
    List(
      readCodexBudget(),
      readClaudeBudget(),
      readGeminiBudget(),
      readDeepseekBudget(previousDeepseekExtra)
    ).flatten
  ).getOrElse(Nil)

  val json = ujson.Obj(
    "schemaVersion" -> 1,
    "generatedBy" -> "scripts/discover-vendor-budgets.scala",
    "generatedAtEpochMillis" -> System.currentTimeMillis(),
    "budgets" -> budgets.map(toJson)
  )

  Try {
    os.makeDir.all(root / ".gh-tasks-llm-executor")
    os.write.over(outputPath, ujson.write(json, indent = 2) + "\n")
  }
  budgets

@main def discoverVendorBudgets(): Unit =
  val budgets = collectAndWriteVendorBudgets(os.pwd)
  val outputPath = os.pwd / VendorBudgetsRelativePath
  println(s"Wrote $outputPath (${budgets.size} vendor(s): ${budgets.map(_.vendor).mkString(", ")})")
