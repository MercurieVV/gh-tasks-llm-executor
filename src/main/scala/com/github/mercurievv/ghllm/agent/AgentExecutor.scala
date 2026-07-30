package com.github.mercurievv.ghllm.agent

import com.github.mercurievv.ghllm.*
import com.github.mercurievv.ghllm.agent.*
import com.github.mercurievv.ghllm.arrow.*
import com.github.mercurievv.ghllm.cli.*
import com.github.mercurievv.ghllm.git.*
import com.github.mercurievv.ghllm.metrics.*

import cats.effect.kernel.Sync
import cats.syntax.all.*

import java.io.BufferedReader
import java.io.InputStream
import java.io.InputStreamReader
import java.time.Instant
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicLong
import java.util.concurrent.atomic.AtomicReference
import scala.collection.mutable.StringBuilder
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import scala.util.Try

type Output = AgentOutput

/** Full prompt text passed to an external agent process. */
opaque type AgentPrompt = String
object AgentPrompt:
  def apply(value: String): AgentPrompt = value
  extension (self: AgentPrompt) def value: String = self

final class AgentExecutor[F[_]](using F: Sync[F]):

  private val TotalTimeoutMillis = 45.minutes.toMillis
  private val PollMillis = 5.seconds.toMillis
  private val MaxTransientAttempts = 3
  private val TransientRetryDelayMillis = 15.seconds.toMillis

  def run(
      runner: TaskRunner,
      prompt: AgentPrompt,
      cwd: os.Path,
      allowedTools: Seq[String] = Nil,
      jsonSchema: Option[String] = None,
      contextFiles: Seq[String] = Nil,
      taskNumber: Option[TaskNumber] = None,
      metricsRoot: Option[os.Path] = None,
      metricsScope: String = "agent",
      deferMetricsOutcome: Boolean = false
  ): F[Output] =
    runAttempt(
      runner,
      prompt,
      cwd,
      allowedTools,
      jsonSchema,
      contextFiles,
      taskNumber,
      metricsRoot,
      metricsScope,
      deferMetricsOutcome,
      attempt = 1
    )

  private def runAttempt(
      runner: TaskRunner,
      prompt: AgentPrompt,
      cwd: os.Path,
      allowedTools: Seq[String],
      jsonSchema: Option[String],
      contextFiles: Seq[String],
      taskNumber: Option[TaskNumber],
      metricsRoot: Option[os.Path],
      metricsScope: String,
      deferMetricsOutcome: Boolean,
      attempt: Int
  ): F[Output] =
    for
      _ <- TaskLogger.llm(
        s"Starting agent execution with ${runner.display} in $cwd"
      )
      result <- F.blocking(
        runMonitored(
          runner,
          prompt,
          cwd,
          allowedTools,
          jsonSchema,
          contextFiles,
          taskNumber,
          metricsRoot,
          metricsScope,
          deferMetricsOutcome
        )
      )
      output = result.output
      _ <-
        if output.value.nonEmpty then TaskLogger.llm(output.value.trim)
        else TaskLogger.llm("Agent produced no output.")
      _ <- TaskLogger.llm(
        s"Agent execution finished with exit code ${result.exitCode}."
      )
      reason = terminationReason(output.value)
      value <-
        if reason.nonEmpty then
          reason.traverse_(r => TaskLogger.llm(s"!!! Termination reason: $r")) *>
            RuntimeException(
              reason.fold(
                s"${runner.agent} stopped with a known fatal error"
              )(r => s"${runner.agent} stopped with a known fatal error: $r")
            ).raiseError[F, Output]
        else if result.exitCode === 0 then output.pure[F]
        else if attempt < MaxTransientAttempts && isTransientAgentFailure(
            output.value
          )
        then
          TaskLogger.llm(
            s"${runner.agent} failed with a transient service error; retrying attempt ${attempt + 1}/$MaxTransientAttempts in ${TransientRetryDelayMillis / 1000}s."
          ) *>
            F.blocking(Thread.sleep(TransientRetryDelayMillis)) *>
            runAttempt(
              runner,
              prompt,
              cwd,
              allowedTools,
              jsonSchema,
              contextFiles,
              taskNumber,
              metricsRoot,
              metricsScope,
              deferMetricsOutcome,
              attempt + 1
            )
        else RuntimeException(s"${runner.agent} exited with ${result.exitCode}").raiseError[F, Output]
    yield value

  private val TerminationReasonPatterns: List[String] = List(
    "session limit",
    "usage limit",
    "quota exceeded",
    "rate limit",
    "please run /login",
    "invalid api key",
    "authentication_error",
    "insufficient balance",
    "permission denied",
    "out of memory",
    "context length exceeded",
    "context_length_exceeded"
  )

  private[agent] def terminationReason(output: String): Option[String] =
    val lower = output.toLowerCase
    val normalizedLower = lower.split("\\s+").mkString(" ")
    TerminationReasonPatterns
      .find(pattern => lower.contains(pattern) || normalizedLower.contains(pattern))
      .flatMap(pattern => output.linesIterator.find(_.toLowerCase.contains(pattern)))
      .orElse(
        TerminationReasonPatterns
          .find(normalizedLower.contains)
          .map(pattern => s"matched fatal agent output pattern: $pattern")
      )
      .map(_.trim)

  private def isTransientAgentFailure(output: String): Boolean =
    val lower = output.toLowerCase
    List(
      "529 overloaded",
      "overloaded",
      "server-side issue",
      "try again in a moment",
      "rate limit",
      "temporarily unavailable",
      "service unavailable",
      "internal server error",
      "bad gateway",
      "gateway timeout",
      "connection closed mid-response",
      "response above may be incomplete"
    ).exists(lower.contains)

  private final case class AgentResult(exitCode: Int, output: Output)

  private def runMonitored(
      runner: TaskRunner,
      prompt: AgentPrompt,
      cwd: os.Path,
      allowedTools: Seq[String],
      jsonSchema: Option[String],
      contextFiles: Seq[String],
      taskNumber: Option[TaskNumber],
      metricsRoot: Option[os.Path],
      metricsScope: String,
      deferMetricsOutcome: Boolean
  ): AgentResult =
    val started = System.currentTimeMillis()
    val metricsRootResolved = metricsRoot.getOrElse(TokenMetrics.defaultRootForWorktree(cwd))
    val metricsBackend = TokenMetrics.defaultBackend(metricsRootResolved)
    val metricsVendor = TokenMetrics.parseVendor(runner.agent.value)
    val usageSource = tokenUsageSource(runner, cwd)
    val beforeUsage = usageSource.flatMap(_.current())
    if deferMetricsOutcome then
      for
        vendor <- metricsVendor
        number <- taskNumber
      do
        AgentExecutor.deferTokenMetrics(
          metricsRootResolved,
          number,
          runner,
          metricsScope,
          metricsBackend,
          TokenMetrics.TokenMetricsEvent(
            timestampMillis = started,
            vendor = vendor,
            usage = TokenUsage.TokenSnapshot.Zero,
            taskNumber = taskNumber,
            model = runner.model,
            scope = metricsScope,
            phase = Some(metricsScope),
            runner = Some(runner.display)
          )
        )
    val metricsSource =
      if usageSource.nonEmpty then "session"
      else
        metricsVendor match
          case Some(TokenUsage.Vendor.Aider) => "agent-output"
          case _                             => "unsupported"
    TaskLogger.unsafeTrace(
      s"token metrics init agent=${runner.agent.value} vendor=${metricsVendor.map(_.toString.toLowerCase).getOrElse("unknown")} model=${runner.model
          .getOrElse("-")} scope=$metricsScope task=${taskNumber.map(_.value.toString).getOrElse("-")} cwd=$cwd root=$metricsRootResolved destination=${metricsBackend.destination} source=$metricsSource"
    )
    val lastActivity = AtomicLong(started)
    val output = StringBuilder()
    val promptForRun = runner.effectivePrompt(prompt, allowedTools, cwd = Some(cwd))
    val command = commandWithReporting(
      runner,
      runner.command(promptForRun, allowedTools, jsonSchema, cwd = Some(cwd), contextFiles = contextFiles)
    )
    TaskLogger.unsafeTrace(
      s"agent command cwd=$cwd args=${commandForLog(command, promptForRun)} promptChars=${promptForRun.value.length}"
    )
    val processBuilder = ProcessBuilder(command*).directory(cwd.toIO)
    runner.invocationEnvironment.foreach { case (name, value) =>
      processBuilder.environment().put(name, value)
    }
    val process = processBuilder.start()
    process.getOutputStream.close()
    val runLogDir =
      os.RelPath(s"agent-${process.pid()}-${fileSafe(runner.agent)}")
    TaskLogger.unsafeWriteArtifact(
      runLogDir / "prompt.txt",
      promptForRun.value + System.lineSeparator()
    )
    TaskLogger.unsafeTrace(
      s"agent process started pid=${process.pid()} alive=${process.isAlive} logDir=$runLogDir"
    )
    val stdout =
      streamReader(
        AgentOutputStream("stdout"),
        process.getInputStream,
        output,
        lastActivity,
        runLogDir / "stdout.log"
      )
    val stderr =
      streamReader(
        AgentOutputStream("stderr"),
        process.getErrorStream,
        output,
        lastActivity,
        runLogDir / "stderr.log"
      )
    stdout.start()
    stderr.start()

    var lastStatus = worktreeStatus(cwd)
    var finished = false
    var timedOut = Option.empty[String]
    var lastDescendantLog = 0L

    while !finished && timedOut.isEmpty do
      finished = process.waitFor(PollMillis, TimeUnit.MILLISECONDS)
      val now = System.currentTimeMillis()
      val status = worktreeStatus(cwd)
      if status =!= lastStatus then
        lastStatus = status
        lastActivity.set(now)
        TaskLogger.unsafeTrace(
          s"agent activity detected from worktree status change in $cwd"
        )

      val idleFor = now - lastActivity.get()
      val totalFor = now - started
      TaskLogger.unsafeMonitor(
        s"agent monitor cwd=$cwd ${processState(process)} running=${!finished} idleMs=$idleFor totalMs=$totalFor"
      )
      if now - lastDescendantLog >= 30.seconds.toMillis then
        lastDescendantLog = now
        val descendants = processDescendants(process)
        TaskLogger.unsafeTrace(s"agent descendants:\n$descendants")
        TaskLogger.unsafeAppendArtifact(
          runLogDir / "descendants.log",
          s"${Instant.now()} idleMs=$idleFor totalMs=$totalFor\n$descendants\n\n"
        )
      if totalFor >= TotalTimeoutMillis then
        timedOut = Some(
          s"Agent exceeded total timeout of ${TotalTimeoutMillis / 1000}s."
        )

    timedOut.foreach { reason =>
      TaskLogger.unsafeLlm(s"$reason Stopping agent process.")
      stopProcessTree(process)
    }
    stdout.join(TimeUnit.SECONDS.toMillis(5))
    stderr.join(TimeUnit.SECONDS.toMillis(5))

    val reportedOutput = parseReportedOutput(runner, AgentOutput(output.toString))
    recordTokenMetrics(
      runner = runner,
      taskNumber = taskNumber,
      metricsRoot = metricsRootResolved,
      metricsScope = metricsScope,
      metricsBackend = metricsBackend,
      metricsVendor = metricsVendor,
      usageSource = usageSource,
      beforeUsage = beforeUsage,
      output = reportedOutput.output,
      turnCount = reportedOutput.turnCount,
      deferMetricsOutcome = deferMetricsOutcome
    )
    timedOut match
      case Some(reason) => throw RuntimeException(reason)
      case None         => AgentResult(process.exitValue(), reportedOutput.output)

  private def tokenUsageSource(
      runner: TaskRunner,
      cwd: os.Path
  ): Option[TokenUsage.TokenUsageSource] =
    TokenMetrics.parseVendor(runner.agent.value).collect {
      case TokenUsage.Vendor.Claude => TokenUsage.ClaudeTokenUsageSource(cwd)
      case TokenUsage.Vendor.Codex  => TokenUsage.CodexTokenUsageSource()
    }

  private def recordTokenMetrics(
      runner: TaskRunner,
      taskNumber: Option[TaskNumber],
      metricsRoot: os.Path,
      metricsScope: String,
      metricsBackend: TokenMetrics.TokenMetricsBackend,
      metricsVendor: Option[TokenUsage.Vendor],
      usageSource: Option[TokenUsage.TokenUsageSource],
      beforeUsage: Option[TokenUsage.TokenSnapshot],
      output: Output,
      turnCount: Option[Int],
      deferMetricsOutcome: Boolean
  ): Unit =
    val usage: Option[TokenUsage.TokenSnapshot] =
      usageSource
        .flatMap(_.current())
        .map(afterUsage => beforeUsage.fold(afterUsage)(before => afterUsage - before))
        .orElse(
          metricsVendor.collect { case TokenUsage.Vendor.Aider =>
            TokenUsage.AiderTokenUsage.parseOutput(output.value)
          }.flatten
        )

    usage match
      case Some(usage)
          if usage.total > 0 || usage.input > 0 || usage.output > 0 || usage.cacheRead > 0 || usage.cacheWrite > 0 =>
        metricsVendor.foreach { vendor =>
          val event = TokenMetrics.TokenMetricsEvent(
            timestampMillis = System.currentTimeMillis(),
            vendor = vendor,
            usage = usage,
            taskNumber = taskNumber,
            model = runner.model,
            scope = metricsScope,
            phase = Some(metricsScope),
            runner = Some(runner.display),
            turnCount = turnCount
          )
          if deferMetricsOutcome then
            taskNumber.foreach(number =>
              AgentExecutor.deferTokenMetrics(
                metricsRoot,
                number,
                runner,
                metricsScope,
                metricsBackend,
                event
              )
            )
          else metricsBackend.record(event)
          TaskLogger.unsafeTrace(
            s"token metrics ${if deferMetricsOutcome then "staged" else "recorded"} agent=${runner.agent.value} vendor=${vendor.toString.toLowerCase} model=${runner.model
                .getOrElse("-")} scope=$metricsScope task=${taskNumber.map(_.value.toString).getOrElse("-")} destination=${metricsBackend.destination} usage=${TokenMetrics
                .renderSummary(usage)}"
          )
        }
      case Some(usage) =>
        TaskLogger.unsafeTrace(
          s"token metrics skipped reason=zero-usage agent=${runner.agent.value} vendor=${metricsVendor
              .map(_.toString.toLowerCase)
              .getOrElse("unknown")} scope=$metricsScope task=${taskNumber.map(_.value.toString).getOrElse("-")} destination=${metricsBackend.destination} usage=${TokenMetrics
              .renderSummary(usage)}"
        )
      case None =>
        TaskLogger.unsafeTrace(
          s"token metrics skipped reason=no-usage agent=${runner.agent.value} vendor=${metricsVendor
              .map(_.toString.toLowerCase)
              .getOrElse("unknown")} scope=$metricsScope task=${taskNumber.map(_.value.toString).getOrElse("-")} destination=${metricsBackend.destination}"
        )

  private def streamReader(
      name: AgentOutputStream,
      stream: InputStream,
      output: StringBuilder,
      lastActivity: AtomicLong,
      artifactPath: os.RelPath
  ): Thread =
    Thread.ofPlatform().name(s"agent-$name-reader").unstarted { () =>
      val reader = BufferedReader(InputStreamReader(stream))
      try
        var line = reader.readLine()
        while line != null do
          output.synchronized {
            output.append(line).append(System.lineSeparator())
          }
          lastActivity.set(System.currentTimeMillis())
          TaskLogger.unsafeAgentOutput(name, AgentOutputLine(line))
          TaskLogger.unsafeAppendArtifact(
            artifactPath,
            s"${Instant.now()} $line${System.lineSeparator()}"
          )
          line = reader.readLine()
      finally reader.close()
    }

  private final case class ReportedOutput(output: Output, turnCount: Option[Int])

  private def commandWithReporting(runner: TaskRunner, command: Seq[String]): Seq[String] =
    if runner.agent.value === "claude" && !command.exists(_.startsWith("--output-format")) then
      val promptIndex = command.indexOf("-p")
      command.patch(if promptIndex >= 0 then promptIndex else command.size, Seq("--output-format", "json"), 0)
    else command

  private def parseReportedOutput(runner: TaskRunner, output: Output): ReportedOutput =
    if runner.agent.value =!= "claude" then ReportedOutput(output, None)
    else
      val json =
        Try(ujson.read(output.value.trim)).toOption.orElse(
          output.value.linesIterator.toList.reverseIterator
            .flatMap(line => Try(ujson.read(line)).toOption)
            .nextOption()
        )
      val obj = json.flatMap(_.objOpt)
      val result = obj.flatMap(_.get("result")).flatMap(_.strOpt).map(AgentOutput.apply).getOrElse(output)
      val turnCount =
        obj
          .flatMap(_.get("num_turns"))
          .flatMap(_.numOpt)
          .map(_.toInt)
          .filter(_ >= 0)
      ReportedOutput(result, turnCount)

  private def worktreeStatus(cwd: os.Path): String =
    scala.util
      .Try {
        os.proc("git", "status", "--porcelain")
          .call(cwd = cwd, stdout = os.Pipe, stderr = os.Pipe, check = false)
          .out
          .text()
      }
      .getOrElse("")

  private def commandForLog(command: Seq[String], prompt: AgentPrompt): String =
    command.zipWithIndex
      .map { case (part, index) =>
        val value =
          if part === prompt.value || index > 0 && command(index - 1) === "-p"
          then s"<prompt:${part.length} chars>"
          else quote(part)
        value
      }
      .mkString(" ")

  private def quote(value: String): String =
    "\"" + value.replace("\\", "\\\\").replace("\"", "\\\"") + "\""

  private def processState(process: Process): String =
    val handle = process.toHandle
    val info = handle.info()
    val cpuMs = info
      .totalCpuDuration()
      .map(_.toMillis.toString)
      .orElse("unknown")
    val command = info.command().orElse("unknown")
    val descendants = handle.descendants().count()
    s"pid=${handle.pid()} alive=${handle.isAlive} cpuMs=$cpuMs descendants=$descendants command=${quote(command)}"

  private def processDescendants(process: Process): String =
    val lines = process.toHandle
      .descendants()
      .iterator()
      .asScala
      .map { handle =>
        val info = handle.info()
        val command = info.command().orElse("unknown")
        val args = info.arguments().map(_.toList).orElse(Nil).mkString(" ")
        val cpuMs =
          info.totalCpuDuration().map(_.toMillis.toString).orElse("unknown")
        s"pid=${handle.pid()} alive=${handle.isAlive} cpuMs=$cpuMs command=${quote(command)} args=${quote(args)}"
      }
      .toList
    if lines.isEmpty then "(none)" else lines.mkString(System.lineSeparator())

  private def stopProcessTree(process: Process): Unit =
    val handle = process.toHandle
    handle.descendants().forEach(_.destroy())
    process.destroy()
    if !process.waitFor(5, TimeUnit.SECONDS) then
      handle.descendants().forEach(_.destroyForcibly())
      process.destroyForcibly()

  private def fileSafe(value: AgentBinary): String =
    value.value.map {
      case char if char.isLetterOrDigit => char
      case '-'                          => '-'
      case '_'                          => '_'
      case _                            => '-'
    }

object AgentExecutor:
  private final case class MetricsKey(
      root: os.Path,
      taskNumber: TaskNumber,
      runner: String,
      scope: String
  )

  private final case class PendingTokenMetrics(
      backend: TokenMetrics.TokenMetricsBackend,
      event: TokenMetrics.TokenMetricsEvent
  )

  private val pendingTokenMetrics =
    AtomicReference(Map.empty[MetricsKey, PendingTokenMetrics])

  private def deferTokenMetrics(
      root: os.Path,
      taskNumber: TaskNumber,
      runner: TaskRunner,
      scope: String,
      backend: TokenMetrics.TokenMetricsBackend,
      event: TokenMetrics.TokenMetricsEvent
  ): Unit =
    val key = MetricsKey(root, taskNumber, runner.display, scope)
    pendingTokenMetrics.updateAndGet { current =>
      current.updatedWith(key) {
        case Some(existing) =>
          val turns =
            (existing.event.turnCount, event.turnCount) match
              case (Some(left), Some(right)) => Some(left + right)
              case (left @ Some(_), None)    => left
              case (None, right)             => right
          Some(
            existing.copy(
              event = existing.event.copy(
                usage = existing.event.usage + event.usage,
                turnCount = turns
              )
            )
          )
        case None => Some(PendingTokenMetrics(backend, event))
      }
    }
    ()

  def completeTokenMetrics[F[_]: Sync](
      root: os.Path,
      taskNumber: TaskNumber,
      runner: TaskRunner,
      scope: String,
      outcome: String
  ): F[Unit] =
    Sync[F].blocking {
      val key = MetricsKey(root, taskNumber, runner.display, scope)
      val removed = pendingTokenMetrics.getAndUpdate(_ - key).get(key)
      removed.foreach(pending =>
        pending.backend.record(
          pending.event.copy(outcome = Some(outcome))
        )
      )
    }

  def apply[F[_]: Sync]: AgentExecutor[F] = new AgentExecutor[F]
