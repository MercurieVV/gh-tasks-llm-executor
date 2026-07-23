import cats.effect.kernel.Sync
import cats.syntax.all.*

import java.io.BufferedReader
import java.io.InputStream
import java.io.InputStreamReader
import java.time.Instant
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicLong
import scala.collection.mutable.StringBuilder
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

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
      jsonSchema: Option[String] = None
  ): F[Output] =
    runAttempt(runner, prompt, cwd, allowedTools, jsonSchema, attempt = 1)

  private def runAttempt(
      runner: TaskRunner,
      prompt: AgentPrompt,
      cwd: os.Path,
      allowedTools: Seq[String],
      jsonSchema: Option[String],
      attempt: Int
  ): F[Output] =
    for
      _ <- TaskLogger.llm(
        s"Starting agent execution with ${runner.display} in $cwd"
      )
      result <- F.blocking(
        runMonitored(runner, prompt, cwd, allowedTools, jsonSchema)
      )
      output = result.output
      _ <-
        if output.value.nonEmpty then TaskLogger.llm(output.value.trim)
        else TaskLogger.llm("Agent produced no output.")
      _ <- TaskLogger.llm(
        s"Agent execution finished with exit code ${result.exitCode}."
      )
      value <-
        if result.exitCode === 0 then output.pure[F]
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
              attempt + 1
            )
        else
          val reason = terminationReason(output.value)
          reason.traverse_(r => TaskLogger.llm(s"!!! Termination reason: $r")) *>
            RuntimeException(
              reason.fold(
                s"${runner.agent} exited with ${result.exitCode}"
              )(r => s"${runner.agent} exited with ${result.exitCode}: $r")
            ).raiseError[F, Output]
    yield value

  private val TerminationReasonPatterns: List[String] = List(
    "session limit",
    "usage limit",
    "quota exceeded",
    "rate limit",
    "please run /login",
    "invalid api key",
    "authentication_error",
    "permission denied",
    "out of memory",
    "context length exceeded",
    "context_length_exceeded"
  )

  private def terminationReason(output: String): Option[String] =
    val lower = output.toLowerCase
    TerminationReasonPatterns
      .find(lower.contains)
      .flatMap(pattern => output.linesIterator.find(_.toLowerCase.contains(pattern)))
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
      jsonSchema: Option[String]
  ): AgentResult =
    val started = System.currentTimeMillis()
    val lastActivity = AtomicLong(started)
    val output = StringBuilder()
    val command = runner.command(prompt, allowedTools, jsonSchema)
    TaskLogger.unsafeTrace(
      s"agent command cwd=$cwd args=${commandForLog(command, prompt)} promptChars=${prompt.value.length}"
    )
    val process = ProcessBuilder(command*)
      .directory(cwd.toIO)
      .start()
    process.getOutputStream.close()
    val runLogDir =
      os.RelPath(s"agent-${process.pid()}-${fileSafe(runner.agent)}")
    TaskLogger.unsafeWriteArtifact(
      runLogDir / "prompt.txt",
      prompt.value + System.lineSeparator()
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

    timedOut match
      case Some(reason) => throw RuntimeException(reason)
      case None         => AgentResult(process.exitValue(), AgentOutput(output.toString))

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
  def apply[F[_]: Sync]: AgentExecutor[F] = new AgentExecutor[F]
