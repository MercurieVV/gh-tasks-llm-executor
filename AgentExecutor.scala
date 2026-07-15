import cats.effect.kernel.Sync
import cats.syntax.all.*

import java.io.BufferedReader
import java.io.InputStream
import java.io.InputStreamReader
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicLong
import scala.collection.mutable.StringBuilder
import scala.concurrent.duration.*

final class AgentExecutor[F[_]](using F: Sync[F]):

  private val InactivityTimeoutMillis = 5.minutes.toMillis
  private val TotalTimeoutMillis = 45.minutes.toMillis
  private val PollMillis = 5.seconds.toMillis

  def run(runner: TaskRunner, prompt: String, cwd: os.Path): F[String] =
    for
      _ <- TaskLogger.llm(
        s"Starting agent execution with ${runner.display} in $cwd"
      )
      result <- F.blocking(runMonitored(runner, prompt, cwd))
      output = result.output
      _ <-
        if output.nonEmpty then TaskLogger.llm(output.trim)
        else TaskLogger.llm("Agent produced no output.")
      _ <- TaskLogger.llm(
        s"Agent execution finished with exit code ${result.exitCode}."
      )
      value <- Either
        .cond(
          result.exitCode === 0,
          output,
          new RuntimeException(
            s"${runner.agent} exited with ${result.exitCode}"
          )
        )
        .liftTo[F]
    yield value

  private final case class AgentResult(exitCode: Int, output: String)

  private def runMonitored(
      runner: TaskRunner,
      prompt: String,
      cwd: os.Path
  ): AgentResult =
    val started = System.currentTimeMillis()
    val lastActivity = AtomicLong(started)
    val output = StringBuilder()
    val process = ProcessBuilder(runner.command(prompt)*)
      .directory(cwd.toIO)
      .start()
    val stdout =
      streamReader("stdout", process.getInputStream, output, lastActivity)
    val stderr =
      streamReader("stderr", process.getErrorStream, output, lastActivity)
    stdout.start()
    stderr.start()

    var lastStatus = worktreeStatus(cwd)
    var finished = false
    var timedOut = Option.empty[String]

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
      TaskLogger.unsafeTrace(
        s"agent monitor cwd=$cwd running=${!finished} idleMs=$idleFor totalMs=$totalFor"
      )
      if idleFor >= InactivityTimeoutMillis then
        timedOut = Some(
          s"Agent had no observable output or worktree changes for ${InactivityTimeoutMillis / 1000}s."
        )
      else if totalFor >= TotalTimeoutMillis then
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
      case None         => AgentResult(process.exitValue(), output.toString)

  private def streamReader(
      name: String,
      stream: InputStream,
      output: StringBuilder,
      lastActivity: AtomicLong
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
          TaskLogger.unsafeLlm(s"agent $name: $line")
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

  private def stopProcessTree(process: Process): Unit =
    val handle = process.toHandle
    handle.descendants().forEach(_.destroy())
    process.destroy()
    if !process.waitFor(5, TimeUnit.SECONDS) then
      handle.descendants().forEach(_.destroyForcibly())
      process.destroyForcibly()

object AgentExecutor:
  def apply[F[_]: Sync]: AgentExecutor[F] = new AgentExecutor[F]
