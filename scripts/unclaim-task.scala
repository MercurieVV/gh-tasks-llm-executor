#!/usr/bin/env scala-cli

//> using scala 3.3.4
//> using dep com.lihaoyi::os-lib:0.11.8

// Force-releases the cross-process claim on a task (see IssueClaim.scala's
// refs/gh-tasks-llm-executor/claims/task-<N> ref), without touching any
// branch, worktree, or issue content. Use when a claim is stuck (e.g. its
// owning process died) and you don't want to wait out the 4h staleness
// auto-release.
//
//   scala-cli run scripts/unclaim-task.scala -- 74

object UnclaimTask:
  def main(args: Array[String]): Unit =
    if args.length != 1 || args(0).trim.isEmpty then
      println("Usage: scala-cli run scripts/unclaim-task.scala -- <task-number>")
      sys.exit(1)

    val taskNumber = args(0).trim
    val ref = s"refs/gh-tasks-llm-executor/claims/task-$taskNumber"

    val repoRoot =
      try
        os.Path(
          os.proc("git", "rev-parse", "--show-toplevel").call().out.text().trim
        )
      catch
        case _: Exception =>
          println("Error: not inside a git repository.")
          sys.exit(1)

    val exists = os
      .proc("git", "ls-remote", "--exit-code", "origin", ref)
      .call(cwd = repoRoot, check = false, stdout = os.Pipe, stderr = os.Pipe)
      .exitCode == 0

    if !exists then println(s"Task #$taskNumber has no active claim.")
    else
      val result = os
        .proc("git", "push", "origin", "--delete", ref)
        .call(cwd = repoRoot, check = false, stdout = os.Pipe, stderr = os.Pipe)
      if result.exitCode == 0 then println(s"Unclaimed task #$taskNumber.")
      else
        println(s"Failed to unclaim task #$taskNumber: ${result.err.text().trim}")
        sys.exit(1)