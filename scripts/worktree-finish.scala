#!/usr/bin/env scala-cli

//> using scala 3.3.4
//> using dep com.lihaoyi::os-lib:0.11.8

import os._

object WorktreeFinish:
  def main(args: Array[String]): Unit =
    val currentDir = os.pwd
    val repoRoot =
      try {
        os.Path(
          os.proc("git", "rev-parse", "--show-toplevel").call().out.text().trim
        )
      } catch {
        case _: Exception =>
          println("Error: Not inside a git repository")
          sys.exit(1)
      }

    var branch = ""
    var wtDir: os.Path = os.Path("/")
    var mainRepo: os.Path = os.Path("/")
    val wtPattern = "\\\\.worktrees/([^/]+)".r

    val currentDirStr = currentDir.toString
    wtPattern.findFirstMatchIn(currentDirStr) match
      case Some(m) =>
        branch = m.group(1)
        wtDir = repoRoot
        val idx = currentDirStr.indexOf("/.worktrees/")
        mainRepo = os.Path(currentDirStr.substring(0, idx))
      case None =>
        if args.isEmpty then
          println(
            "Error: Not in a worktree. Please provide the branch name as an argument."
          )
          sys.exit(1)
        branch = args.head
        mainRepo = repoRoot
        wtDir = mainRepo / ".worktrees" / branch

    if !os.exists(wtDir) then
      println(s"Error: Worktree directory not found at $wtDir")
      sys.exit(1)

    val message =
      if args.length > 1 then args(1) else s"$branch: automated implementation"

    // Check for changes in worktree
    val isClean = os
      .proc("git", "status", "--porcelain")
      .call(cwd = wtDir)
      .out
      .text()
      .trim
      .isEmpty
    if !isClean then
      println("Committing changes in worktree...")
      os.proc("git", "add", ".").call(cwd = wtDir)
      os.proc("git", "commit", "-m", message).call(cwd = wtDir)

    // Detect default branch
    val base =
      try {
        val raw = os
          .proc(
            "git",
            "symbolic-ref",
            "--quiet",
            "--short",
            "refs/remotes/origin/HEAD"
          )
          .call(cwd = mainRepo)
          .out
          .text()
          .trim
        raw.stripPrefix("origin/")
      } catch {
        case _: Exception => "main"
      }

    val finalBase =
      if os
          .proc("git", "show-ref", "--verify", "--quiet", s"refs/heads/$base")
          .call(cwd = mainRepo, check = false)
          .exitCode == 0
      then base
      else if os
          .proc("git", "show-ref", "--verify", "--quiet", "refs/heads/master")
          .call(cwd = mainRepo, check = false)
          .exitCode == 0
      then "master"
      else "main"

    println(s"Merging branch '$branch' into '$finalBase'...")
    os.proc("git", "checkout", finalBase).call(cwd = mainRepo)
    os.proc(
      "git",
      "merge",
      branch,
      "--no-ff",
      "-m",
      s"Merge branch '$branch' into $finalBase"
    ).call(cwd = mainRepo)

    println(s"Removing worktree at '$wtDir'...")
    try {
      os.proc("git", "worktree", "remove", "--force", wtDir.toString)
        .call(cwd = mainRepo)
    } catch {
      case _: Exception => // ignore if already gone
    }

    println(s"Deleting branch '$branch'...")
    val branchDeleted = os
      .proc("git", "branch", "-d", branch)
      .call(cwd = mainRepo, check = false)
      .exitCode == 0 ||
      os.proc("git", "branch", "-D", branch)
        .call(cwd = mainRepo, check = false)
        .exitCode == 0

    println(
      "========================================================================"
    )
    println("Worktree cleaned up successfully!")
    println(s"  Merged branch: $branch into $finalBase")
    println(s"  Returned to: $mainRepo (branch: $finalBase)")
    println("")
    println("To return your shell to the main repository, run:")
    println(s"  cd $mainRepo")
    println(
      "========================================================================"
    )
