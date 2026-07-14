#!/usr/bin/env scala-cli

//> using scala 3.3.4
//> using dep com.lihaoyi::os-lib:0.11.8

import os._

object WorktreeStart:
  def main(args: Array[String]): Unit =
    if args.isEmpty then
      println("Error: task branch name required")
      sys.exit(1)

    val branch = args.head
    val repoRoot = os.Path(
      os.proc("git", "rev-parse", "--show-toplevel").call().out.text().trim
    )

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
          .call(cwd = repoRoot)
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
          .call(cwd = repoRoot, check = false)
          .exitCode == 0
      then base
      else if os
          .proc("git", "show-ref", "--verify", "--quiet", "refs/heads/master")
          .call(cwd = repoRoot, check = false)
          .exitCode == 0
      then "master"
      else "main"

    val wt = repoRoot / ".worktrees" / branch

    if os.exists(wt) then
      println(s"Error: Worktree directory already exists: $wt")
      sys.exit(1)

    println("Fetching latest changes from origin...")
    try {
      os.proc("git", "fetch", "origin", finalBase, "--quiet")
        .call(cwd = repoRoot)
    } catch {
      case _: Exception => // ignore fetch failures
    }

    println(s"Creating branch '$branch' off '$finalBase'...")
    val branchCreated = os
      .proc("git", "branch", branch, s"origin/$finalBase")
      .call(cwd = repoRoot, check = false)
      .exitCode == 0 ||
      os.proc("git", "branch", branch, finalBase)
        .call(cwd = repoRoot, check = false)
        .exitCode == 0

    println(s"Adding git worktree at '$wt'...")
    val wtAdded = os
      .proc("git", "worktree", "add", "-b", branch, wt.toString, finalBase)
      .call(cwd = repoRoot, check = false)
      .exitCode == 0 ||
      os.proc("git", "worktree", "add", wt.toString, branch)
        .call(cwd = repoRoot, check = false)
        .exitCode == 0

    if !wtAdded then
      println("Error: Failed to add git worktree")
      sys.exit(1)

    // Copy untracked config/rules files
    println("Syncing workspace rules and config files to worktree...")
    os.makeDir.all(wt / ".agents")
    val filesToCopy = Seq(
      Path(".agents/mcp_config.json", repoRoot),
      Path(".agents/AGENTS.md", repoRoot),
      Path(".cursorrules", repoRoot),
      Path("scala-rules.md", repoRoot)
    )

    for f <- filesToCopy if os.exists(f) do
      val relative = f.relativeTo(repoRoot)
      val dest = wt / relative
      os.makeDir.all(dest / os.up)
      os.copy.over(f, dest)

    println(
      "========================================================================"
    )
    println("Worktree created successfully!")
    println(s"  Path: $wt")
    println(s"  Branch: $branch")
    println("")
    println("To switch to your new worktree, run:")
    println(s"  cd $wt")
    println(
      "========================================================================"
    )
