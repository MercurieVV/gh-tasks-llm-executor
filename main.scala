import arrowstep.core.*
import arrowstep.runtime.ProtocolJson
import cats.Id

import java.io.{BufferedReader, InputStreamReader}

case class Issue(
    number: Int,
    title: String,
    body: String,
    state: String
)

@main
def main(args: String*): Unit = {
  val executor =
    optionValue(args, "--executor", "--llm").getOrElse(resolveExecutor(args))

  try {
    println("Fetching open issues from GitHub...")
    val issuesJson = os
      .proc(
        "gh",
        "issue",
        "list",
        "--state",
        "open",
        "--limit",
        "1000",
        "--json",
        "number,title,body,state"
      )
      .call()
      .out
      .text()

    val json = ujson.read(issuesJson)
    val issues = json.arr.map { item =>
      val number = item("number").num.toInt
      val title = item("title").str
      val body =
        if (item.obj.contains("body") && !item("body").isNull) item("body").str
        else ""
      val state =
        if (item.obj.contains("state") && !item("state").isNull)
          item("state").str
        else ""
      Issue(number, title, body, state)
    }.toList

    val quotedExecutor = java.util.regex.Pattern.quote(executor)
    val ExecutorRegex =
      s"""(?i)(?:preferr?ed\\s+executor|executor)\\s*(?:is\\s+)?(?:is:?\\s+)?:?\\s*$quotedExecutor""".r
    def hasTargetExecutor(issue: Issue): Boolean = {
      ExecutorRegex.findFirstIn(issue.body).isDefined
    }

    val depLineKeywords =
      List("depends on", "depend on", "dependency", "dependencies", "parent")
    val IssueNumRegex = """#(\d+)""".r

    def getDependencies(body: String): List[Int] = {
      body.linesIterator
        .flatMap { line =>
          val lower = line.toLowerCase
          if (depLineKeywords.exists(k => lower.contains(k))) {
            IssueNumRegex.findAllMatchIn(line).map(_.group(1).toInt)
          } else {
            Nil
          }
        }
        .toList
        .distinct
    }

    val openIssueNumbers = issues.map(_.number).toSet

    def hasUnresolvedDependencies(issue: Issue): Boolean = {
      val deps = getDependencies(issue.body)
      deps.exists(dep => openIssueNumbers.contains(dep))
    }

    val candidateTasks = issues.filter(hasTargetExecutor)
    println(
      s"Found ${candidateTasks.size} open tasks with '$executor' preferred executor."
    )

    val nextTaskOpt =
      candidateTasks.find(task => !hasUnresolvedDependencies(task))
    nextTaskOpt match {
      case None =>
        println(
          s"No tasks found without unresolved dependencies and with '$executor' executor."
        )
      case Some(nextTask) =>
        val taskId = nextTask.number
        println(s"Selected next task: #$taskId - ${nextTask.title}")
        setIssueStatus(taskId, "in progress")

        // Read parent task conclusion comment if exists
        val ParentRegex = """(?i)parent:?\s*#(\d+)""".r
        val parentIdOpt =
          ParentRegex.findFirstMatchIn(nextTask.body).map(_.group(1).toInt)
        val parentConclusionOpt = parentIdOpt.flatMap { parentId =>
          println(
            s"Found parent task #$parentId. Fetching conclusion comment..."
          )
          try {
            val res = os
              .proc(
                "gh",
                "issue",
                "view",
                parentId.toString,
                "--json",
                "comments"
              )
              .call()
            val commentsJson = ujson.read(res.out.text())
            val comments = commentsJson("comments").arr
            val conclusionComment = comments
              .find(c => c("body").str.toLowerCase.contains("conclusion"))
              .orElse(comments.lastOption)
              .map(c => c("body").str)
            conclusionComment
          } catch {
            case e: Exception =>
              println(
                s"Failed to read comments for parent task #$parentId: ${e.getMessage}"
              )
              None
          }
        }

        // Start new worktree
        val worktreePath = os.pwd / os.up / s"task-$taskId"
        val branchName = s"task-$taskId"

        println(s"Starting new worktree at $worktreePath")
        if (os.exists(worktreePath)) {
          println(
            s"Worktree directory $worktreePath already exists. Cleaning up..."
          )
          try {
            os.proc(
              "git",
              "worktree",
              "remove",
              "--force",
              worktreePath.toString
            ).call(cwd = os.pwd)
          } catch {
            case _: Exception => // ignore
          }
          if (os.exists(worktreePath)) {
            os.remove.all(worktreePath)
          }
        }

        val branchExists =
          try {
            os.proc("git", "show-ref", "--verify", s"refs/heads/$branchName")
              .call(cwd = os.pwd)
            true
          } catch {
            case _: Exception => false
          }

        if (branchExists) {
          println(s"Branch $branchName already exists, adding worktree for it")
          os.proc("git", "worktree", "add", worktreePath.toString, branchName)
            .call(cwd = os.pwd)
        } else {
          println(s"Creating branch $branchName and adding worktree")
          os.proc(
            "git",
            "worktree",
            "add",
            "-b",
            branchName,
            worktreePath.toString
          ).call(cwd = os.pwd)
        }

        // Run task there
        val parentConclusionStr = parentConclusionOpt
          .map(c => s"\nParent Task Conclusion Comment:\n$c\n")
          .getOrElse("")
        val prompt = s"""Task ID: #$taskId
Title: ${nextTask.title}

Task Description:
${nextTask.body}
$parentConclusionStr
Please implement this task in the current repository. Make any necessary file changes.
"""
        println(s"Running task #$taskId with $executor...")
        val output =
          runCommandAndCapture(Seq(executor, "-p", prompt), worktreePath)

        // Put output in task comment
        println(s"Commenting run output on task #$taskId...")
        val commentBody = s"""Task run output:
```
$output
```"""
        val tempFile = os.temp(commentBody)
        os.proc(
          "gh",
          "issue",
          "comment",
          taskId.toString,
          "--body-file",
          tempFile.toString
        ).call(cwd = os.pwd)

        // Check if there was files changed
        val gitStatus = os
          .proc("git", "status", "--porcelain")
          .call(cwd = worktreePath)
          .out
          .text()
          .trim
        val filesChanged = gitStatus.nonEmpty

        if (filesChanged) {
          println("Files changed. Committing and merging changes...")
          os.proc("git", "add", "-A").call(cwd = worktreePath)
          os.proc(
            "git",
            "commit",
            "-m",
            s"Implement task #$taskId: ${nextTask.title}"
          ).call(cwd = worktreePath)

          val hasRemote =
            os.proc("git", "remote").call(cwd = os.pwd).out.text().trim.nonEmpty
          if (hasRemote) {
            println("Pushing branch to origin...")
            os.proc("git", "push", "-u", "origin", branchName)
              .call(cwd = worktreePath)
            println("Creating Pull Request...")
            os.proc(
              "gh",
              "pr",
              "create",
              "--title",
              s"Task #$taskId: ${nextTask.title}",
              "--body",
              s"Closes #$taskId",
              "--head",
              branchName
            ).call(cwd = worktreePath)
            println("Merging Pull Request...")
            os.proc("gh", "pr", "merge", "--merge", "--delete-branch")
              .call(cwd = worktreePath)
          } else {
            println("No remote detected. Merging branch locally...")
            val mainBranch = os
              .proc("git", "branch", "--show-current")
              .call(cwd = os.pwd)
              .out
              .text()
              .trim

            // Remove worktree first so we can merge/delete the branch
            println(s"Removing worktree at $worktreePath")
            os.proc(
              "git",
              "worktree",
              "remove",
              "--force",
              worktreePath.toString
            ).call(cwd = os.pwd)

            println(s"Merging branch $branchName into $mainBranch...")
            os.proc("git", "merge", branchName).call(cwd = os.pwd)

            println(s"Deleting local branch $branchName...")
            os.proc("git", "branch", "-d", branchName).call(cwd = os.pwd)
          }
        } else {
          println("No files changed.")
        }

        // Clean up worktree if it still exists
        if (os.exists(worktreePath)) {
          println(s"Removing worktree at $worktreePath")
          os.proc("git", "worktree", "remove", "--force", worktreePath.toString)
            .call(cwd = os.pwd)
          try {
            os.proc("git", "branch", "-D", branchName).call(cwd = os.pwd)
          } catch {
            case _: Exception => // ignore
          }
        }

        // Close task with comment
        println(s"Closing task #$taskId with comment...")
        setIssueStatus(taskId, "completed")
        val closeComment =
          s"Task #$taskId completed successfully. Worktree closed."
        os.proc(
          "gh",
          "issue",
          "close",
          taskId.toString,
          "--comment",
          closeComment
        ).call(cwd = os.pwd)
        println("Task execution finished successfully.")
    }
  } catch {
    case e: Exception =>
      System.err.println(s"Fatal error during execution: ${e.getMessage}")
      e.printStackTrace()
      sys.exit(1)
  }
}

def optionValue(args: Seq[String], names: String*): Option[String] =
  names.iterator
    .flatMap { name =>
      args.indexOf(name) match {
        case idx if idx >= 0 && idx + 1 < args.length => Some(args(idx + 1))
        case _                                        => None
      }
    }
    .nextOption()

def resolveExecutor(args: Seq[String]): String = {
  val input = AskInput(
    questions = List(
      Question(
        id = "executor",
        text = "Which LLM executor command should run selected GitHub tasks?",
        kind = QuestionKind.FreeText,
        default = Some("codex"),
        current = None,
        context = Some(
          "Use the same command name you would pass to --executor, for example codex, claude, gemini, or agy."
        )
      )
    ),
    context = Some(
      "gh-tasks-llm-executor needs an executor command before it can start a task worktree."
    )
  )

  val answers = optionValue(args, "--answers")
    .flatMap(ProtocolJson.parseAnswers)
    .orElse(readAnswersFile())

  answers match {
    case None =>
      val message = ProgramSays.NeedInput(input.context, input.questions)
      println(ProtocolJson.render(message))
      sys.exit(message.exitCode)

    case Some(rawAnswers) =>
      Validator.basic[Id].validate(input.questions, rawAnswers) match {
        case Right(valid) =>
          valid.toMap("executor")
        case Left(problems) =>
          val message = ProgramSays.Rejected(problems, input.questions)
          println(ProtocolJson.render(message))
          sys.exit(message.exitCode)
      }
  }
}

def readAnswersFile(): Option[Answers] = {
  val path = os.pwd / ".agents" / "answers.json"
  Option.when(os.exists(path))(os.read(path)).flatMap(ProtocolJson.parseAnswers)
}

def runCommandAndCapture(cmd: Seq[String], cwd: os.Path): String = {
  val pb = new java.lang.ProcessBuilder(cmd*)
  pb.directory(cwd.toIO)
  pb.redirectErrorStream(true)
  val process = pb.start()

  val reader = new BufferedReader(new InputStreamReader(process.getInputStream))
  val sb = new java.lang.StringBuilder()
  var line = reader.readLine()
  while (line != null) {
    println(line)
    sb.append(line).append("\n")
    line = reader.readLine()
  }
  process.waitFor()
  sb.toString
}

def setIssueStatus(taskId: Int, status: String): Unit = {
  // 1. Try to edit labels using gh CLI
  val (toAdd, toRemove) = if (status == "in progress") {
    (
      List("status: in progress", "in progress"),
      List("status: completed", "completed")
    )
  } else {
    (
      List("status: completed", "completed"),
      List("status: in progress", "in progress")
    )
  }

  try {
    val addFlags = toAdd.flatMap(l => Seq("--add-label", l))
    val removeFlags = toRemove.flatMap(l => Seq("--remove-label", l))
    val fullCmd =
      Seq("gh", "issue", "edit", taskId.toString) ++ addFlags ++ removeFlags
    os.proc(fullCmd).call()
  } catch {
    case e: Exception =>
      println(
        s"Warning: Failed to update GitHub labels for task #$taskId: ${e.getMessage}"
      )
  }

  // 2. Try to run local ght command if it exists
  try {
    os.proc("ght", "status", taskId.toString, status).call()
  } catch {
    case _: Exception => // ignore if ght is not found or fails
  }
}
