import cats.effect.kernel.Sync
import cats.syntax.all
import cats.syntax.all.*

import scala.concurrent.duration.*
import scala.util.Try
import cats.data.Kleisli

opaque type Author = String
object Author:
  def apply(value: String): Author = value
  extension (self: Author) def value: String = self

opaque type CreatedAt = String
object CreatedAt:
  def apply(value: String): CreatedAt = value
  extension (self: CreatedAt) def value: String = self

opaque type Name = String
object Name:
  def apply(value: String): Name = value
  extension (self: Name) def value: String = self

opaque type State2 = String
object State2:
  def apply(value: String): State2 = value
  extension (self: State2) def value: String = self

opaque type Url = String
object Url:
  def apply(value: String): Url = value
  extension (self: Url) def value: String = self

opaque type Message = String
object Message:
  def apply(value: String): Message = value
  extension (self: Message) def value: String = self

opaque type Message2 = String
object Message2:
  def apply(value: String): Message2 = value
  extension (self: Message2) def value: String = self

opaque type Message3 = String
object Message3:
  def apply(value: String): Message3 = value
  extension (self: Message3) def value: String = self

opaque type Message4 = String
object Message4:
  def apply(value: String): Message4 = value
  extension (self: Message4) def value: String = self

opaque type State = String
object State:
  def apply(value: String): State = value
  extension (self: State) def value: String = self

/** Status field returned by `gh run list`, normalized to uppercase. */
opaque type WorkflowRunStatus = String
object WorkflowRunStatus:
  def apply(value: String): WorkflowRunStatus = value
  extension (self: WorkflowRunStatus) def value: String = self

/** Error message used when branch CI has failed. */
opaque type BranchChecksFailedMessage = String
object BranchChecksFailedMessage:
  def apply(value: String): BranchChecksFailedMessage = value
  extension (self: BranchChecksFailedMessage) def value: String = self

/** Success message used when branch CI has passed. */
opaque type BranchChecksPassedMessage = String
object BranchChecksPassedMessage:
  def apply(value: String): BranchChecksPassedMessage = value
  extension (self: BranchChecksPassedMessage) def value: String = self

/** Progress message used while branch CI is still pending. */
opaque type BranchChecksPendingMessage = String
object BranchChecksPendingMessage:
  def apply(value: String): BranchChecksPendingMessage = value
  extension (self: BranchChecksPendingMessage) def value: String = self

/** Body text of a GitHub issue comment, distinct from the issue's own body. */
opaque type IssueCommentBody = String
object IssueCommentBody:
  def apply(value: String): IssueCommentBody = value
  extension (self: IssueCommentBody) def value: String = self

/** GitHub issue title. */
opaque type IssueTitle = String
object IssueTitle:
  def apply(value: String): IssueTitle = value
  extension (self: IssueTitle) def value: String = self

/** Git commit SHA returned by GitHub for CI and merge checks. */
opaque type CommitSha = String
object CommitSha:
  def apply(value: String): CommitSha = value.asInstanceOf[CommitSha]
  extension (opaqueValue: CommitSha) def value: String = opaqueValue.asInstanceOf[String]
  given cats.Eq[CommitSha] = cats.Eq.by(_.value)

/** Absolute timestamp or duration expressed in epoch milliseconds. */
opaque type DeadlineMillis = Long
object DeadlineMillis:
  def apply(value: Long): DeadlineMillis = value.asInstanceOf[DeadlineMillis]
  extension (opaqueValue: DeadlineMillis)
    def value: Long = opaqueValue.asInstanceOf[Long]
    def +(another: DeadlineMillis): DeadlineMillis =
      DeadlineMillis(opaqueValue.value + another.value)
  given cats.Eq[DeadlineMillis] = cats.Eq.by(_.value)

final case class Issue(
    number: TaskNumber,
    title: IssueTitle,
    body: IssueBody,
    state: State,
    labels: List[String] = Nil
)

object GitHub:

  final case class NoOpenPullRequestToResumeException(branchName: BranchName)
      extends RuntimeException(
        s"No open Pull Request found for $branchName to resume."
      )

  private val PullRequestCheckTimeoutMillis = DeadlineMillis(
    30.minutes.toMillis
  )
  private val PullRequestCheckPollMillis = DeadlineMillis(30.seconds.toMillis)
  private val PullRequestNoChecksGraceMillis = DeadlineMillis(
    3.minutes.toMillis
  )

  def fetchIssues[F[_]: Sync]: Kleisli[F, os.Path, List[Issue]] =
    Kleisli.apply { root =>
      Sync[F].blocking {
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
            "number,title,body,state,labels"
          )
          .call(cwd = root)
          .out
          .text()

        ujson.read(issuesJson).arr.toList.flatMap(parseIssue)
      }
    }

  private def parseIssue(value: ujson.Value): Option[Issue] =
    value match
      case ujson.Obj(fields) =>
        for
          number <- fields.get("number").collect { case ujson.Num(value) =>
            TaskNumber(value.toInt)
          }
          title <- fields.get("title").collect { case ujson.Str(value) =>
            value
          }
        yield Issue(
          number = number,
          title = IssueTitle(title),
          body = IssueBody(
            fields
              .get("body")
              .collect { case ujson.Str(value) => value }
              .getOrElse("")
          ),
          state = State(
            fields
              .get("state")
              .collect { case ujson.Str(value) => value }
              .getOrElse("")
          ),
          labels = fields
            .get("labels")
            .collect { case ujson.Arr(items) => items.toList }
            .getOrElse(Nil)
            .flatMap {
              case ujson.Obj(labelFields) =>
                labelFields.get("name").collect { case ujson.Str(name) =>
                  name
                }
              case _ => None
            }
        )
      case _ => None

  def taskRunner(issue: Issue): Option[TaskRunner] =
    taskRunners(issue).headOption

  def taskRunners(issue: Issue): List[TaskRunner] =
    val lines = issue.body.value.linesIterator.toList
    val fields = lines
      .flatMap(parseRunnerField)
      .toMap

    val listed = parseRunnerList(lines)

    val single =
      fields
        .get("agent/model/effort/version")
        .orElse(fields.get("agent/model/version"))
        .flatMap(parseRunnerTriplet)
        .orElse {
          for
            agent <- fields
              .get("agent")
              .orElse(fields.get("executor"))
              .orElse(fields.get("llm"))
            model <- fields.get("model")
            effort = fields.get("effort")
            version <- fields.get("version").orElse(fields.get("model version"))
          yield TaskRunner(
            agent = AgentBinary(agent),
            model = Some(model),
            effort = effort,
            version = Some(version)
          )
        }

    (listed ++ single.toList).distinct

  private val RunnerFieldRegex =
    """(?i)^\s*(?:[-*]\s*)?(?:preferred\s+)?(agent/model/effort/version|agent/model/version|agent|executor|llm|model|effort|model\s+version|version)\s*(?:is\s+)?(?::|=)\s*(.+?)\s*$""".r

  private def parseRunnerField(line: String): Option[(String, String)] =
    line match
      case RunnerFieldRegex(key, value) if value.nonEmpty =>
        Some(key.toLowerCase -> value.trim)
      case _ => None

  private def parseRunnerTriplet(value: String): Option[TaskRunner] =
    value.split("/", -1).toList.map(_.trim) match
      case agent :: model :: effort :: version :: Nil if agent.nonEmpty && model.nonEmpty =>
        Some(
          TaskRunner(
            AgentBinary(agent),
            Some(model),
            Option(effort).filter(_.nonEmpty),
            Option(version).filter(_.nonEmpty)
          )
        )
      case agent :: model :: version :: Nil if agent.nonEmpty && model.nonEmpty =>
        Some(
          TaskRunner(
            AgentBinary(agent),
            Some(model),
            None,
            Option(version).filter(_.nonEmpty)
          )
        )
      case _ => None

  private def parseRunnerList(lines: List[String]): List[TaskRunner] =
    val (_, runners) = lines.foldLeft((false, List.empty[TaskRunner])) { case ((inRunnerBlock, found), line) =>
      val normalized = line.trim
      val lower = normalized.toLowerCase
      if isRunnerHeader(lower) then
        val inline = parseRunnerTriplets(normalized)
        (true, found ++ inline)
      else if inRunnerBlock && (normalized.startsWith("-") || normalized
          .startsWith("*"))
      then
        val bullet = normalized.stripPrefix("-").stripPrefix("*").trim
        (inRunnerBlock, found ++ parseRunnerTriplets(bullet))
      else if inRunnerBlock && normalized.isEmpty then (false, found)
      else (inRunnerBlock, found)
    }
    runners

  private def parseRunnerTriplets(line: String): List[TaskRunner] =
    val normalized = line.trim.stripPrefix("-").stripPrefix("*").trim
    val lower = normalized.toLowerCase

    val values =
      if isRunnerHeader(lower) then
        normalized.split(":", 2).toList match
          case _ :: tail :: Nil => tail
          case _                => ""
      else normalized

    values.split(",").toList.flatMap(parseRunnerTriplet)

  private def isRunnerHeader(lower: String): Boolean =
    lower.contains("preferred llms/models/efforts/versions") ||
      lower.contains("preferred llms/models/versions") ||
      lower.contains("agent/model/effort/version") ||
      lower.contains("agent/model/version")

  private val DepLineKeywords =
    List("depends on", "depend on", "dependency", "dependencies")

  private val IssueNumRegex = """#(\d+)""".r
  private val ParentRegex = """(?i)\bparent\b\s*:?\s*#(\d+)""".r
  private val PullRequestMentionRegex =
    """(?i)\bPR\s*#(\d+)|/pull/(\d+)""".r

  def getDependencies(body: IssueBody): List[TaskNumber] =
    body.value.linesIterator
      .flatMap { line =>
        val lower = line.toLowerCase
        if DepLineKeywords.exists(keyword => lower.contains(keyword)) then
          IssueNumRegex
            .findAllMatchIn(line)
            .map(_.group(1).toInt)
            .map(TaskNumber.apply)
        else Nil
      }
      .toList
      .distinct

  def parentIds(issue: Issue): List[TaskNumber] =
    issue.body.value.linesIterator
      .flatMap(line => ParentRegex.findAllMatchIn(line).map(_.group(1).toInt))
      .toList
      .distinct
      .map(TaskNumber(_))

  def hasUnresolvedDependencies(
      issue: Issue,
      openIssueNumbers: Set[TaskNumber]
  ): Boolean =
    getDependencies(issue.body).exists(openIssueNumbers.contains)

  def hasOpenChildren(issue: Issue, openIssues: List[Issue]): Boolean =
    openIssues.exists(child => parentIds(child).contains(issue.number))

  def dependencyConclusion[F[_]](progress: String => F[Unit])(using
      F: Sync[F]
  ): Kleisli[F, (os.Path, Issue), Option[String]] =
    Kleisli.apply { case (root, task) =>
      getDependencies(task.body).distinct
        .traverse(id => singleDependencyConclusion(progress).run((root, id)))
        .map { conclusions =>
          val rendered = conclusions.flatten.map { case (id, comment) =>
            s"- #$id: $comment"
          }
          Option.when(rendered.nonEmpty)(rendered.mkString("\n"))
        }
    }

  private def singleDependencyConclusion[F[_]](progress: String => F[Unit])(using
      F: Sync[F]
  ): Kleisli[F, (os.Path, TaskNumber), Option[(TaskNumber, String)]] =
    Kleisli.apply { case (root, dependencyId) =>
      progress(
        s"Found dependency task #$dependencyId. Fetching conclusion comment..."
      ) *>
        F.blocking {
          Try {
            val res = os
              .proc(
                "gh",
                "issue",
                "view",
                dependencyId.toString,
                "--json",
                "comments"
              )
              .call(cwd = root)
            val comments = ujson.read(res.out.text())("comments").arr.toList
            comments
              .find(
                commentBody(_).exists(_.toLowerCase.contains("conclusion"))
              )
              .orElse(comments.lastOption)
              .flatMap(commentBody)
          }.toEither
        }.flatMap {
          case Right(comment) => F.pure(comment.map(dependencyId -> _))
          case Left(error) =>
            progress(
              s"Failed to read comments for dependency task #$dependencyId: ${error.getMessage}"
            ).as(None)
        }
    }

  /** Builds the context block given to the agent when a task has prior execution
    * history.
    *
    * This exists so reopened, retried, or resumed tasks do not start from a blank
    * prompt. The executor reads the issue's recent comments and any pull requests
    * linked from the issue, then enriches those pull requests with their workflow
    * runs. That gives the next agent run the previous conclusion, restart notes,
    * related PRs, and CI failures/pending checks it needs to continue or repair the
    * task instead of repeating already-completed work.
    *
    * The method returns `None` when the issue has no automation history and no
    * related pull requests, keeping normal first-run prompts unchanged. If GitHub
    * history cannot be read, it logs progress and treats the task as having no
    * replay context so a transient GitHub read problem does not block claiming the
    * task.
    */
  def replayContext[F[_]](progress: String => F[Unit])(using
      F: Sync[F]
  ): Kleisli[F, (os.Path, Issue), Option[String]] =
    Kleisli.apply { case (root, task) =>
      for
        history <- issueHistory((root, task.number)).handleErrorWith { error =>
          progress(
            s"Failed to read replay history for task #${task.number}: ${error.getMessage}"
          ).as(IssueHistory(Nil, Nil))
        }
        pullRequests <- history.pullRequests.traverse(pr => pullRequestCiContext((root, pr)))
        context = formatReplayContext(history.comments, pullRequests.flatten)
        replay = Option.when(context.trim.nonEmpty)(context)
        _ <- replay.fold(F.unit)(_ =>
          progress(
            s"Found prior task history for #${task.number}. Running in replay/continue mode."
          )
        )
      yield replay
    }

  def verifyRelatedPullRequestCiForTask[F[_]](progress: String => F[Unit])(using
      F: Sync[F]
  ): Kleisli[F, (os.Path, Issue, BranchName), Unit] =
    Kleisli.apply { case (root, task, branchName) =>
      def relatedPullRequests: F[List[PullRequestCiContext]] =
        for
          currentPullRequest <- pullRequestCiContextForBranch((root, branchName))
          historicalPullRequests <-
            currentPullRequest.fold {
              for
                history <- issueHistory((root, task.number)).handleErrorWith { error =>
                  progress(
                    s"Failed to read related pull request CI history for task #${task.number}: ${error.getMessage}"
                  ).as(IssueHistory(Nil, Nil))
                }
                pullRequests <- history.pullRequests.traverse(pr => pullRequestCiContext((root, pr)))
              yield pullRequests.flatten
            }(pullRequest => List(pullRequest).pure[F])
        yield historicalPullRequests

      def loop(deadlineMillis: DeadlineMillis): F[Unit] =
        for
          now <- F.blocking(System.currentTimeMillis())
          pullRequests <- relatedPullRequests
          failure = relatedPullRequestCiFailureMessage(pullRequests)
          pending = relatedPullRequestCiPendingMessage(pullRequests)
          _ <- failure match
            case Some(message) =>
              F.raiseError(new RuntimeException(message))
            case None if now >= deadlineMillis.value && pending.nonEmpty =>
              F.raiseError(
                new RuntimeException(
                  s"Timed out waiting for related pull request CI: ${pending.get}"
                )
              )
            case None =>
              pending match
                case Some(message) =>
                  progress(message) *>
                    F.blocking(
                      Thread.sleep(PullRequestCheckPollMillis.value)
                    ) *>
                    loop(deadlineMillis)
                case None if pullRequests.nonEmpty =>
                  progress(
                    s"Related pull request CI is passing for task #${task.number}."
                  )
                case None =>
                  progress(
                    s"No related pull request CI found for task #${task.number}."
                  )
        yield ()

      for
        _ <- progress(
          s"Waiting for related pull request CI on task #${task.number} with ${PullRequestCheckTimeoutMillis.value / 1000}s timeout..."
        )
        started <- F.blocking(System.currentTimeMillis())
        _ <- loop(DeadlineMillis(started) + PullRequestCheckTimeoutMillis)
      yield ()
    }

  def hasOpenPullRequestForBranch[F[_]: Sync]: Kleisli[F, (os.Path, BranchName), Boolean] =
    Kleisli.apply { case (root, branchName) =>
      pullRequestForBranch((root, branchName)).map(_.exists(_.state.value === "OPEN"))
    }

  private final case class IssueHistory(
      comments: List[IssueComment],
      pullRequests: List[TaskNumber]
  )

  private final case class IssueComment(
      author: Author,
      createdAt: CreatedAt,
      body: IssueCommentBody
  )

  private def issueHistory[F[_]: Sync]: Kleisli[F, (os.Path, TaskNumber), IssueHistory] =
    Kleisli.apply { case (root, taskId) =>
      callOutput(
        root,
        "gh",
        "issue",
        "view",
        taskId.toString,
        "--json",
        "comments,closedByPullRequestsReferences"
      ).map(parseIssueHistory)
    }

  private def parseIssueHistory(output: String): IssueHistory =
    Try(ujson.read(output).obj).toOption.fold(IssueHistory(Nil, Nil)) { fields =>
      val comments = fields
        .get("comments")
        .collect { case ujson.Arr(values) =>
          values.toList.flatMap(parseIssueComment)
        }
        .getOrElse(Nil)
      val pullRequests = fields
        .get("closedByPullRequestsReferences")
        .collect { case ujson.Arr(values) =>
          values.toList.flatMap(parsePullRequestNumber)
        }
        .getOrElse(Nil)
        .map(TaskNumber.apply)
      val mentionedPullRequests =
        comments
          .flatMap(comment => pullRequestMentions(comment.body))
          .map(TaskNumber.apply)
      IssueHistory(comments, (pullRequests ++ mentionedPullRequests).distinct)
    }

  private def parseIssueComment(value: ujson.Value): Option[IssueComment] =
    value match
      case ujson.Obj(fields) =>
        for body <- fields.get("body").collect { case ujson.Str(value) =>
            value
          }
        yield IssueComment(
          author = Author(
            fields
              .get("author")
              .collect { case ujson.Obj(authorFields) =>
                authorFields
                  .get("login")
                  .collect { case ujson.Str(value) => value }
              }
              .flatten
              .getOrElse("unknown")
          ),
          createdAt = CreatedAt(
            fields
              .get("createdAt")
              .collect { case ujson.Str(value) => value }
              .getOrElse("unknown time")
          ),
          body = IssueCommentBody(body)
        )
      case _ => None

  private def parsePullRequestNumber(value: ujson.Value): Option[Int] =
    value match
      case ujson.Obj(fields) =>
        fields.get("number").collect { case ujson.Num(value) => value.toInt }
      case _ => None

  private def pullRequestMentions(value: IssueCommentBody): List[Int] =
    PullRequestMentionRegex
      .findAllMatchIn(value.value)
      .flatMap(matchResult =>
        List(
          Option(matchResult.group(1)),
          Option(matchResult.group(2))
        ).flatten.headOption
          .flatMap(_.toIntOption)
      )
      .toList

  private final case class PullRequestCiContext(
      number: Int,
      state: String,
      url: Url,
      baseRefName: BranchName,
      headRefName: BranchName,
      mergeCommit: Option[CommitSha],
      runs: List[WorkflowRun]
  )

  private def pullRequestCiContext[F[_]](using
      F: Sync[F]
  ): Kleisli[F, (os.Path, TaskNumber), Option[PullRequestCiContext]] =
    Kleisli.apply { case (root, number) =>
      callOutputUnchecked(
        root,
        "gh",
        "pr",
        "view",
        number.toString,
        "--json",
        "number,state,url,baseRefName,headRefName,mergeCommit"
      ).flatMap { output =>
        parsePullRequestCiContext(output).traverse { ciContext =>
          ciContext.mergeCommit.fold(ciContext.copy(runs = Nil).pure[F]) { commit =>
            workflowRuns((root, ciContext.baseRefName, commit))
              .map(runs => ciContext.copy(runs = runs))
          }
        }
      }
    }

  private def pullRequestCiContextForBranch[F[_]](using
      F: Sync[F]
  ): Kleisli[F, (os.Path, BranchName), Option[PullRequestCiContext]] =
    Kleisli.apply { case (root, branchName) =>
      pullRequestForBranch((root, branchName)).flatMap {
        case Some(pullRequest) =>
          pullRequestCiContext((root, pullRequest.number))
        case None => F.pure(None)
      }
    }

  private def parsePullRequestCiContext(
      output: String
  ): Option[PullRequestCiContext] =
    Try(ujson.read(output).obj).toOption.flatMap { fields =>
      for
        number <- fields.get("number").collect { case ujson.Num(value) =>
          value.toInt
        }
        state <- fields.get("state").collect { case ujson.Str(value) =>
          value
        }
      yield PullRequestCiContext(
        number = number,
        state = state,
        url = Url(
          fields
            .get("url")
            .collect { case ujson.Str(value) => value }
            .getOrElse("")
        ),
        baseRefName = BranchName(
          fields
            .get("baseRefName")
            .collect { case ujson.Str(value) => value }
            .getOrElse("")
        ),
        headRefName = BranchName(
          fields
            .get("headRefName")
            .collect { case ujson.Str(value) => value }
            .getOrElse("")
        ),
        mergeCommit = fields
          .get("mergeCommit")
          .collect { case ujson.Obj(commitFields) =>
            commitFields.get("oid").collect { case ujson.Str(value) => value }
          }
          .flatten
          .map(CommitSha.apply),
        runs = Nil
      )
    }

  private def workflowRuns[F[_]: Sync]: Kleisli[F, (os.Path, BranchName, CommitSha), List[WorkflowRun]] =
    Kleisli.apply { case (root, branchName, commitSha) =>
      callOutputUnchecked(
        root,
        "gh",
        "run",
        "list",
        "--branch",
        branchName.value,
        "--commit",
        commitSha.value,
        "--limit",
        "100",
        "--json",
        "name,status,conclusion,url"
      ).map(parseWorkflowRuns)
    }

  private def formatReplayContext(
      comments: List[IssueComment],
      pullRequests: List[PullRequestCiContext]
  ): String =
    val relevantComments = comments.takeRight(8)
    val hasAutomationHistory = comments.exists(comment =>
      val lower = comment.body.value.toLowerCase
      lower.startsWith("llm run output:") ||
      lower.startsWith("script conclusion:") ||
      lower.startsWith("task run output:")
    )
    val shouldReplay = pullRequests.nonEmpty || hasAutomationHistory

    if !shouldReplay then ""
    else
      val commentsText =
        if relevantComments.isEmpty then "No recent issue comments."
        else
          relevantComments
            .map(comment => s"- ${comment.createdAt} @${comment.author}: ${truncate(comment.body, 2000)}")
            .mkString("\n")
      val prText =
        if pullRequests.isEmpty then "No related pull requests found."
        else
          pullRequests
            .map { pr =>
              val runText =
                if pr.runs.isEmpty then "no related workflow runs found"
                else formatRuns(pr.runs)
              s"- PR #${pr.number} ${pr.state} ${pr.url} head=${pr.headRefName} base=${pr.baseRefName} merge=${pr.mergeCommit
                  .getOrElse("none")} runs=[$runText]"
            }
            .mkString("\n")

      s"""Recent issue comments:
$commentsText

Related pull requests and CI runs:
$prText
"""

  private def relatedPullRequestCiFailureMessage(
      pullRequests: List[PullRequestCiContext]
  ): Option[String] =
    val prsWithRuns = pullRequests.filter(_.runs.nonEmpty)
    val failed =
      prsWithRuns.flatMap(pr => pr.runs.filter(_.failed).map(pr -> _))

    if failed.nonEmpty then
      Some(
        "Related pull request CI failed: " + failed
          .map { case (pr, run) =>
            s"PR #${pr.number} ${run.name}=${run.status}/${run.conclusion} ${run.url}"
          }
          .mkString(", ")
      )
    else None

  private def relatedPullRequestCiPendingMessage(
      pullRequests: List[PullRequestCiContext]
  ): Option[String] =
    val prsWithRuns = pullRequests.filter(_.runs.nonEmpty)
    val pending =
      prsWithRuns.flatMap(pr => pr.runs.filterNot(_.passed).map(pr -> _))

    if pending.nonEmpty then
      Some(
        "Related pull request CI is still pending: " + pending
          .map { case (pr, run) =>
            val state =
              if run.conclusion.nonEmpty then s"${run.status}/${run.conclusion}"
              else run.status
            s"PR #${pr.number} ${run.name}=$state ${run.url}"
          }
          .mkString(", ")
      )
    else None

  private def truncate(value: IssueCommentBody, maxLength: Int): String =
    val normalized = value.value.linesIterator.mkString("\\n")
    if normalized.length <= maxLength then normalized
    else normalized.take(maxLength) + "... [truncated]"

  private def commentBody(value: ujson.Value): Option[String] =
    value match
      case ujson.Obj(fields) =>
        fields.get("body").collect { case ujson.Str(body) => body }
      case _ => None

  def commentConclusion[F[_]](progress: String => F[Unit])(using
      Sync[F]
  ): Kleisli[F, (os.Path, Issue, TaskRunner, Option[String]), Unit] =
    Kleisli.apply { case (root, task, runner, conclusion) =>
      val conclusionLine =
        conclusion.fold("")(text => s"\nConclusion:\n$text\n")
      val body =
        s"""Script conclusion:
Task #${task.number} completed successfully.
$conclusionLine
Runner:
${runner.display}
"""
      progress(s"Leaving conclusion comment on task #${task.number}...") *>
        call(
          root,
          "gh",
          "issue",
          "comment",
          task.number.toString,
          "--body",
          body
        )
    }

  private val ScriptCommentPrefixes = List(
    "llm run output:",
    "script conclusion:",
    "questions before execution:",
    "script stopped before closing task",
    "warning. split expected but did not exist",
    "was evaluated as needing split subtasks",
    "are completed. parent task is ready for completion check",
    "completed successfully. worktree closed",
    TaskMetadata.MetadataCommentPrefix
  )

  // Chronological (oldest first) bodies of comments this script posted to
  // persist TaskMetadata, so TaskMetadataStore can fold them into one merged
  // view without ever touching the issue body.
  def metadataCommentBodies[F[_]](using
      F: Sync[F]
  ): Kleisli[F, (os.Path, TaskNumber), List[IssueCommentBody]] =
    Kleisli.apply { case (root, taskId) =>
      issueHistory((root, taskId))
        .handleErrorWith(_ => F.pure(IssueHistory(Nil, Nil)))
        .map(
          _.comments
            .map(_.body)
            .filter(
              _.value.trim.toLowerCase
                .startsWith(TaskMetadata.MetadataCommentPrefix)
            )
        )
    }

  def commentTaskMetadata[F[_]](progress: String => F[Unit])(using
      Sync[F]
  ): Kleisli[F, (os.Path, TaskNumber, IssueBody), Unit] =
    Kleisli.apply { case (root, taskId, metadataText) =>
      progress(s"Leaving metadata comment on task #$taskId...") *>
        call(
          root,
          "gh",
          "issue",
          "comment",
          taskId.toString,
          "--body",
          metadataText.value
        )
    }

  private def isScriptComment(body: IssueCommentBody): Boolean =
    val lower = body.value.trim.toLowerCase
    ScriptCommentPrefixes.exists(prefix => lower.startsWith(prefix) || lower.contains(prefix))

  def hasCompletedLabel(task: Issue): Boolean =
    task.labels.exists(label => label === "status: completed" || label === "completed")

  def hasCompletionComment[F[_]](using
      F: Sync[F]
  ): Kleisli[F, (os.Path, Issue), Boolean] =
    Kleisli.apply { case (root, task) =>
      issueHistory((root, task.number))
        .handleErrorWith(_ => F.pure(IssueHistory(Nil, Nil)))
        .map(_.comments.exists { comment =>
          comment.body.value.trim.toLowerCase.contains(
            "completed successfully. worktree closed"
          )
        })
    }

  // True only if the script actually asked something. Metadata alone can
  // claim needs-input (manual edit, stale state) without any question ever
  // being posted; that must not count as a real block.
  def hasQuestionComment[F[_]](using
      F: Sync[F]
  ): Kleisli[F, (os.Path, Issue), Boolean] =
    Kleisli.apply { case (root, task) =>
      issueHistory((root, task.number))
        .handleErrorWith(_ => F.pure(IssueHistory(Nil, Nil)))
        .map(_.comments.exists { comment =>
          comment.body.value.trim.toLowerCase
            .startsWith("questions before execution:")
        })
    }

  // Looks for a human reply left after the script's most recent
  // "Questions before execution:" comment, so a needs-input task can be
  // unblocked without a manual issue-body edit.
  def userAnswer[F[_]](progress: String => F[Unit])(using
      F: Sync[F]
  ): Kleisli[F, (os.Path, Issue), Option[String]] =
    Kleisli.apply { case (root, task) =>
      issueHistory((root, task.number))
        .handleErrorWith(_ => F.pure(IssueHistory(Nil, Nil)))
        .flatMap { history =>
          val comments = history.comments
          val lastQuestionIndex = comments.lastIndexWhere(comment =>
            comment.body.value.trim.toLowerCase
              .startsWith("questions before execution:")
          )
          if lastQuestionIndex < 0 then F.pure(None)
          else
            val replies = comments
              .drop(lastQuestionIndex + 1)
              .filterNot(comment => isScriptComment(comment.body))
            if replies.isEmpty then F.pure(None)
            else
              val answer = replies
                .map(comment => s"@${comment.author}: ${comment.body.value.trim}")
                .mkString("\n\n")
              progress(
                s"Found user answer to prior questions on task #${task.number}."
              ).as(Some(answer))
        }
    }

  def commentNeedsUserInput[F[_]](progress: String => F[Unit])(using
      Sync[F]
  ): Kleisli[F, (os.Path, Issue, String), Unit] =
    Kleisli.apply { case (root, task, questions) =>
      val body =
        s"""Questions before execution:
$questions
"""
      progress(s"Leaving questions on task #${task.number}...") *>
        call(
          root,
          "gh",
          "issue",
          "comment",
          task.number.toString,
          "--body",
          body
        )
    }

  def commentTaskFailure[F[_]](progress: String => F[Unit])(using
      Sync[F]
  ): Kleisli[F, (os.Path, Issue, String), Unit] =
    Kleisli.apply { case (root, task, reason) =>
      val body =
        s"""Script stopped before closing task #${task.number}.

Reason:
$reason
"""
      progress(s"Leaving failure comment on task #${task.number}...") *>
        call(
          root,
          "gh",
          "issue",
          "comment",
          task.number.toString,
          "--body",
          body
        )
    }

  def commentSplitMissingWarning[F[_]](progress: String => F[Unit])(using
      Sync[F]
  ): Kleisli[F, (os.Path, Issue), Unit] =
    Kleisli.apply { case (root, task) =>
      val body =
        s"""Warning. split expected but did not exist. Creating missing split subtasks without re-evaluating.

Task #${task.number} was evaluated as needing split subtasks, but no open child issues reference it as parent.
"""
      progress(s"Leaving split-missing warning on task #${task.number}...") *>
        call(
          root,
          "gh",
          "issue",
          "comment",
          task.number.toString,
          "--body",
          body
        )
    }

  def commentSplitEvaluation[F[_]](progress: String => F[Unit])(using
      Sync[F]
  ): Kleisli[F, (os.Path, Issue), Unit] =
    Kleisli.apply { case (root, task) =>
      val body =
        s"""Task #${task.number} was evaluated as needing split subtasks.

This parent task will not be implemented directly. Run child tasks first; when all children are completed, the parent will be eligible for completion check.
"""
      progress(
        s"Leaving split-evaluation comment on task #${task.number}..."
      ) *>
        call(
          root,
          "gh",
          "issue",
          "comment",
          task.number.toString,
          "--body",
          body
        )
    }

  // A split task's subtasks merge into a shared integration branch
  // (task-<parentId>, see main.scala's taskRun) rather than each landing on
  // the default branch independently. Once every subtask issue is closed,
  // that integration branch is itself merged into the default branch in one
  // shot, and only then is the parent issue closed - so a split feature
  // always lands atomically, never part-by-part.
  def checkParentsForCompletion[F[_]](progress: String => F[Unit])(using
      F: Sync[F]
  ): Kleisli[F, (os.Path, Issue), Unit] =
    Kleisli.apply { case (root, task) =>
      parentIds(task).traverse_ { parentId =>
        for
          openIssues <- fetchIssues(root)
          openChildren = openIssues.filter(child => parentIds(child).contains(parentId))
          _ <-
            if openChildren.isEmpty then
              progress(
                s"All child tasks for parent #$parentId are completed. Merging integration branch into the default branch..."
              ) *> mergeIntegrationBranch(progress)((root, parentId))
            else
              progress(
                s"Parent #$parentId still has ${openChildren.size} open child task(s)."
              )
        yield ()
      }
    }

  private def mergeIntegrationBranch[F[_]](progress: String => F[Unit])(using
      F: Sync[F]
  ): Kleisli[F, (os.Path, TaskNumber), Unit] =
    Kleisli.apply { case (root, parentId) =>
      val branchName = BranchName(s"task-$parentId")
      F.blocking {
        os.proc("git", "rev-parse", "--verify", branchName.value)
          .call(cwd = root, stdout = os.Pipe, stderr = os.Pipe, check = false)
          .exitCode === 0
      }.flatMap {
        case false =>
          progress(
            s"No integration branch $branchName found for parent #$parentId; nothing to merge."
          )
        case true =>
          for
            pullRequest <- ensurePullRequest(progress).run(
              (
                root,
                branchName,
                None,
                parentId,
                IssueTitle(s"Integrate subtasks for #$parentId"),
                Some(s"Integrate subtasks for #$parentId"),
                Some(
                  s"Merges completed subtasks of #$parentId into the default branch.\n\nCloses #$parentId"
                )
              )
            )
            _ <- awaitPullRequestChecks(progress).run(
              (
                root,
                BranchName(pullRequest.number.value.toString),
                PullRequestCheckTimeoutMillis,
                PullRequestCheckPollMillis
              )
            )
            _ <- progress("Merging integration Pull Request...")
            mergeResult <- call(
              root,
              "gh",
              "pr",
              "merge",
              pullRequest.number.toString,
              "--merge"
            ).as(true).handleErrorWith { error =>
              val msg = error.getMessage.toLowerCase
              val isConflict = msg.contains("conflict") || msg
                .contains("mergeable") || msg.contains("fail")
              if isConflict then
                for
                  _ <- progress(
                    s"WARNING: Merge conflict or failure detected on integration Pull Request #${pullRequest.number}. " +
                      s"Parent task #$parentId cannot be merged automatically. Please resolve the conflicts and merge PR #${pullRequest.number} manually on GitHub."
                  )
                  _ <- call(
                    root,
                    "gh",
                    "issue",
                    "comment",
                    parentId.toString,
                    "--body",
                    s"Merge conflict or merge failure detected on integration Pull Request #${pullRequest.number}. Please resolve the conflicts on GitHub to close this task."
                  ).void.handleErrorWith { commentError =>
                    progress(
                      s"Failed to leave conflict comment on parent #$parentId: ${commentError.getMessage}"
                    )
                  }
                yield false
              else F.raiseError(error)
            }
            _ <-
              if mergeResult then
                for
                  merged <- mergedPullRequest((root, pullRequest.number))
                  _ <- awaitBranchChecks(progress)(
                    (
                      root,
                      merged.baseRefName,
                      merged.mergeCommit,
                      PullRequestCheckTimeoutMillis,
                      PullRequestCheckPollMillis
                    )
                  )
                  _ <- setIssueStatus(progress)((root, parentId, "completed"))
                  _ <- closeIssue((root, parentId))
                yield ()
              else F.unit
          yield ()
      }
    }

  // Assumes the branch has already been pushed (see Git[F].push in main.scala's
  // publishRemote, which owns the push-failure repair/retry loop) — this just
  // opens/reuses the PR and merges it.
  def createAndMergePr[F[_]](
      progress: String => F[Unit]
  )(using Sync[F]): Kleisli[
    F,
    (
        os.Path,
        os.Path,
        BranchName,
        Option[BranchName],
        Issue,
        Option[String],
        Option[String]
    ),
    Unit
  ] =
    Kleisli.apply {
      case (
            root,
            worktreePath,
            branchName,
            baseBranch,
            task,
            pullRequestTitle,
            pullRequestBody
          ) =>
        for
          pullRequest <- ensurePullRequest(progress).run(
            (
              worktreePath,
              branchName,
              baseBranch,
              task.number,
              task.title,
              pullRequestTitle,
              pullRequestBody
            )
          )
          _ <- mergeAndVerify(progress)((root, pullRequest))
        yield ()
    }

  // Verifies checks, merges, then verifies the base branch's own CI on the
  // merge commit. Shared by pushCreateAndMergePr (fresh PR) and
  // resumeOpenPullRequest (an already-pushed PR from a prior, interrupted
  // run) so both paths finish a task the same way.
  private def mergeAndVerify[F[_]](progress: String => F[Unit])(using
      F: Sync[F]
  ): Kleisli[F, (os.Path, PullRequest), Unit] =
    Kleisli.apply { case (root, pullRequest) =>
      for
        _ <- awaitPullRequestChecks(progress)(
          (
            root,
            BranchName(pullRequest.number.value.toString),
            PullRequestCheckTimeoutMillis,
            PullRequestCheckPollMillis
          )
        )
        _ <- progress("Merging Pull Request...")
        _ <- call(
          root,
          "gh",
          "pr",
          "merge",
          pullRequest.number.toString,
          "--merge"
        )
        merged <- mergedPullRequest((root, pullRequest.number))
        _ <-
          if merged.baseRefName.value === "master" || merged.baseRefName.value === "main"
          then
            awaitBranchChecks(progress)(
              (
                root,
                merged.baseRefName,
                merged.mergeCommit,
                PullRequestCheckTimeoutMillis,
                PullRequestCheckPollMillis
              )
            )
          else
            progress(
              s"Skipping branch checks for non-default base branch ${merged.baseRefName.value}."
            )
      yield ()
    }

  // A prior run may have pushed a branch and opened a PR but exited before
  // merging (e.g. checks were still pending, or the process was interrupted
  // after push). selectTask then refuses to re-run the implementer while
  // that PR is open (see main.scala hasOpenPullRequestForBranch check), so
  // this resumes exactly where that run left off: verify checks, merge,
  // verify the base branch's post-merge CI.
  def resumeOpenPullRequest[F[_]](progress: String => F[Unit])(using
      F: Sync[F]
  ): Kleisli[F, (os.Path, BranchName), Unit] =
    Kleisli.apply { case (root, branchName) =>
      pullRequestForBranch((root, branchName)).flatMap {
        case Some(pullRequest) if pullRequest.state.value === "OPEN" =>
          progress(
            s"Found open Pull Request #${pullRequest.number} for $branchName; verifying checks and merging..."
          ) *> mergeAndVerify(progress)((root, pullRequest))
        case _ =>
          F.raiseError(
            NoOpenPullRequestToResumeException(branchName)
          )
      }
    }

  private final case class PullRequest(number: TaskNumber, state: State)

  private def ensurePullRequest[F[_]](
      progress: String => F[Unit]
  )(using F: Sync[F]): Kleisli[
    F,
    (
        os.Path,
        BranchName,
        Option[BranchName],
        TaskNumber,
        IssueTitle,
        Option[String],
        Option[String]
    ),
    PullRequest
  ] =
    Kleisli.apply {
      case (
            worktreePath,
            branchName,
            baseBranch,
            taskNumber,
            taskTitle,
            pullRequestTitle,
            pullRequestBody
          ) =>
        pullRequestForBranch((worktreePath, branchName)).flatMap {
          case Some(pullRequest) if pullRequest.state.value === "OPEN" =>
            progress(
              s"Pull Request #${pullRequest.number} for $branchName already exists."
            ).as(pullRequest)
          case _ =>
            progress("Creating Pull Request...") *>
              call(
                worktreePath,
                Seq(
                  "gh",
                  "pr",
                  "create",
                  "--title",
                  pullRequestTitle
                    .filter(_.trim.nonEmpty)
                    .getOrElse(s"Task #$taskNumber: ${taskTitle.value}"),
                  "--body",
                  pullRequestBody
                    .filter(_.trim.nonEmpty)
                    .getOrElse(s"Closes #$taskNumber"),
                  "--head",
                  branchName.value
                ) ++ baseBranch.toList
                  .map(_.value)
                  .flatMap(base => Seq("--base", base))*
              ) *>
              pullRequestForBranch((worktreePath, branchName)).flatMap {
                case Some(pullRequest) if pullRequest.state.value === "OPEN" =>
                  pullRequest.pure[F]
                case _ =>
                  F.raiseError(
                    new RuntimeException(
                      s"Could not find open Pull Request for $branchName after creation."
                    )
                  )
              }
        }
    }

  private def pullRequestForBranch[F[_]](using
      F: Sync[F]
  ): Kleisli[F, (os.Path, BranchName), Option[PullRequest]] =
    Kleisli.apply { case (root, branchName) =>
      callOutputUnchecked(
        root,
        "gh",
        "pr",
        "view",
        branchName.value,
        "--json",
        "number,state"
      ).map(parsePullRequest)
    }

  private def parsePullRequest(output: String): Option[PullRequest] =
    Try(ujson.read(output).obj).toOption.flatMap { fields =>
      for
        number <- fields.get("number").collect { case ujson.Num(value) =>
          TaskNumber(value.toInt)
        }
        state <- fields.get("state").collect { case ujson.Str(value) =>
          value.trim.toUpperCase
        }
      yield PullRequest(number, State(state))
    }

  private def awaitPullRequestChecks[F[_]](progress: String => F[Unit])(using
      F: Sync[F]
  ): Kleisli[F, (os.Path, BranchName, DeadlineMillis, DeadlineMillis), Unit] =
    Kleisli.apply { case (root, branchName, timeoutMillis, pollMillis) =>
      def loop(
          deadlineMillis: DeadlineMillis,
          noChecksSince: Option[Long]
      ): F[Unit] =
        for
          now <- F.blocking(System.currentTimeMillis())
          _ <-
            if now >= deadlineMillis.value then
              F.raiseError(
                new RuntimeException(
                  s"Timed out waiting for Pull Request checks on $branchName."
                )
              )
            else
              pullRequestCheckStatus((root, branchName)).flatMap {
                case PullRequestChecksPending(message) =>
                  progress(message.value) *>
                    F.blocking(Thread.sleep(pollMillis.value)) *>
                    loop(deadlineMillis, None)
                case PullRequestChecksUnavailable(message) =>
                  pullRequestMergeConflict((root, branchName)).flatMap {
                    case true =>
                      F.raiseError(
                        new RuntimeException(
                          s"Pull Request for $branchName has merge conflicts with its base branch; GitHub cannot trigger checks until resolved."
                        )
                      )
                    case false =>
                      val firstSeen = noChecksSince.getOrElse(now)
                      if now - firstSeen >= PullRequestNoChecksGraceMillis.value
                      then
                        progress(
                          s"$message Proceeding after ${PullRequestNoChecksGraceMillis.value / 1000}s with no PR checks."
                        )
                      else
                        progress(message.value) *>
                          F.blocking(Thread.sleep(pollMillis.value)) *>
                          loop(deadlineMillis, Some(firstSeen))
                  }
                case PullRequestChecksPassed(message) =>
                  progress(message.value)
                case PullRequestChecksFailed(message) =>
                  F.raiseError(new RuntimeException(message.value))
              }
        yield ()

      for
        _ <- progress(
          s"Waiting for Pull Request checks on $branchName with ${timeoutMillis.value / 1000}s timeout..."
        )
        started <- F.blocking(System.currentTimeMillis())
        _ <- loop(DeadlineMillis(started) + timeoutMillis, None)
      yield ()
    }

  private def pullRequestMergeConflict[F[_]](using
      F: Sync[F]
  ): Kleisli[F, (os.Path, BranchName), Boolean] =
    Kleisli.apply { case (root, branchName) =>
      callOutputUnchecked(
        root,
        "gh",
        "pr",
        "view",
        branchName.value,
        "--json",
        "mergeable"
      ).map { output =>
        Try(ujson.read(output).obj).toOption
          .flatMap(_.get("mergeable"))
          .collect { case ujson.Str(value) => value.trim.toUpperCase }
          .contains("CONFLICTING")
      }
    }

  private sealed trait PullRequestCheckStatus

  private final case class PullRequestChecksPending(message: Message3) extends PullRequestCheckStatus

  private final case class PullRequestChecksUnavailable(message: Message4) extends PullRequestCheckStatus

  private final case class PullRequestChecksPassed(message: Message2) extends PullRequestCheckStatus

  private final case class PullRequestChecksFailed(message: Message) extends PullRequestCheckStatus

  private def pullRequestCheckStatus[F[_]](using
      F: Sync[F]
  ): Kleisli[F, (os.Path, BranchName), PullRequestCheckStatus] =
    Kleisli.apply { case (root, branchName) =>
      callOutputUnchecked(
        root,
        "gh",
        "pr",
        "checks",
        branchName.value,
        "--json",
        "name,state"
      ).map { output =>
        val checks = parsePullRequestChecks(output)
        val failed = checks.filter(check =>
          Set("FAIL", "FAILED", "FAILURE", "ERROR", "CANCELLED", "TIMED_OUT")
            .contains(check.state.value)
        )
        val pending = checks.filterNot(check =>
          Set("SUCCESS", "PASS", "PASSED", "SKIPPED", "NEUTRAL")
            .contains(check.state.value)
        )
        if output.trim.isEmpty || checks.isEmpty then
          PullRequestChecksUnavailable(
            Message4(s"Pull Request checks for $branchName are not available yet.")
          )
        else if failed.nonEmpty then
          PullRequestChecksFailed(
            Message(s"Pull Request checks failed for $branchName: ${formatChecks(failed)}")
          )
        else if pending.nonEmpty then
          PullRequestChecksPending(
            Message3(s"Pull Request checks pending for $branchName: ${formatChecks(pending)}")
          )
        else
          PullRequestChecksPassed(
            Message2(s"Pull Request checks passed for $branchName.")
          )
      }
    }

  private final case class PullRequestCheck(name: Name, state: State2)

  private def parsePullRequestChecks(output: String): List[PullRequestCheck] =
    Try(ujson.read(output).arr.toList).toOption.toList.flatten.flatMap {
      case ujson.Obj(fields) =>
        for
          name <- fields.get("name").collect { case ujson.Str(value) => value }
          state <- fields.get("state").collect { case ujson.Str(value) =>
            value.trim.toUpperCase
          }
        yield PullRequestCheck(Name(name), State2(state))
      case _ => None
    }

  private def formatChecks(checks: List[PullRequestCheck]): String =
    checks.map(check => s"${check.name}=${check.state}").mkString(", ")

  private final case class MergedPullRequest(
      baseRefName: BranchName,
      mergeCommit: CommitSha
  )

  private def mergedPullRequest[F[_]](using
      F: Sync[F]
  ): Kleisli[F, (os.Path, TaskNumber), MergedPullRequest] =
    Kleisli.apply { case (root, pullRequestNumber) =>
      callOutput(
        root,
        "gh",
        "pr",
        "view",
        pullRequestNumber.toString,
        "--json",
        "baseRefName,mergeCommit"
      ).flatMap { output =>
        parseMergedPullRequest(output).liftTo[F](
          new RuntimeException(
            s"Could not read merged Pull Request metadata for #$pullRequestNumber."
          )
        )
      }
    }

  private def parseMergedPullRequest(
      output: String
  ): Option[MergedPullRequest] =
    Try(ujson.read(output).obj).toOption.flatMap { fields =>
      for
        baseRefName <- fields.get("baseRefName").collect { case ujson.Str(value) =>
          value
        }
        mergeCommit <- fields
          .get("mergeCommit")
          .collect { case ujson.Obj(commitFields) =>
            commitFields.get("oid").collect { case ujson.Str(value) =>
              CommitSha(value)
            }
          }
          .flatten
      yield MergedPullRequest(BranchName(baseRefName), mergeCommit)
    }

  private def awaitBranchChecks[F[_]](
      progress: String => F[Unit]
  )(using F: Sync[F]): Kleisli[
    F,
    (os.Path, BranchName, CommitSha, DeadlineMillis, DeadlineMillis),
    Unit
  ] =
    Kleisli.apply { case (root, branchName, commitSha, timeoutMillis, pollMillis) =>
      def loop(deadlineMillis: DeadlineMillis): F[Unit] =
        for
          now <- F.blocking(System.currentTimeMillis())
          _ <-
            if now >= deadlineMillis.value then
              F.raiseError(
                new RuntimeException(
                  s"Timed out waiting for CI on $branchName at $commitSha."
                )
              )
            else
              branchCheckStatus((root, branchName, commitSha)).flatMap {
                case BranchChecksPending(message) =>
                  progress(message.value) *>
                    F.blocking(Thread.sleep(pollMillis.value)) *>
                    loop(deadlineMillis)
                case BranchChecksPassed(message) =>
                  progress(message.value)
                case BranchChecksFailed(message) =>
                  F.raiseError(new RuntimeException(message.value))
              }
        yield ()

      for
        _ <- progress(
          s"Waiting for CI on $branchName at $commitSha with ${timeoutMillis.value / 1000}s timeout..."
        )
        started <- F.blocking(System.currentTimeMillis())
        _ <- loop(DeadlineMillis(started) + timeoutMillis)
      yield ()
    }

  private sealed trait BranchCheckStatus

  private final case class BranchChecksPending(
      message: BranchChecksPendingMessage
  ) extends BranchCheckStatus

  private final case class BranchChecksPassed(
      message: BranchChecksPassedMessage
  ) extends BranchCheckStatus

  private final case class BranchChecksFailed(
      message: BranchChecksFailedMessage
  ) extends BranchCheckStatus

  private def branchCheckStatus[F[_]](using
      F: Sync[F]
  ): Kleisli[F, (os.Path, BranchName, CommitSha), BranchCheckStatus] =
    Kleisli.apply { case (root, branchName, commitSha) =>
      callOutputUnchecked(
        root,
        "gh",
        "run",
        "list",
        "--branch",
        branchName.value,
        "--commit",
        commitSha.value,
        "--limit",
        "100",
        "--json",
        "name,status,conclusion,url"
      ).map { output =>
        val runs = parseWorkflowRuns(output)
        val failed = runs.filter(_.failed)
        val pending = runs.filterNot(_.passed)
        if output.trim.isEmpty || runs.isEmpty then
          BranchChecksPending(
            BranchChecksPendingMessage(
              s"No CI runs found yet for $branchName at $commitSha."
            )
          )
        else if failed.nonEmpty then
          BranchChecksFailed(
            BranchChecksFailedMessage(
              s"CI failed on $branchName at $commitSha: ${formatRuns(failed)}"
            )
          )
        else if pending.nonEmpty then
          BranchChecksPending(
            BranchChecksPendingMessage(
              s"CI pending on $branchName at $commitSha: ${formatRuns(pending)}"
            )
          )
        else
          BranchChecksPassed(
            BranchChecksPassedMessage(
              s"CI passed on $branchName at $commitSha."
            )
          )
      }
    }

  private final case class WorkflowRun(
      name: String,
      status: WorkflowRunStatus,
      conclusion: String,
      url: String
  ):
    def passed: Boolean =
      status.value === "COMPLETED" &&
        Set("SUCCESS", "SKIPPED", "NEUTRAL").contains(conclusion)

    def failed: Boolean =
      status.value === "COMPLETED" &&
        Set(
          "FAILURE",
          "CANCELLED",
          "TIMED_OUT",
          "ACTION_REQUIRED",
          "STARTUP_FAILURE",
          "STALE"
        ).contains(conclusion)

  private def parseWorkflowRuns(output: String): List[WorkflowRun] =
    Try(ujson.read(output).arr.toList).toOption.toList.flatten.flatMap {
      case ujson.Obj(fields) =>
        for
          name <- fields.get("name").collect { case ujson.Str(value) => value }
          status <- fields.get("status").collect { case ujson.Str(value) =>
            value.trim.toUpperCase
          }
        yield WorkflowRun(
          name = name,
          status = WorkflowRunStatus(status),
          conclusion = fields
            .get("conclusion")
            .collect { case ujson.Str(value) => value.trim.toUpperCase }
            .getOrElse(""),
          url = fields
            .get("url")
            .collect { case ujson.Str(value) => value }
            .getOrElse("")
        )
      case _ => None
    }

  private def formatRuns(runs: List[WorkflowRun]): String =
    runs
      .map(run =>
        val state =
          if run.conclusion.nonEmpty then s"${run.status}/${run.conclusion}"
          else run.status
        val urlPart = if run.url.nonEmpty then s" ${run.url}" else ""
        s"${run.name}=$state$urlPart"
      )
      .mkString(", ")

  def closeIssue[F[_]: Sync]: Kleisli[F, (os.Path, TaskNumber), Unit] =
    Kleisli.apply { case (root, taskId) =>
      call(
        root,
        "gh",
        "issue",
        "close",
        taskId.toString,
        "--comment",
        s"Task #$taskId completed successfully. Worktree closed."
      )
    }

  def setIssueStatus[F[_]](progress: String => F[Unit])(using
      Sync[F]
  ): Kleisli[F, (os.Path, TaskNumber, String), Unit] =
    Kleisli.apply { case (root, taskId, status) =>
      val (toAdd, toRemove) =
        if status === "in progress" then
          (
            List("status: in progress", "in progress"),
            List("status: completed", "completed")
          )
        else
          (
            List("status: completed", "completed"),
            List("status: in progress", "in progress")
          )

      val addFlags = toAdd.flatMap(label => Seq("--add-label", label))
      val removeFlags = toRemove.flatMap(label => Seq("--remove-label", label))
      val addCmd = Seq("gh", "issue", "edit", taskId.toString) ++ addFlags
      val removeCmd = Seq("gh", "issue", "edit", taskId.toString) ++ removeFlags

      def run(cmd: Seq[String]): F[Unit] =
        call(root, cmd*).handleErrorWith { error =>
          progress(
            s"Warning: Failed to update GitHub labels for task #$taskId: ${error.getMessage}"
          )
        }

      def ensureLabel(label: String): F[Unit] =
        call(
          root,
          "gh",
          "label",
          "create",
          label,
          "--color",
          "ededed",
          "--force"
        ).handleErrorWith(_ => Sync[F].unit)

      toAdd.traverse_(ensureLabel) *> run(addCmd) *> run(removeCmd)
    }

  private def call[F[_]: Sync](cwd: os.Path, command: String*): F[Unit] =
    TaskLogger.trace[F](s"command cwd=$cwd args=${formatCommand(command)}") *>
      Sync[F]
        .blocking {
          os
            .proc(command)
            .call(cwd = cwd, stdout = os.Pipe, stderr = os.Pipe, check = false)
        }
        .flatMap { result =>
          for
            stdout <- Sync[F].blocking(result.out.text().trim)
            stderr <- Sync[F].blocking(result.err.text().trim)
            _ <-
              if stdout.nonEmpty then TaskLogger.trace[F](s"command stdout ${truncate(stdout)}")
              else Sync[F].unit
            _ <-
              if stderr.nonEmpty then TaskLogger.trace[F](s"command stderr ${truncate(stderr)}")
              else Sync[F].unit
            _ <-
              if result.exitCode === 0 then Sync[F].unit
              else
                Sync[F].raiseError(
                  new RuntimeException(
                    s"Command failed with exit code ${result.exitCode}: ${formatCommand(command)}" +
                      (if stderr.nonEmpty then s"\nstderr: $stderr" else "") +
                      (if stdout.nonEmpty then s"\nstdout: $stdout" else "")
                  )
                )
          yield ()
        }

  private def callOutput[F[_]: Sync](
      cwd: os.Path,
      command: String*
  ): F[String] =
    TaskLogger.trace[F](
      s"command-output cwd=$cwd args=${formatCommand(command)}"
    ) *>
      Sync[F]
        .blocking {
          os
            .proc(command)
            .call(cwd = cwd, stdout = os.Pipe, stderr = os.Pipe, check = false)
        }
        .flatMap { result =>
          for
            stdout <- Sync[F].blocking(result.out.text().trim)
            stderr <- Sync[F].blocking(result.err.text().trim)
            _ <-
              if stderr.nonEmpty then TaskLogger.trace[F](s"command stderr ${truncate(stderr)}")
              else Sync[F].unit
            output <-
              if result.exitCode === 0 then stdout.pure[F]
              else
                Sync[F].raiseError(
                  new RuntimeException(
                    s"Command failed with exit code ${result.exitCode}: ${formatCommand(command)}" +
                      (if stderr.nonEmpty then s"\nstderr: $stderr" else "")
                  )
                )
          yield output
        }

  private def callOutputUnchecked[F[_]: Sync](
      cwd: os.Path,
      command: String*
  ): F[String] =
    TaskLogger.trace[F](
      s"command-output-unchecked cwd=$cwd args=${formatCommand(command)}"
    ) *>
      Sync[F]
        .blocking {
          os
            .proc(command)
            .call(cwd = cwd, stdout = os.Pipe, stderr = os.Pipe, check = false)
        }
        .flatMap { result =>
          for
            stdout <- Sync[F].blocking(result.out.text().trim)
            stderr <- Sync[F].blocking(result.err.text().trim)
            _ <-
              if stderr.nonEmpty then TaskLogger.trace[F](s"command stderr ${truncate(stderr)}")
              else Sync[F].unit
          yield stdout
        }

  private def formatCommand(command: Seq[String]): String =
    redactCommand(command.toList)
      .map(value => s""""${truncate(value)}"""")
      .mkString(" ")

  private def redactCommand(command: List[String]): List[String] =
    command match
      case Nil => Nil
      case ("--body" | "--body-file") :: _ :: tail =>
        command.head :: "<redacted>" :: redactCommand(tail)
      case head :: tail => head :: redactCommand(tail)

  private def truncate(value: String): String =
    if value.length <= 160 then value else value.take(160) + "...[truncated]"
