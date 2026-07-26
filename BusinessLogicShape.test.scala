import cats.data.Kleisli
import cats.effect.IO
import cats.effect.Ref
import cats.effect.std.CountDownLatch
import cats.syntax.all.*
import munit.CatsEffectSuite

import scala.concurrent.duration.*

import BusinessLogicFixture.*

/** Smoke tests for the two `BusinessLogic` compositions that changed behaviour representation: the `--parallel`
  * decision (was an `if` inside an effect) and the vanished-Pull-Request fallback (was a hand-copied duplicate of the
  * ordinary run pipeline).
  */
class BusinessLogicShapeSuite extends CatsEffectSuite:

  private def selection(parallel: Boolean, numbers: List[Int]) =
    TaskSelection(
      context.copy(parallelExecution = ParallelExecution(parallel)),
      numbers.map(candidate(_, parallel = parallel))
    )

  private def withRunOnce(runOnce: TestFlow[TaskCandidate, RunSummary]) =
    logic.copy(
      programArrows = logic.programArrows.copy(
        loadOpenIssues = Kleisli(IO.pure),
        routeParallelExecution = Kleisli { sel =>
          IO.pure(Either.cond(!sel.context.parallelExecution.value, sel, sel))
        },
        lastSummary = Kleisli(summaries => IO.pure(summaries.last))
      ),
      traversalArrows = logic.traversalArrows.copy(
        routeRecursiveMode = Kleisli(candidate => IO.pure(Right(candidate))),
        runOnce = runOnce
      )
    )

  test("sequential mode finishes each candidate before starting the next"):
    Ref[IO].of(List.empty[String]).flatMap { events =>
      val runOnce = Kleisli { (candidate: TaskCandidate) =>
        val number = candidate.issue.number.value
        events.update(_ :+ s"start$number") *>
          events.update(_ :+ s"end$number").as(summary(s"ran $number"))
      }
      withRunOnce(runOnce).executeSelectedCandidates
        .run(selection(parallel = false, List(1, 2, 3)))
        .flatMap { result =>
          events.get.map { seen =>
            assertEquals(result, summary("ran 3"))
            assertEquals(seen, List("start1", "end1", "start2", "end2", "start3", "end3"))
          }
        }
    }

  test("--parallel overlaps candidates up to MaxParallelism"):
    // Every candidate blocks until MaxParallelism of them have started. A sequential traversal
    // never reaches that count and times out; a genuinely concurrent one (bounded to exactly
    // MaxParallelism at a time) gets past it for each successive batch.
    CountDownLatch[IO](ParallelArrows.MaxParallelism).flatMap { latch =>
      val runOnce = Kleisli { (candidate: TaskCandidate) =>
        latch.release *> latch.await.as(summary(s"ran ${candidate.issue.number.value}"))
      }
      withRunOnce(runOnce).executeSelectedCandidates
        .run(selection(parallel = true, List.range(1, ParallelArrows.MaxParallelism + 1)))
        .timeout(10.seconds)
        .map(result => assertEquals(result, summary(s"ran ${ParallelArrows.MaxParallelism}")))
    }

  test("an escaped push failure is reported for that candidate and does not break the batch"):
    Ref[IO].of(List.empty[String]).flatMap { events =>
      val pushFailure = RuntimeException("""Command failed with exit code 1: "git" "push" "-u" "origin" "task-1"""")
      val runOnce = Kleisli { (candidate: TaskCandidate) =>
        val number = candidate.issue.number.value
        events.update(_ :+ s"start$number") *>
          IO.raiseError[RunSummary](pushFailure).whenA(number == 1) *>
          events.update(_ :+ s"end$number").as(summary(s"ran $number"))
      }
      withRunOnce(runOnce)
        .copy(
          programArrows = withRunOnce(runOnce).programArrows.copy(
            recoverCandidateFailure = Impl.recoverCandidateFailure[IO]
          )
        )
        .executeSelectedCandidates
        .run(selection(parallel = false, List(1, 2)))
        .attempt
        .flatMap { result =>
          events.get.map { seen =>
            assertEquals(result, Right(summary("ran 2")))
            assertEquals(seen, List("start1", "start2", "end2"))
          }
        }
    }

  test("a vanished Pull Request falls back to the real run pipeline"):
    val gone = GitHub.NoOpenPullRequestToResumeException(BranchName("task-1"))
    Ref[IO].of(List.empty[String]).flatMap { steps =>
      logic
        .copy(
          taskArrows = logic.taskArrows.copy(
            resumeTask = logic.taskArrows.resumeTask.copy(
              resume = logic.taskArrows.resumeTask.resume.copy(
                startResume = Kleisli(run => IO.pure(PullRequestResume(run, 2))),
                resumePullRequest = Kleisli(_ => IO.raiseError(gone)),
                // The repair loop declines it first: not repairable, so it
                // leaves the loop and only then becomes a resume-error branch.
                routeResumeFailure = Kleisli { case (_, error) => IO.pure(Left(error)) },
                raiseResumeFailure = Kleisli(IO.raiseError)
              ),
              announceResume = Kleisli(IO.pure),
              routeResumeError = Kleisli {
                case (run, _: GitHub.NoOpenPullRequestToResumeException) => IO.pure(Left(run))
                case (run, error)                                        => IO.pure(Right((run, error)))
              },
              announceNoPullRequest = Kleisli(run => steps.update(_ :+ "announceNoPr").as(run))
            ),
            announceTask = Kleisli(run => steps.update(_ :+ "announceTask").as(run)),
            fetchTaskContext = Kleisli(run => steps.update(_ :+ "fetch").as(PreparedTask(run, None, None))),
            evaluateTask = Kleisli(task => steps.update(_ :+ "evaluate").as(Right(Right(task)))),
            markTaskInProgress = Kleisli(task => steps.update(_ :+ "markInProgress").as(task)),
            acquireWorktreeAndExecute = Kleisli(task => steps.update(_ :+ "execute").as(task.claimedTask)),
            completedTaskSummary = Kleisli(_ => IO.pure(summary("ran normally")))
          )
        )
        .resumeExistingPullRequest
        .run(claimed(1))
        .flatMap { result =>
          steps.get.map { seen =>
            assertEquals(result, summary("ran normally"))
            assertEquals(
              seen,
              List("announceNoPr", "announceTask", "fetch", "evaluate", "markInProgress", "execute")
            )
          }
        }
    }
