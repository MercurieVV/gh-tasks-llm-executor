package com.github.mercurievv.ghllm.arrow

import com.github.mercurievv.ghllm.*
import com.github.mercurievv.ghllm.agent.*
import com.github.mercurievv.ghllm.arrow.*
import com.github.mercurievv.ghllm.cli.*
import com.github.mercurievv.ghllm.git.*
import com.github.mercurievv.ghllm.metrics.*

import cats.data.Kleisli
import cats.effect.IO
import cats.effect.Ref
import cats.syntax.all.*
import munit.CatsEffectSuite

import BusinessLogicFixture.TestFlow
import BusinessLogicFixture.issue
import BusinessLogicFixture.runner
import cats.syntax.all.*

class ArrowAttemptSuite extends CatsEffectSuite:
  private val attempt = ArrowAttempt[TestFlow]
  private val boom = RuntimeException("boom")

  test("success keeps the input alongside the output"):
    attempt
      .attempt(Kleisli((n: Int) => IO.pure(n * 2)))
      .run(21)
      .map(result => assertEquals(result, Right((21, 42))))

  test("failure keeps the input alongside the error"):
    attempt
      .attempt(Kleisli((_: Int) => IO.raiseError[Int](boom)))
      .run(7)
      .map(result => assertEquals(result, Left((7, boom))))

class PublishRemoteArrowsSuite extends CatsEffectSuite:

  private val request =
    PublishRequest(os.pwd, os.pwd, BranchName("task-1"), None, issue(1), AgentFinalization(None, None), runner)
  private val remote = RemotePublication(request)
  private val pushRequest = PushRequest(os.pwd, os.pwd, BranchName("task-1"), issue(1), runner)
  private val boom = RuntimeException("rejected")

  private def arrows(
      pushBranch: TestFlow[PushRequest, Unit],
      createAndMergePullRequest: TestFlow[PublishRequest, Unit]
  ) = PublishRemoteArrows[TestFlow](
    toPushRequest = Kleisli(_ => IO.pure(pushRequest)),
    pushBranch = pushBranch,
    toPublishRequest = Kleisli(publication => IO.pure(publication.request)),
    createAndMergePullRequest = createAndMergePullRequest
  )

  // The composition uses `&&&`, which is sequential on this arrow. If that ever
  // became the concurrent variant, the Pull Request could be opened against a
  // branch that has not landed yet - so pin the order.
  test("the branch is pushed before the Pull Request is opened"):
    Ref[IO].of(List.empty[String]).flatMap { events =>
      arrows(pushBranch = Kleisli(_ => events.update(_ :+ "push")), createAndMergePullRequest = Kleisli(_ => events.update(_ :+ "merge"))).publishRemote.run(remote) *> events.get.map(seen => assertEquals(seen, List("push", "merge")))
    }

  test("a repaired push is retried, and the merge still follows it"):
    Ref[IO].of(List.empty[String]).flatMap { events =>
      Ref[IO].of(0).flatMap { pushes =>
        val base = arrows(
          pushBranch = Kleisli { _ =>
            pushes.updateAndGet(_ + 1).flatMap { attempt =>
              events.update(_ :+ s"push$attempt") *>
                IO.raiseError[Unit](boom).whenA(attempt == 1)
            }
          },
          createAndMergePullRequest = Kleisli(_ => events.update(_ :+ "merge"))
        )
        val retryPush = BusinessLogicRetry.retryWithRepair[IO, PushRequest](
          routeFailure = Kleisli { case (request, _) =>
            events.update(_ :+ "repair").as(Right(request))
          },
          raiseFailure = Kleisli(IO.raiseError)
        )(base.pushBranch)

        base.copy(pushBranch = retryPush).publishRemote.run(remote) *> events.get.map(seen => assertEquals(seen, List("push1", "repair", "push2", "merge")))
      }
    }

  test("an unrepairable push failure stops before the Pull Request is opened"):
    Ref[IO].of(false).flatMap { merged =>
      arrows(
        pushBranch = Kleisli(_ => IO.raiseError(boom)),
        createAndMergePullRequest = Kleisli(_ => merged.set(true))
      ).publishRemote
        .run(remote)
        .attempt
        .flatMap { result =>
          merged.get.map { didMerge =>
            assertEquals(result, Left(boom))
            assert(!didMerge)
          }
        }
    }

  test("a resolved merge conflict retries the merge"):
    Ref[IO].of(0).flatMap { merges =>
      val base = arrows(
        pushBranch = Kleisli(_ => IO.unit),
        createAndMergePullRequest = Kleisli { _ =>
          merges.updateAndGet(_ + 1).flatMap(attempt => IO.raiseError[Unit](boom).whenA(attempt == 1))
        }
      )
      val retryMerge = BusinessLogicRetry.retryWithRepair[IO, PublishRequest](
        routeFailure = Kleisli { case (request, _) => IO.pure(Right(request)) },
        raiseFailure = Kleisli(IO.raiseError)
      )(base.createAndMergePullRequest)

      base.copy(createAndMergePullRequest = retryMerge).publishRemote.run(remote) *> merges.get.map(assertEquals(_, 2))
    }

  test("merge conflict repair prompt names unmerged files and requires staging"):
    val prompt = BusinessLogicRetry
      .mergeConflictRepairPrompt(issue(117), "task-35", "task-33", Seq(".gitignore", "build.mill"))
      .value

    assertEquals(prompt.contains("head branch `task-35`"), true)
    assertEquals(prompt.contains("base branch `task-33`"), true)
    assertEquals(prompt.contains("Unmerged files reported by Git:"), true)
    assertEquals(prompt.contains("- .gitignore"), true)
    assertEquals(prompt.contains("- build.mill"), true)
    assertEquals(prompt.contains("git diff --ours -- <file>"), true)
    assertEquals(prompt.contains("git diff --theirs -- <file>"), true)
    assertEquals(prompt.contains("git log --oneline --decorate --graph HEAD...origin/task-33"), true)
    assertEquals(prompt.contains("git add"), true)
    assertEquals(prompt.contains("git diff --name-only --diff-filter=U"), true)
    assertEquals(prompt.contains("Do not run `git commit`"), true)

  test("GitHub mergePullRequest conflict is repairable"):
    val error = RuntimeException(
      """Command failed with exit code 1: "gh" "pr" "merge" "71" "--merge"
        |stderr: GraphQL: Pull Request has merge conflicts (mergePullRequest)""".stripMargin
    )

    assertEquals(BusinessLogicRetry.isMergeConflictError(error), true)

  test("gh's own wording for the same conflict is repairable too"):
    // The CLI does not use GitHub's GraphQL phrasing. Missing this one failed
    // task #56 outright on a conflict the repair path resolves routinely.
    val error = RuntimeException(
      """Command failed with exit code 1: "gh" "pr" "merge" "85" "--merge"
        |stderr: X Pull request MercurieVV/scala-purrism#85 is not mergeable: the merge commit cannot be cleanly created.
        |Run the following to resolve the merge conflicts locally:
        |  gh pr checkout 85 && git fetch origin task-39 && git merge origin/task-39""".stripMargin
    )

    assertEquals(BusinessLogicRetry.isMergeConflictError(error), true)

  test("an unrelated merge failure stays unrepairable"):
    val error = RuntimeException(
      """Command failed with exit code 1: "gh" "pr" "merge" "85" "--merge"
        |stderr: GraphQL: Base branch was modified. Review and try the merge again.""".stripMargin
    )

    assertEquals(BusinessLogicRetry.isMergeConflictError(error), false)
