import cats.data.Kleisli
import cats.effect.IO
import cats.effect.Ref
import cats.syntax.all.*
import munit.CatsEffectSuite

import BusinessLogicFixture.TestFlow
import BusinessLogicFixture.issue
import BusinessLogicFixture.runner

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
  private val pushRequest = PushRequest(os.pwd, BranchName("task-1"), issue(1), runner)
  private val boom = RuntimeException("rejected")

  private def arrows(
      pushBranch: TestFlow[PushRequest, Unit],
      createAndMergePullRequest: TestFlow[PublishRequest, Unit],
      routePushFailure: TestFlow[(PushRequest, Throwable), Either[Throwable, PushRequest]] = Kleisli {
        case (_, error) => IO.pure(Left(error))
      },
      routeMergeFailure: TestFlow[(PublishRequest, Throwable), Either[Throwable, PublishRequest]] = Kleisli {
        case (_, error) => IO.pure(Left(error))
      }
  ) = PublishRemoteArrows[TestFlow](
    toPushRequest = Kleisli(_ => IO.pure(pushRequest)),
    pushBranch = pushBranch,
    routePushFailure = routePushFailure,
    raisePushFailure = Kleisli(IO.raiseError),
    toPublishRequest = Kleisli(publication => IO.pure(publication.request)),
    createAndMergePullRequest = createAndMergePullRequest,
    routeMergeFailure = routeMergeFailure,
    raiseMergeFailure = Kleisli(IO.raiseError)
  )

  // The composition uses `&&&`, which is sequential on this arrow. If that ever
  // became the concurrent variant, the Pull Request could be opened against a
  // branch that has not landed yet - so pin the order.
  test("the branch is pushed before the Pull Request is opened"):
    Ref[IO].of(List.empty[String]).flatMap { events =>
      arrows(
        pushBranch = Kleisli(_ => events.update(_ :+ "push")),
        createAndMergePullRequest = Kleisli(_ => events.update(_ :+ "merge"))
      ).publishRemote
        .run(remote)
        .flatMap(_ => events.get.map(seen => assertEquals(seen, List("push", "merge"))))
    }

  test("a repaired push is retried, and the merge still follows it"):
    Ref[IO].of(List.empty[String]).flatMap { events =>
      Ref[IO].of(0).flatMap { pushes =>
        arrows(
          pushBranch = Kleisli { _ =>
            pushes.updateAndGet(_ + 1).flatMap { attempt =>
              events.update(_ :+ s"push$attempt") *>
                IO.raiseError[Unit](boom).whenA(attempt == 1)
            }
          },
          routePushFailure = Kleisli { case (request, _) =>
            events.update(_ :+ "repair").as(Right(request))
          },
          createAndMergePullRequest = Kleisli(_ => events.update(_ :+ "merge"))
        ).publishRemote
          .run(remote)
          .flatMap(_ => events.get.map(seen => assertEquals(seen, List("push1", "repair", "push2", "merge"))))
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
      arrows(
        pushBranch = Kleisli(_ => IO.unit),
        createAndMergePullRequest = Kleisli { _ =>
          merges.updateAndGet(_ + 1).flatMap(attempt => IO.raiseError[Unit](boom).whenA(attempt == 1))
        },
        routeMergeFailure = Kleisli { case (request, _) => IO.pure(Right(request)) }
      ).publishRemote
        .run(remote)
        .flatMap(_ => merges.get.map(assertEquals(_, 2)))
    }
