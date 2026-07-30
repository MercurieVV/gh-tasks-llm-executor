package com.github.mercurievv.ghllm.arrow

import com.github.mercurievv.ghllm.*

import cats.data.Kleisli
import cats.effect.IO
import cats.effect.Ref
import munit.CatsEffectSuite

import BusinessLogicFixture.*

class FanOutCacheTest extends CatsEffectSuite:

  private val CacheTtlEnvironmentVariable = "ENABLE_PROMPT_CACHING_1H"

  private def selection(numbers: List[Int]) =
    TaskSelection(
      context.copy(parallelExecution = ParallelExecution(true)),
      numbers.map(candidate(_, parallel = true))
    )

  private def observedInvocationEnvironments(
      numbers: List[Int]
  ): IO[List[Map[String, String]]] =
    Ref[IO].of(List.empty[Map[String, String]]).flatMap { observed =>
      val runOnce = Kleisli { (candidate: TaskCandidate) =>
        observed
          .update(_ :+ candidate.runner.invocationEnvironment)
          .as(summary(s"ran ${candidate.issue.number.value}"))
      }
      val fanOutLogic =
        logic.copy(
          programArrows = logic.programArrows.copy(
            loadOpenIssues = Kleisli(IO.pure),
            routeParallelExecution = Kleisli(selection => IO.pure(Left(selection))),
            lastSummary = Kleisli(summaries => IO.pure(summaries.last))
          ),
          traversalArrows = logic.traversalArrows.copy(
            routeRecursiveMode = Kleisli(candidate => IO.pure(Right(candidate))),
            runOnce = runOnce
          )
        )

      fanOutLogic.executeSelectedCandidates
        .run(selection(numbers))
        .flatMap(_ => observed.get)
    }

  test("three siblings receive the 1-hour cache TTL flag"):
    observedInvocationEnvironments(List(1, 2, 3)).map { environments =>
      assertEquals(environments.size, 3)
      assert(environments.forall(_.get(CacheTtlEnvironmentVariable).contains("1")))
    }

  test("a single-child edge does not receive the 1-hour cache TTL flag"):
    observedInvocationEnvironments(List(1)).map { environments =>
      assertEquals(environments, List(Map.empty[String, String]))
    }
