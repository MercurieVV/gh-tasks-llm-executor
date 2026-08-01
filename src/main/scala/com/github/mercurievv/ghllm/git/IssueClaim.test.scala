package com.github.mercurievv.ghllm.git

import cats.effect.IO
import cats.effect.Ref
import munit.CatsEffectSuite
import cats.syntax.all.*

class TransientNetworkSuite extends munit.FunSuite:

  test("a resolver failure is transient"):
    assert(IssueClaim.isTransientNetwork("fatal: unable to access '...': Could not resolve host: github.com"))

  test("a dial timeout is transient"):
    assert(IssueClaim.isTransientNetwork("dial tcp 140.82.121.4:443: i/o timeout"))

  test("a rejected push is not transient — the server had an opinion"):
    assert(!IssueClaim.isTransientNetwork("! [rejected] task-1 -> task-1 (non-fast-forward)"))

  test("an existing claim ref is not transient"):
    assert(!IssueClaim.isTransientNetwork("error: failed to push some refs: ref already exists"))

class RetryOnTransientSuite extends CatsEffectSuite:
  private def result(exitCode: Int, stderr: String) =
    os.CommandResult(Seq("git"), exitCode, Seq(Right(geny.Bytes(stderr.getBytes))))

  private def attemptsOf(results: List[os.CommandResult]) =
    Ref[IO].of(results).map { remaining =>
      remaining -> remaining.modify {
        case head :: tail => tail -> head
        case Nil          => Nil -> result(0, "")
      }
    }

  test("a transient failure is retried until it succeeds"):
    // The backoff sleeps are real, so this uses a single retry: what matters is
    // that a second attempt happens at all.
    attemptsOf(List(result(128, "Could not resolve host: github.com"), result(0, ""))).flatMap {
      case (remaining, attempt) =>
        (IssueClaim.retryOnTransient[IO](_ => IO.unit)("claiming", attempt), remaining.get).mapN((res, left) => (res.exitCode, left.size))
          .map(assertEquals(_, (0, 0)))
    }

  test("a non-transient failure is returned on the first attempt, unretried"):
    attemptsOf(List(result(1, "! [rejected] non-fast-forward"), result(0, ""))).flatMap { case (remaining, attempt) =>
      (IssueClaim.retryOnTransient[IO](_ => IO.unit)("claiming", attempt), remaining.get).mapN((res, left) => (res.exitCode, left.size))
        .map(assertEquals(_, (1, 1)))
    }
