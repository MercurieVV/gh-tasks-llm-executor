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
import io.github.mercurievv.minuscles.fieldsnames.derivation.semiauto.FieldNamesDerivation.fieldsNames
import munit.CatsEffectSuite

import ArrowLogging.*

final case class LoggingPair[A, B](
    first: A,
    second: B
)

object LoggingPair:
  given Functor2[LoggingPair] with
    def map[A, B, C, D](value: LoggingPair[A, B])(
        first: A => C,
        second: B => D
    ): LoggingPair[C, D] =
      LoggingPair(first(value.first), second(value.second))

final case class LoggingInner[-->[_, _]](
    increment: Int --> Int
)

object LoggingInner:
  given Functor2K[LoggingInner] = Functor2K.derived
  given Monoid2K[LoggingInner] = Monoid2K.derived

final case class LoggingOuter[-->[_, _]](
    inner: LoggingInner[-->],
    stringify: Int --> String
)

object LoggingOuter:
  given Functor2K[LoggingOuter] = Functor2K.derived
  given Monoid2K[LoggingOuter] = Monoid2K.derived

class ArrowLoggingSuite extends CatsEffectSuite:
  type TestFlow[A, B] = Kleisli[IO, A, B]

  test("Functor2 maps both type parameters"):
    assertEquals(
      Functor2[LoggingPair].map(LoggingPair(1, "ok"))(_ + 1, _.length),
      LoggingPair(2, 2)
    )

  test("Function2 transforms binary type constructors"):
    val toTuple: Function2[LoggingPair, Tuple2] =
      [A, B] => (value: LoggingPair[A, B]) => (value.first, value.second)

    assertEquals(toTuple[Int, String](LoggingPair(1, "ok")), (1, "ok"))

  test("Function2K transforms structures over binary type constructors"):
    val identityOuter: Function2K[LoggingOuter, LoggingOuter] =
      [F[_, _]] => (value: LoggingOuter[F]) => value
    val value = LoggingOuter[LoggingPair](
      inner = LoggingInner(
        increment = LoggingPair(1, 2)
      ),
      stringify = LoggingPair(3, "ok")
    )

    assertEquals(identityOuter[LoggingPair](value), value)

  test("Monoid2K derives empty nested classes"):
    val empty = Monoid2K[LoggingOuter].emptyK[ArrowLogging[TestFlow]]
    val named = LoggingOuter[ArrowLogging[TestFlow]](
      inner = LoggingInner(
        increment = ArrowLog(Some("increment"), None)
      ),
      stringify = ArrowLog(Some("stringify"), None)
    )

    assertEquals(empty.inner.increment, ArrowLog(None, None))
    assertEquals(empty.stringify, ArrowLog(None, None))
    assertEquals(Semigroup2K[LoggingOuter].combineK(empty, named), named)
    assertEquals(Semigroup2K[LoggingOuter].combineK(named, empty), named)

  test("withArrowLogging logs and executes arrows in derived nested classes"):
    for
      entries <- Ref.of[IO, Vector[String]](Vector.empty)
      logger = new ArrowLogger[TestFlow]:
        def apply[A, B](name: String, arrow: TestFlow[A, B]): TestFlow[A, B] =
          Kleisli { input =>
            arrow.run(input).flatTap { output =>
              entries.update(_ :+ s"$name=$output")
            }
          }
      arrows = LoggingOuter[TestFlow](
        inner = LoggingInner(
          increment = Kleisli((value: Int) => IO.pure(value + 1))
        ),
        stringify = Kleisli((value: Int) => IO.pure(s"value:$value"))
      )
      logged = arrows.withArrowLogging(logger)
      incremented <- logged.inner.increment.run(41)
      stringified <- logged.stringify.run(7)
      logs <- entries.get
    yield
      assertEquals(incremented, 42)
      assertEquals(stringified, "value:7")
      assertEquals(
        logs,
        Vector(
          "innerincrement=42",
          "stringify=value:7"
        )
      )
