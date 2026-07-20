import cats.data.Kleisli
import cats.effect.IO
import cats.effect.Ref
import io.github.mercurievv.minuscles.fieldsnames.derivation.semiauto.FieldNamesDerivation.fieldsNames
import munit.CatsEffectSuite

import ArrowLogging.*

final case class LoggingInner[-->[_, _]](
    increment: Int --> Int
)

object LoggingInner:
  given Functor2K[LoggingInner] = Functor2K.derived
  given Semigroup2K[LoggingInner] = Semigroup2K.derived

final case class LoggingOuter[-->[_, _]](
    inner: LoggingInner[-->],
    stringify: Int --> String
)

object LoggingOuter:
  given Functor2K[LoggingOuter] = Functor2K.derived
  given Semigroup2K[LoggingOuter] = Semigroup2K.derived

class ArrowLoggingSuite extends CatsEffectSuite:
  type TestFlow[A, B] = Kleisli[IO, A, B]

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
