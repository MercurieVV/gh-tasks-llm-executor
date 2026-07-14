package arrowstep.runtime

import arrowstep.core.{Answers, Ask, AskInput, Flow}
import cats.effect.Concurrent
import cats.effect.implicits.*
import cats.syntax.all.*

object ParallelAsk:

  def both[F[_]: Concurrent](left: Ask[F], right: Ask[F]): Flow[F, (AskInput, AskInput), (Answers, Answers)] =
    Flow.apply { case (leftInput, rightInput) =>
      (left(leftInput), right(rightInput)).parTupled
    }

  def many[F[_]: Concurrent](asks: List[Ask[F]]): Flow[F, List[AskInput], List[Answers]] =
    Flow.apply { inputs =>
      if asks.size === inputs.size then asks.zip(inputs).parTraverse { case (ask, input) => ask(input) }
      else Concurrent[F].raiseError(new IllegalArgumentException("asks and inputs must have the same size"))
    }
