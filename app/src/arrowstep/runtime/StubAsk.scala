package arrowstep.runtime

import arrowstep.core.{Answers, Ask, AskInput}
import cats.Applicative

final class StubAsk[F[_]] private (answer: AskInput => Answers)(using F: Applicative[F])
    extends Ask[F]:

  def apply(input: AskInput): F[Answers] =
    F.pure(answer(input))

object StubAsk:

  def apply[F[_]: Applicative](canned: Answers): StubAsk[F] =
    fromFunction(_ => canned)

  def fromFunction[F[_]: Applicative](answer: AskInput => Answers): StubAsk[F] =
    new StubAsk[F](answer)
