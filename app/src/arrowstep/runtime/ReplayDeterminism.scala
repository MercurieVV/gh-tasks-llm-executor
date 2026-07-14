package arrowstep.runtime

import arrowstep.core.{Answers, Ask, AskInput, Flow, Question}
import cats.effect.{Ref, Sync}
import cats.syntax.all.*

final case class ReplayTrace(inputs: List[AskInput]):
  def questionSequences: List[List[Question]] =
    inputs.map(_.questions)

final case class ReplayDeterminismCheck(first: ReplayTrace, second: ReplayTrace):
  def deterministic: Boolean =
    first.questionSequences == second.questionSequences

  def differingIndex: Option[Int] =
    ReplayDeterminism.firstDifference(first.questionSequences, second.questionSequences)

object ReplayDeterminism:

  def check[F[_]: Sync, A, B](
      root: os.Path,
      answers: Answers,
      input: A
  )(flow: Ask[F] => Flow[F, A, B]): F[ReplayDeterminismCheck] =
    trace(root, answers, input)(flow).flatMap { first =>
      trace(root, answers, input)(flow).map(second => ReplayDeterminismCheck(first, second))
    }

  def trace[F[_]: Sync, A, B](
      root: os.Path,
      answers: Answers,
      input: A
  )(flow: Ask[F] => Flow[F, A, B]): F[ReplayTrace] =
    for
      _ <- AnswerLog.write[F](root, answers)
      inputs <- Ref.of[F, List[AskInput]](Nil)
      ask = recordingAsk(ReplayAsk[F](root), inputs)
      _ <- flow(ask).run(input).attempt
      recorded <- inputs.get
    yield ReplayTrace(recorded.reverse)

  private[runtime] def firstDifference[A](left: List[A], right: List[A]): Option[Int] =
    left.zip(right).zipWithIndex.find { case ((l, r), _) => l != r }.map { case (_, index) => index }
      .orElse {
        Option.when(left.size != right.size)(left.size.min(right.size))
      }

  private def recordingAsk[F[_]: Sync](delegate: Ask[F], inputs: Ref[F, List[AskInput]]): Ask[F] =
    new Ask[F]:
      def apply(input: AskInput): F[Answers] =
        inputs.update(input :: _) *> delegate(input)
