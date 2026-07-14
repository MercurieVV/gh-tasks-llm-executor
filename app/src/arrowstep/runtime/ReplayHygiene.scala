package arrowstep.runtime

import arrowstep.core.{Answers, Ask, AskInput, Flow}
import cats.effect.{Ref, Sync}
import cats.syntax.all.*

final case class ReplayHygieneResult[+B](result: Either[Throwable, B], retainedAnswers: Answers)

object ReplayHygiene:

  def run[F[_]: Sync, A, B](
      root: os.Path,
      input: A
  )(flow: Ask[F] => Flow[F, A, B]): F[ReplayHygieneResult[B]] =
    for
      inputs <- Ref.of[F, List[AskInput]](Nil)
      ask = recordingAsk(ReplayAsk[F](root), inputs)
      result <- flow(ask).run(input).attempt
      recorded <- inputs.get
      retained <- AnswerLog.pruneStale[F](root, recorded.flatMap(_.questions.map(_.id)).toSet)
    yield ReplayHygieneResult(result, retained)

  private def recordingAsk[F[_]: Sync](delegate: Ask[F], inputs: Ref[F, List[AskInput]]): Ask[F] =
    new Ask[F]:
      def apply(input: AskInput): F[Answers] =
        inputs.update(input :: _) *> delegate(input)
