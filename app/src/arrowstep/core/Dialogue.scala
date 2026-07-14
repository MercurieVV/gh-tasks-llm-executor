package arrowstep.core

import cats.{Applicative, Monad}
import cats.syntax.all.*

/** The typed agent gap (D7): the one hole where an LLM sits. Interpreters (`ReplayAsk`,
  * `LiveAsk`, `StubAsk` — P1/P2) implement it; `core` only names it.
  */
trait Ask[F[_]]:
  def apply(input: AskInput): F[Answers]

/** The **only** factory of [[ValidAnswers]] (D7). Unvalidated `Answers` physically cannot reach
  * the effects stage because the sole door to `ValidAnswers` is `ValidAnswers.fromValidated`,
  * which is `private[core]` and called nowhere but here.
  */
trait Validator[F[_]]:
  def validate(qs: List[Question], a: Answers): F[Either[List[Problem], ValidAnswers]]

object Validator:

  /** A basic validator: `Choice` answers must be in `allowed`; `FreeText` must be non-empty when
    * `freeTextNonEmpty` is set. A missing answer falls back to the question's `default`, else it is
    * a `Problem`. Problem messages are precise (e.g. `'maybe' not in [yes, no]`).
    */
  def basic[F[_]](using F: Applicative[F]): Validator[F] =
    basic[F](freeTextNonEmpty = true)

  def basic[F[_]](freeTextNonEmpty: Boolean)(using F: Applicative[F]): Validator[F] =
    new Validator[F]:
      def validate(qs: List[Question], a: Answers): F[Either[List[Problem], ValidAnswers]] =
        val results: List[Either[Problem, (String, String)]] = qs.map { q =>
          val resolved = a.get(q.id).orElse(q.default)
          q.kind match
            case QuestionKind.FreeText =>
              resolved match
                case Some(v) if !freeTextNonEmpty || v.trim.nonEmpty => Right(q.id -> v)
                case Some(_) => Left(Problem(q.id, "must not be empty"))
                case None    => Left(Problem(q.id, "no answer provided"))
            case QuestionKind.Choice(allowed) =>
              resolved match
                case Some(v) if allowed.contains(v) => Right(q.id -> v)
                case Some(v) =>
                  Left(Problem(q.id, "'" + v + "' not in [" + allowed.mkString(", ") + "]"))
                case None    => Left(Problem(q.id, "no answer provided"))
        }
        val problems = results.collect { case Left(p) => p }
        val accepted = results.collect { case Right(kv) => kv }.toMap
        F.pure(
          if problems.isEmpty then Right(ValidAnswers.fromValidated(accepted))
          else Left(problems)
        )

/** The re-ask loop that turns a raw `Ask` + `Validator` into a lawful pipeline step. */
object Dialogue:

  /** `ask >>> validate`, looping on rejection: on `Left(problems)` it rebuilds an `AskInput` from
    * the offending questions (with each problem attached to that question's context) and asks
    * again, until `Right(valid)`. The loop is `Monad.tailRecM` — stack-safe, no `var`, no manual
    * recursion (scala-rules §2).
    */
  def askUntilValid[F[_]](ask: Ask[F], validator: Validator[F])(using
      F: Monad[F]
  ): Flow[F, AskInput, ValidAnswers] =
    Flow.apply { (input: AskInput) =>
      F.tailRecM(input) { current =>
        for
          answers <- ask(current)
          verdict <- validator.validate(current.questions, answers)
        yield verdict match
          case Right(valid)   => Right(valid)
          case Left(problems) => Left(reAsk(current, problems))
      }
    }

  /** Build the next `AskInput`: keep only the questions that drew a problem, fold each problem's
    * message into that question's `context`, and summarise all problems at the input level.
    */
  def reAsk(input: AskInput, problems: List[Problem]): AskInput =
    val byId = problems.groupBy(_.questionId)
    val offending = input.questions.flatMap { q =>
      byId.get(q.id).map { ps =>
        val msg = ps.map(_.message).mkString("; ")
        q.copy(context = Some(q.context.fold(msg)(existing => existing + "\n" + msg)))
      }
    }
    val summary = problems.map(p => p.questionId + ": " + p.message).mkString("\n")
    AskInput(offending, Some(summary))
