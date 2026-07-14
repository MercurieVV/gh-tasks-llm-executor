package arrowstep.runtime

import arrowstep.core.Answers
import arrowstep.core.Problem
import arrowstep.core.ProgramSays
import arrowstep.core.Question
import arrowstep.core.QuestionKind

import scala.util.Try

object ProtocolJson:

  def render(programSays: ProgramSays[ujson.Value]): String =
    toJson(programSays).render()

  def parse(raw: String): Either[String, ProgramSays[ujson.Value]] =
    Try(ujson.read(raw)).toEither.left.map(_ => "invalid JSON").flatMap(fromJson)

  def toJson(programSays: ProgramSays[ujson.Value]): ujson.Value =
    programSays match
      case ProgramSays.NeedInput(context, questions) =>
        ujson.Obj.from(
          Seq(
            "status" -> ujson.Str("need-input"),
            "context" -> optionString(context),
            "questions" -> questionsJson(questions)
          )
        )

      case ProgramSays.Rejected(problems, questions) =>
        ujson.Obj.from(
          Seq(
            "status" -> ujson.Str("rejected"),
            "problems" -> ujson.Arr.from(problems.map(problemJson)),
            "questions" -> questionsJson(questions)
          )
        )

      case ProgramSays.Done(result) =>
        ujson.Obj.from(
            Seq(
              "status" -> ujson.Str("done"),
              "result" -> result
            )
          )

  def fromJson(value: ujson.Value): Either[String, ProgramSays[ujson.Value]] =
    value match
      case ujson.Obj(fields) =>
        stringField(fields, "status").flatMap {
          case "need-input" =>
            for
              context <- optionalStringField(fields, "context")
              questions <- questionsFromJson(fields.get("questions"))
            yield ProgramSays.NeedInput(context, questions)

          case "rejected" =>
            for
              problems <- problemsFromJson(fields.get("problems"))
              questions <- questionsFromJson(fields.get("questions"))
            yield ProgramSays.Rejected(problems, questions)

          case "done" =>
            fields.get("result").toRight("missing result").map(ProgramSays.Done(_))

          case other =>
            Left("unknown status: " + other)
        }

      case _ => Left("protocol message must be a JSON object")

  def renderAnswers(answers: Answers): String =
    answersJson(answers).render()

  def parseAnswers(raw: String): Option[Answers] =
    Try(ujson.read(raw)).toOption.flatMap(answersFromJson)

  private def answersJson(answers: Answers): ujson.Value =
    ujson.Obj.from(answers.toMap.toList.sortBy(_._1).map { case (id, answer) =>
      id -> ujson.Str(answer)
    })

  private def answersFromJson(value: ujson.Value): Option[Answers] =
    value match
      case ujson.Obj(values) =>
        values.toList
          .foldRight(Option(List.empty[(String, String)])) {
            case ((id, ujson.Str(answer)), Some(entries)) => Some((id -> answer) :: entries)
            case (_, _)                                   => None
          }
          .map(entries => Answers(entries.toMap))
      case _ => None

  private def questionsJson(questions: List[Question]): ujson.Value =
    ujson.Arr.from(questions.map(questionJson))

  private def questionJson(question: Question): ujson.Value =
    ujson.Obj.from(questionFields(question))

  private def questionFields(question: Question): Seq[(String, ujson.Value)] =
    Seq(
      "id" -> ujson.Str(question.id),
      "text" -> ujson.Str(question.text)
    ) ++ kindFields(question.kind) ++ Seq(
      "default" -> optionString(question.default),
      "current" -> optionString(question.current),
      "context" -> optionString(question.context)
    )

  private def kindFields(kind: QuestionKind): Seq[(String, ujson.Value)] =
    kind match
      case QuestionKind.FreeText =>
        Seq("kind" -> ujson.Str("free-text"))
      case QuestionKind.Choice(allowed) =>
        Seq(
          "kind" -> ujson.Str("choice"),
          "allowed" -> ujson.Arr.from(allowed.map(ujson.Str(_)))
        )

  private def problemJson(problem: Problem): ujson.Value =
    ujson.Obj.from(
      Seq(
        "questionId" -> ujson.Str(problem.questionId),
        "message" -> ujson.Str(problem.message)
      )
    )

  private def optionString(value: Option[String]): ujson.Value =
    value.fold[ujson.Value](ujson.Null)(ujson.Str(_))

  private def questionsFromJson(value: Option[ujson.Value]): Either[String, List[Question]] =
    value match
      case Some(ujson.Arr(items)) =>
        sequence(items.toList.map(questionFromJson))
      case Some(_) => Left("questions must be an array")
      case None    => Left("missing questions")

  private def questionFromJson(value: ujson.Value): Either[String, Question] =
    value match
      case ujson.Obj(fields) =>
        for
          id <- stringField(fields, "id")
          text <- stringField(fields, "text")
          kind <- questionKindFromJson(fields)
          default <- optionalStringField(fields, "default")
          current <- optionalStringField(fields, "current")
          context <- optionalStringField(fields, "context")
        yield Question(id, text, kind, default, current, context)
      case _ => Left("question must be a JSON object")

  private def questionKindFromJson(fields: collection.Map[String, ujson.Value]): Either[String, QuestionKind] =
    stringField(fields, "kind").flatMap {
      case "free-text" => Right(QuestionKind.FreeText)
      case "choice" =>
        fields.get("allowed") match
          case Some(ujson.Arr(items)) =>
            sequence(items.toList.map {
              case ujson.Str(value) => Right(value)
              case _                => Left("allowed values must be strings")
            }).map(QuestionKind.Choice(_))
          case Some(_) => Left("allowed must be an array")
          case None    => Left("choice question missing allowed values")
      case other => Left("unknown question kind: " + other)
    }

  private def problemsFromJson(value: Option[ujson.Value]): Either[String, List[Problem]] =
    value match
      case Some(ujson.Arr(items)) =>
        sequence(items.toList.map(problemFromJson))
      case Some(_) => Left("problems must be an array")
      case None    => Left("missing problems")

  private def problemFromJson(value: ujson.Value): Either[String, Problem] =
    value match
      case ujson.Obj(fields) =>
        for
          questionId <- stringField(fields, "questionId")
          message <- stringField(fields, "message")
        yield Problem(questionId, message)
      case _ => Left("problem must be a JSON object")

  private def stringField(fields: collection.Map[String, ujson.Value], name: String): Either[String, String] =
    fields.get(name) match
      case Some(ujson.Str(value)) => Right(value)
      case Some(_)                => Left(name + " must be a string")
      case None                   => Left("missing " + name)

  private def optionalStringField(
      fields: collection.Map[String, ujson.Value],
      name: String
  ): Either[String, Option[String]] =
    fields.get(name) match
      case Some(ujson.Str(value)) => Right(Some(value))
      case Some(ujson.Null)       => Right(None)
      case Some(_)                => Left(name + " must be a string or null")
      case None                   => Right(None)

  private def sequence[A](items: List[Either[String, A]]): Either[String, List[A]] =
    items.foldRight(Right(List.empty[A]): Either[String, List[A]]) { (item, acc) =>
      for
        value <- item
        values <- acc
      yield value :: values
    }
