package arrowstep.runtime

import arrowstep.core.Answers
import cats.effect.Sync
import cats.syntax.all.*

object AnswerLog:

  private val AgentsDir = ".agents"
  private val AnswersFile = "answers.json"
  private val CachePrefix = "_cache."

  def read[F[_]: Sync]: F[Answers] =
    Sync[F].delay(os.pwd).flatMap(read[F])

  def read[F[_]: Sync](root: os.Path): F[Answers] =
    Sync[F].delay {
      val file = path(root)
      if os.exists(file) then parse(os.read(file)).getOrElse(Answers(Map.empty))
      else Answers(Map.empty)
    }

  def write[F[_]: Sync](a: Answers): F[Unit] =
    Sync[F].delay(os.pwd).flatMap(write[F](_, a))

  def write[F[_]: Sync](root: os.Path, a: Answers): F[Unit] =
    Sync[F].delay {
      val dir = root / AgentsDir
      os.makeDir.all(dir)
      os.write.over(path(root), render(a), createFolders = true)
    }

  def reset[F[_]: Sync](root: os.Path): F[Unit] =
    write(root, Answers(Map.empty))

  def pruneStale[F[_]: Sync](root: os.Path, activeQuestionIds: Set[String]): F[Answers] =
    read[F](root).flatMap { answers =>
      val pruned = retainActive(answers, activeQuestionIds)
      write(root, pruned).as(pruned)
    }

  def retainActive(answers: Answers, activeQuestionIds: Set[String]): Answers =
    Answers(answers.toMap.filter { case (id, _) => activeQuestionIds.contains(id) || isCacheId(id) })

  def merge(left: Answers, right: Answers): Answers =
    Answers(left.toMap ++ right.toMap)
  def upsert(answers: Answers, id: String, answer: String): Answers =
    Answers(answers.toMap.updated(id, answer))
  private def isCacheId(id: String): Boolean =
    id.startsWith(CachePrefix)
  private def path(root: os.Path): os.Path =
    root / AgentsDir / AnswersFile
  def parse(raw: String): Option[Answers] =
    ProtocolJson.parseAnswers(raw)
  def render(a: Answers): String =
    ProtocolJson.renderAnswers(a)
