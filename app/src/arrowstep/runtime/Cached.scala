package arrowstep.runtime

import arrowstep.core.Flow
import cats.effect.Sync
import cats.syntax.all.*

final case class CacheKey(value: String)

object Cached:

  private val Prefix = "_cache."

  def string[F[_]: Sync](key: CacheKey)(compute: F[String]): F[String] =
    Sync[F].delay(os.pwd).flatMap(root => string(root, key)(compute))

  def string[F[_]: Sync](root: os.Path, key: CacheKey)(compute: F[String]): F[String] =
    val id = answerId(key)
    AnswerLog.read[F](root).flatMap { answers =>
      answers.get(id).fold {
        compute.flatMap { value =>
          AnswerLog.read[F](root).flatMap { latest =>
            AnswerLog.write[F](root, AnswerLog.upsert(latest, id, value)).as(value)
          }
        }
      }(Sync[F].pure)
    }

  def flow[F[_]: Sync, A](key: CacheKey)(compute: A => F[String]): F[Flow[F, A, String]] =
    Sync[F].delay(os.pwd).map(root => flow(root, key)(compute))

  def flow[F[_]: Sync, A](root: os.Path, key: CacheKey)(compute: A => F[String]): Flow[F, A, String] =
    Flow.apply(a => string(root, key)(compute(a)))

  private[runtime] def answerId(key: CacheKey): String =
    Prefix + key.value
