package com.github.mercurievv.ghllm.arrow

import com.github.mercurievv.ghllm.*
import com.github.mercurievv.ghllm.agent.*
import com.github.mercurievv.ghllm.arrow.*
import com.github.mercurievv.ghllm.cli.*
import com.github.mercurievv.ghllm.git.*
import com.github.mercurievv.ghllm.metrics.*

import io.github.mercurievv.minuscles.fieldsnames.FieldsNames
import shapeless3.deriving.*
import shapeless3.deriving.internals.Kinds

type ArrowLogging[-->[_, _]] = [A, B] =>> ArrowLog[-->, A, B]
type Function2[F[_, _], G[_, _]] = [A, B] => F[A, B] => G[A, B]
type Function2K[T[_[_, _]], U[_[_, _]]] = [F[_, _]] => T[F] => U[F]

object K12
    extends Kind[
      [_[_, _]] =>> Any,
      [_[_, _]] =>> Tuple,
      [t[_[_, _]]] =>> t[[_, _] =>> Any],
      [t[_[_, _]]] =>> [a[_, _]] =>> Kinds.Head[t[a]],
      [t[_[_, _]]] =>> [a[_, _]] =>> Kinds.Tail[t[a]]
    ]:

  type Id[A, B] = [f[_, _]] =>> f[A, B]

  extension [T[_[_, _]], A[_, _]](gen: ProductGeneric[T])
    inline def toRepr(o: T[A]): gen.MirroredElemTypes[A] =
      Tuple.fromProduct(o.asInstanceOf).asInstanceOf[gen.MirroredElemTypes[A]]

    inline def fromRepr(r: gen.MirroredElemTypes[A]): T[A] =
      gen.fromProduct(r.asInstanceOf).asInstanceOf[T[A]]

  extension [F[_[_[_, _]]], T[_[_, _]]](inst: ProductInstances[F, T])
    inline def construct[R[_, _]](
        f: [t[_[_, _]]] => F[t] => t[R]
    ): T[R] =
      inst.erasedConstruct(f.asInstanceOf).asInstanceOf

    inline def map[A[_, _], R[_, _]](x: T[A])(
        f: [t[_[_, _]]] => (F[t], t[A]) => t[R]
    ): T[R] =
      inst.erasedMap(x)(f.asInstanceOf).asInstanceOf

    inline def map2[A[_, _], B[_, _], R[_, _]](x: T[A], y: T[B])(
        f: [t[_[_, _]]] => (F[t], t[A], t[B]) => t[R]
    ): T[R] =
      inst.erasedMap2(x, y)(f.asInstanceOf).asInstanceOf

trait Functor2[F[_, _]]:
  def map[A, B, C, D](value: F[A, B])(
      first: A => C,
      second: B => D
  ): F[C, D]

object Functor2:
  def apply[F[_, _]](using functor: Functor2[F]): Functor2[F] =
    functor

trait Functor2K[T[_[_, _]]]:
  def mapK[F[_, _], G[_, _]](value: T[F])(f: Function2[F, G]): T[G]

object Functor2K:
  def apply[T[_[_, _]]](using functor: Functor2K[T]): Functor2K[T] =
    functor

  given [A, B]: Functor2K[K12.Id[A, B]] with
    def mapK[F[_, _], G[_, _]](value: F[A, B])(f: Function2[F, G]): G[A, B] =
      f[A, B](value)

  given product[T[_[_, _]]](using
      inst: K12.ProductInstances[Functor2K, T]
  ): Functor2K[T] with
    def mapK[F[_, _], G[_, _]](value: T[F])(f: Function2[F, G]): T[G] =
      inst.map(value)(
        [field[_[_, _]]] => (fieldFunctor: Functor2K[field], fieldValue: field[F]) => fieldFunctor.mapK(fieldValue)(f)
      )

  inline def derived[T[_[_, _]]](using
      K12.ProductGeneric[T]
  ): Functor2K[T] =
    product

trait Apply2K[T[_[_, _]]]:
  def apK[F[_, _]](functions: T[[A, B] =>> F[A, B] => F[A, B]])(value: T[F]): T[F]

object Apply2K:
  def apply[T[_[_, _]]](using applyK: Apply2K[T]): Apply2K[T] =
    applyK

  given [A, B]: Apply2K[K12.Id[A, B]] with
    def apK[F[_, _]](
        functions: F[A, B] => F[A, B]
    )(value: F[A, B]): F[A, B] =
      functions(value)

  given product[T[_[_, _]]](using
      inst: K12.ProductInstances[Apply2K, T]
  ): Apply2K[T] with
    def apK[F[_, _]](
        functions: T[[A, B] =>> F[A, B] => F[A, B]]
    )(value: T[F]): T[F] =
      inst.map2(functions, value)(
        [field[_[_, _]]] =>
          (
              fieldApply: Apply2K[field],
              fieldFunctions: field[[A, B] =>> F[A, B] => F[A, B]],
              fieldValue: field[F]
          ) => fieldApply.apK(fieldFunctions)(fieldValue)
      )

  inline def derived[T[_[_, _]]](using
      K12.ProductGeneric[T]
  ): Apply2K[T] =
    product

trait Semigroup2[F[_, _]]:
  def combine[A, B](x: F[A, B], y: F[A, B]): F[A, B]

object Semigroup2:
  def apply[F[_, _]](using semigroup: Semigroup2[F]): Semigroup2[F] =
    semigroup

trait Monoid2[F[_, _]] extends Semigroup2[F]:
  def empty[A, B]: F[A, B]

object Monoid2:
  def apply[F[_, _]](using monoid: Monoid2[F]): Monoid2[F] =
    monoid

trait Semigroup2K[T[_[_, _]]]:
  def combineK[F[_, _]: Semigroup2](x: T[F], y: T[F]): T[F]

object Semigroup2K:
  def apply[T[_[_, _]]](using semigroup: Semigroup2K[T]): Semigroup2K[T] =
    semigroup

  given [A, B]: Semigroup2K[K12.Id[A, B]] with
    def combineK[F[_, _]: Semigroup2](
        x: F[A, B],
        y: F[A, B]
    ): F[A, B] =
      Semigroup2[F].combine(x, y)

  given product[T[_[_, _]]](using
      inst: K12.ProductInstances[Semigroup2K, T]
  ): Semigroup2K[T] with
    def combineK[F[_, _]: Semigroup2](x: T[F], y: T[F]): T[F] =
      inst.map2(x, y)(
        [field[_[_, _]]] =>
          (
              fieldSemigroup: Semigroup2K[field],
              left: field[F],
              right: field[
                F
              ]
          ) => fieldSemigroup.combineK(left, right)
      )

  inline def derived[T[_[_, _]]](using
      K12.ProductGeneric[T]
  ): Semigroup2K[T] =
    product

trait Monoid2K[T[_[_, _]]] extends Semigroup2K[T]:
  def emptyK[F[_, _]: Monoid2]: T[F]

object Monoid2K:
  def apply[T[_[_, _]]](using monoid: Monoid2K[T]): Monoid2K[T] =
    monoid

  given [A, B]: Monoid2K[K12.Id[A, B]] with
    def emptyK[F[_, _]: Monoid2]: F[A, B] =
      Monoid2[F].empty[A, B]

    def combineK[F[_, _]: Semigroup2](
        x: F[A, B],
        y: F[A, B]
    ): F[A, B] =
      Semigroup2[F].combine(x, y)

  given product[T[_[_, _]]](using
      inst: K12.ProductInstances[Monoid2K, T]
  ): Monoid2K[T] with
    def emptyK[F[_, _]: Monoid2]: T[F] =
      inst.construct(
        [field[_[_, _]]] => (fieldMonoid: Monoid2K[field]) => fieldMonoid.emptyK[F]
      )

    def combineK[F[_, _]: Semigroup2](x: T[F], y: T[F]): T[F] =
      inst.map2(x, y)(
        [field[_[_, _]]] =>
          (
              fieldMonoid: Monoid2K[field],
              left: field[F],
              right: field[
                F
              ]
          ) => fieldMonoid.combineK(left, right)
      )

  inline def derived[T[_[_, _]]](using
      K12.ProductGeneric[T]
  ): Monoid2K[T] =
    product

final case class ArrowLog[-->[_, _], A, B](
    name: Option[String],
    arrow: Option[A --> B]
)

object ArrowLog:
  given [-->[_, _], A, B]: FieldsNames[ArrowLog[-->, A, B]] with
    def withFieldsNames(fieldInfo: String): ArrowLog[-->, A, B] =
      ArrowLog(Some(fieldInfo), None)

  given [-->[_, _]]: Monoid2[ArrowLogging[-->]] with
    def empty[A, B]: ArrowLog[-->, A, B] =
      ArrowLog(None, None)

    def combine[A, B](
        x: ArrowLog[-->, A, B],
        y: ArrowLog[-->, A, B]
    ): ArrowLog[-->, A, B] =
      ArrowLog(x.name.orElse(y.name), x.arrow.orElse(y.arrow))

trait ArrowLogger[-->[_, _]]:
  def apply[A, B](name: String, arrow: A --> B): A --> B

object ArrowLogging:
  extension [T[_[_, _]], -->[_, _]](value: T[-->])
    inline def withArrowLogging(
        logger: ArrowLogger[-->]
    )(using
        Functor2K[T],
        Semigroup2K[T],
        FieldsNames[T[ArrowLogging[-->]]]
    ): T[-->] =
      Functor2K
        .apply[T]
        .mapK(
          Semigroup2K
            .apply[T]
            .combineK[ArrowLogging[-->]](
              summon[FieldsNames[T[ArrowLogging[-->]]]].withFieldsNames(""),
              Functor2K
                .apply[T]
                .mapK(value)(
                  [A, B] => (arrow: A --> B) => ArrowLog(None, Some(arrow))
                )
            )
        )(
          [A, B] =>
            (logged: ArrowLog[-->, A, B]) =>
              logger(
                logged.name.getOrElse("unknown name"),
                logged.arrow.getOrElse(
                  throw IllegalStateException("Missing arrow for logged field")
                )
            )
        )
