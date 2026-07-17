import io.github.mercurievv.minuscles.fieldsnames.FieldsNames
import io.github.mercurievv.minuscles.fieldsnames.derivation.semiauto.FieldNamesDerivation.fieldsNames
import io.github.mercurievv.minuscles.shapeless3typeclasses.Monoid as ProductMonoid

import scala.compiletime.erasedValue
import scala.compiletime.summonFrom
import scala.deriving.Mirror

type ArrowLogging[-->[_, _]] = [A, B] =>> ArrowLog[-->, A, B]

final case class ArrowLog[-->[_, _], A, B](
    name: Option[String],
    arrow: Option[A --> B]
)

object ArrowLog:
  given [-->[_, _], A, B]: FieldsNames[ArrowLog[-->, A, B]] with
    def withFieldsNames(fieldInfo: String): ArrowLog[-->, A, B] =
      ArrowLog(Some(fieldInfo), None)

  given [-->[_, _], A, B]: ProductMonoid[ArrowLog[-->, A, B]] with
    def empty: ArrowLog[-->, A, B] = ArrowLog(None, None)

    def combine(
        x: ArrowLog[-->, A, B],
        y: ArrowLog[-->, A, B]
    ): ArrowLog[-->, A, B] =
      ArrowLog(x.name.orElse(y.name), x.arrow.orElse(y.arrow))

trait ArrowLogger[-->[_, _]]:
  def apply[A, B](name: String, arrow: A --> B): A --> B

object ArrowLogging:
  extension [-->[_, _]](value: BusinessLogic[-->])
    inline def withArrowLogging(
        logger: ArrowLogger[-->]
    ): BusinessLogic[-->] =
      lowerProduct[BusinessLogic[ArrowLogging[-->]], BusinessLogic[-->], -->](
        ProductMonoid[BusinessLogic[ArrowLogging[-->]]].combine(
          summon[FieldsNames[BusinessLogic[ArrowLogging[-->]]]]
            .withFieldsNames(""),
          mapProduct[
            BusinessLogic[-->],
            BusinessLogic[ArrowLogging[-->]],
            -->
          ](
            value,
            [A, B] => (arrow: A --> B) => ArrowLog(None, Some(arrow))
          )
        ),
        logger
      )

  private inline def mapProduct[From, To, -->[_, _]](
      value: From,
      mapArrow: [A, B] => A --> B => ArrowLog[-->, A, B]
  )(using
      fromMirror: Mirror.ProductOf[From],
      toMirror: Mirror.ProductOf[To]
  ): To =
    toMirror.fromProduct(
      mapFields[
        fromMirror.MirroredElemTypes,
        toMirror.MirroredElemTypes,
        -->
      ](Tuple.fromProduct(value.asInstanceOf[Product]), mapArrow)
    )

  private inline def mapFields[
      FromFields <: Tuple,
      ToFields <: Tuple,
      -->[_, _]
  ](
      fields: Tuple,
      mapArrow: [A, B] => A --> B => ArrowLog[-->, A, B]
  ): Tuple =
    inline erasedValue[(FromFields, ToFields)] match
      case _: (EmptyTuple, EmptyTuple) =>
        EmptyTuple
      case _: ((fromField *: fromFieldsTail), (toField *: toFieldsTail)) =>
        val fieldValue = fields.asInstanceOf[NonEmptyTuple].head
        mapField[fromField, toField, -->](fieldValue, mapArrow) *:
          mapFields[fromFieldsTail, toFieldsTail, -->](
            fields.asInstanceOf[NonEmptyTuple].tail,
            mapArrow
          )

  private inline def mapField[FromField, ToField, -->[_, _]](
      field: Any,
      mapArrow: [A, B] => A --> B => ArrowLog[-->, A, B]
  ): Any =
    inline erasedValue[(FromField, ToField)] match
      case _: (-->[input, output], ArrowLog[--> @unchecked, _, _]) =>
        mapArrow[input, output](field.asInstanceOf[input --> output])
      case _ =>
        summonFrom { case fromMirror: Mirror.ProductOf[FromField] =>
          summonFrom {
            case toMirror: Mirror.ProductOf[ToField] =>
              mapProduct[FromField, ToField, -->](
                field.asInstanceOf[FromField],
                mapArrow
              )(using fromMirror, toMirror)
            case _ =>
              field
          }
        }

  private inline def lowerProduct[From, To, -->[_, _]](
      value: From,
      logger: ArrowLogger[-->]
  )(using
      fromMirror: Mirror.ProductOf[From],
      toMirror: Mirror.ProductOf[To]
  ): To =
    toMirror.fromProduct(
      lowerFields[
        fromMirror.MirroredElemTypes,
        toMirror.MirroredElemTypes,
        -->
      ](
        Tuple.fromProduct(value.asInstanceOf[Product]),
        logger
      )
    )

  private inline def lowerFields[
      FromFields <: Tuple,
      ToFields <: Tuple,
      -->[_, _]
  ](
      fields: Tuple,
      logger: ArrowLogger[-->]
  ): Tuple =
    inline erasedValue[(FromFields, ToFields)] match
      case _: (EmptyTuple, EmptyTuple) =>
        EmptyTuple
      case _: ((fromField *: fromFieldsTail), (toField *: toFieldsTail)) =>
        val fieldValue = fields.asInstanceOf[NonEmptyTuple].head
        lowerField[fromField, toField, -->](fieldValue, logger) *:
          lowerFields[fromFieldsTail, toFieldsTail, -->](
            fields.asInstanceOf[NonEmptyTuple].tail,
            logger
          )

  private inline def lowerField[FromField, ToField, -->[_, _]](
      field: Any,
      logger: ArrowLogger[-->]
  ): Any =
    inline erasedValue[(FromField, ToField)] match
      case _: (ArrowLog[--> @unchecked, input, output], -->[_, _]) =>
        val logged = field.asInstanceOf[ArrowLog[-->, input, output]]
        logger(
          logged.name.getOrElse("unknown"),
          logged.arrow.getOrElse(
            throw IllegalStateException("Missing arrow for logged field")
          )
        )
      case _ =>
        summonFrom { case fromMirror: Mirror.ProductOf[FromField] =>
          summonFrom {
            case toMirror: Mirror.ProductOf[ToField] =>
              lowerProduct[FromField, ToField, -->](
                field.asInstanceOf[FromField],
                logger
              )(using fromMirror, toMirror)
            case _ =>
              field
          }
        }
