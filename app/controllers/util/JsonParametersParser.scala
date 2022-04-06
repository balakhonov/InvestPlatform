package controllers.util

import com.balakhonov.invest.util.{DateTimeFormat, DateUtil}
import com.balakhonov.validation.codes.ErrorCodes
import com.balakhonov.validation.util.ErrorHelper.{systemError, validationError}
import play.api.libs.json._

import java.lang.reflect.Field
import java.time._
import java.time.format.DateTimeFormatterBuilder
import java.time.temporal.ChronoField.{DAY_OF_MONTH, MONTH_OF_YEAR}
import java.util.Base64
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

object JsonParametersParser { //scalastyle:ignore

  type StringValidator = String => Any
  type IntValidator = Int => Any
  type LongValidator = Long => Any
  type DoubleValidator = Double => Any
  type BooleanValidator = Boolean => Any
  type DayOfWeekValidator = DayOfWeek => Any
  type MonthValidator = Month => Any
  type MonthDayValidator = MonthDay => Any
  type LocalTimeValidator = LocalTime => Any
  type LocalDateValidator = LocalDate => Any
  type LocalDateTimeValidator = LocalDateTime => Any
  type ZonedDateTimeValidator = ZonedDateTime => Any
  type ByteArrayValidator = Array[Byte] => Any
  type JsObjectValidator = JsObject => Any
  type TypedValidator[T] = T => Unit

  private val S = classOf[String]
  private val JSO = classOf[JsObject]
  private val I = classOf[Int]
  private val L = classOf[Long]
  private val DE = classOf[Double]
  private val B = classOf[Boolean]
  private val BA = classOf[scala.Array[Byte]]
  private val LD = classOf[LocalDate]
  private val ZDT = classOf[ZonedDateTime]
  // Raw timestamp. Without timezone
  private val LDT = classOf[LocalDateTime]
  private val DOW = classOf[DayOfWeek]
  private val M = classOf[Month]
  private val MD = classOf[MonthDay]

  private val DAY_OF_MONTH_FORMATTER = new DateTimeFormatterBuilder()
    .appendValue(MONTH_OF_YEAR, 2)
    .appendLiteral('-')
    .appendValue(DAY_OF_MONTH, 2)
    .toFormatter

  def isPropertyExists(propName: String)(implicit obj: JsValue): Boolean = {
    (obj \ propName).toOption.isDefined
  }

  def parseString(propName: String, validators: StringValidator*)(implicit obj: JsValue): String = {
    parseValue(obj, propName, S, validators.toList)
  }

  def parseInt(propName: String, validators: IntValidator*)(implicit obj: JsValue): Int = {
    parseValue(obj, propName, I, validators.toList)
  }

  def parseDayOfWeek(propName: String, validators: DayOfWeekValidator*)(implicit obj: JsValue): DayOfWeek = {
    parseValue(obj, propName, DOW, validators.toList)
  }

  def parseMonth(propName: String, validators: MonthValidator*)(implicit obj: JsValue): Month = {
    parseValue(obj, propName, M, validators.toList)
  }

  def parseMonthDay(propName: String, validators: MonthDayValidator*)(implicit obj: JsValue): MonthDay = {
    parseValue(obj, propName, MD, validators.toList)
  }

  def parseForeignKey(propName: String, validators: IntValidator*)(implicit obj: JsValue): Int = {
    val id = parseInt(propName, validators: _*)

    if (id < 1)
      validationError(ErrorCodes.FOREIGN_KEY_SHOULD_BE_GREATER_THAN_ZERO(propName, id))

    id
  }

  def parseForeignKeyArray(propName: String)(implicit obj: JsValue): Seq[Int] = {
    parseArray(propName).value.map(jsFKtoInt(propName, _)).toSeq
  }

  def parseOptionalForeignKeyArray(propName: String)
                                  (implicit obj: JsValue): Option[Seq[Int]] = {
    parseOptionalArray(propName) match {
      case Some(arr) =>
        val res = arr.value.map(jsFKtoInt(propName, _)).toSeq
        Some(res)

      case _ => None
    }
  }

  def parsePrimaryKey()(implicit obj: JsValue): Int = {
    val id = parseInt("id")
    require(id > 0, s"Primary key 'id'='$id' should be > 0")

    id
  }

  def parseMinutes(propName: String, validators: IntValidator*)
                  (implicit obj: JsValue): Int = {
    val minutes = parseInt(propName, validators: _*)
    require(minutes >= 0 && minutes <= 59, s"Minutes value '$propName'='$minutes' should be in range [0, 59]")
    minutes
  }

  def parseLong(propName: String, validators: LongValidator*)
               (implicit obj: JsValue): Long = {
    parseValue(obj, propName, L, validators.toList)
  }

  def parseDouble(propName: String, validators: DoubleValidator*)
                 (implicit obj: JsValue): Double = {
    parseValue(obj, propName, DE, validators.toList)
  }

  def parseBoolean(propName: String, validators: BooleanValidator*)
                  (implicit obj: JsValue): Boolean = {
    parseValue(obj, propName, B, validators.toList)
  }

  def parseBlob(propName: String, validators: ByteArrayValidator*)
               (implicit obj: JsValue): scala.Array[Byte] = {
    parseValue(obj, propName, BA, validators.toList)
  }

  def parseLocalDate(propName: String, validators: LocalDateValidator*)
                    (implicit obj: JsValue): LocalDate = {
    parseValue(obj, propName, LD, validators.toList)
  }

  def parseZonedDateTime(propName: String, validators: ZonedDateTimeValidator*)
                        (implicit obj: JsValue): ZonedDateTime = {
    parseValue(obj, propName, ZDT, validators.toList)
  }

  def parseLocalDateTime(propName: String, validators: LocalDateTimeValidator*)
                        (implicit obj: JsValue): LocalDateTime = {
    parseValue(obj, propName, LDT, validators.toList)
  }

  def parseLocalTime(propName: String)(implicit obj: JsValue): LocalTime = {
    parseLocalTimeValue(obj, propName)
  }

  def parseArray(propName: String)(implicit obj: JsValue): JsArray = {
    (obj \ propName).asOpt[JsArray].getOrElse {
      validationError(ErrorCodes.JSON_ARRAY_IS_NOT_FOUND(propName))
    }
  }

  def parseIntArray(propName: String)(implicit obj: JsValue): Seq[Int] = {
    parseArray(propName).value.map { item =>
      Try(item.as[Int]).getOrElse {
        validationError(ErrorCodes.ELEMENT_IS_NOT_AN_INTEGER_VALUE_IN_ARRAY(item.toString, propName))
      }
    }.toSeq
  }

  def parseJsObject(propName: String)(implicit obj: JsValue): JsObject = {
    (obj \ propName).asOpt[JsObject].getOrElse {
      validationError(ErrorCodes.JSON_OBJECT_IS_NOT_FOUND(propName))
    }
  }

  def parseJsArray(propName: String)(implicit obj: JsValue): JsArray = {
    (obj \ propName).asOpt[JsArray].getOrElse {
      validationError(ErrorCodes.JSON_ARRAY_IS_NOT_FOUND(propName))
    }
  }

  def parseBinaryInt(propName: String, size: Int, validators: IntValidator*)
                    (implicit obj: JsValue): Int = {
    val value = parseValue(obj, propName, S, Nil)
    parseStringAsBinaryInt(propName, value, size, validators: _*)
  }

  def parseOptionalBinaryInt(propName: String, size: Int, validators: IntValidator*)
                            (implicit obj: JsValue): Option[Int] = {
    parseOptionalValue(obj, propName, S, Nil) collect { case Some(value) =>
      parseStringAsBinaryInt(propName, value, size, validators: _*)
    }
  }

  def parseOptionalArray(propName: String)(implicit obj: JsValue): Option[JsArray] = {
    (obj \ propName).asOpt[JsArray]
  }

  def parseOptionalIntArray(propName: String)(implicit obj: JsValue): Option[Seq[Int]] = {
    parseOptionalArray(propName).map { array =>
      array.value.map { item =>
        Try(item.as[Int]).getOrElse {
          validationError(ErrorCodes.ELEMENT_IS_NOT_AN_INTEGER_VALUE_IN_ARRAY(item.toString, propName))
        }
      }.toSeq
    }
  }

  def parseOptionalList[T](propName: String, parser: JsValue => T)(implicit obj: JsValue): Option[List[T]] = {
    parseOptionalArray(propName).map(_.value.map(parser(_)).toList)
  }

  def parseOptionalJsObject(propName: String)(implicit obj: JsValue): Option[JsObject] = {
    (obj \ propName).asOpt[JsObject]
  }

  def parseOptionalObject[T](propName: String, parser: JsValue => T)(implicit obj: JsValue): Option[T] = {
    parseOptionalJsObject(propName).map(parser)
  }

  def parseOptionalString(propName: String, validators: StringValidator*)
                         (implicit obj: JsValue): Option[String] = {
    parseOptionalStringValue(propName, validators: _ *).flatten
  }

  def parseOptionalMetaFieldString(propName: String, validators: StringValidator*)
                                  (implicit obj: JsValue,
                                   metaFieldsRequired: Map[String, Boolean]): Option[String] = {
    if (metaFieldsRequired(propName)) {
      Some(parseString(propName, validators: _ *))
    } else {
      parseOptionalString(propName, validators: _ *)
    }
  }

  def parseOptionalInt(propName: String, validators: IntValidator*)
                      (implicit obj: JsValue): Option[Int] = {
    parseOptionalValue(obj, propName, I, validators.toList).flatten
  }

  def parseOptionalMetaFieldInt(propName: String, validators: IntValidator*)
                               (implicit obj: JsValue,
                                metaFieldsRequired: Map[String, Boolean]): Option[Int] = {
    if (metaFieldsRequired(propName)) {
      Some(parseInt(propName, validators: _ *))
    } else {
      parseOptionalInt(propName, validators: _*)
    }
  }

  def parseOptionalDayOfWeek(propName: String, validators: DayOfWeekValidator*)
                            (implicit obj: JsValue): Option[DayOfWeek] = {
    parseOptionalValue(obj, propName, DOW, validators.toList).flatten
  }

  def parseOptionalMonth(propName: String, validators: MonthValidator*)
                        (implicit obj: JsValue): Option[Month] = {
    parseOptionalValue(obj, propName, M, validators.toList).flatten
  }

  def parseOptionalMonthDay(propName: String, validators: MonthDayValidator*)
                           (implicit obj: JsValue): Option[MonthDay] = {
    parseOptionalValue(obj, propName, MD, validators.toList).flatten
  }

  def parseOptionalForeignKey(propName: String, validators: IntValidator*)
                             (implicit obj: JsValue): Option[Int] = {
    val id = parseOptionalInt(propName, validators: _*)
    if (id.exists(_ < 1)) None else id
  }

  def parseOptionalMetaFieldForeignKey(propName: String, validators: IntValidator*)
                                      (implicit obj: JsValue,
                                       metaFieldsRequired: Map[String, Boolean]): Option[Int] = {
    if (metaFieldsRequired(propName)) {
      Some(parseForeignKey(propName, validators: _ *))
    } else {
      parseOptionalForeignKey(propName, validators: _ *)
    }
  }

  def parseOptionalPrimaryKey()(implicit obj: JsValue): Option[Int] = {
    val id = parseOptionalInt("id")
    if (id.exists(_ < 1)) None else id
  }

  def parseOptionalLong(propName: String, validators: LongValidator*)
                       (implicit obj: JsValue): Option[Long] = {
    parseOptionalValue(obj, propName, L, validators.toList).flatten
  }

  def parseOptionalDouble(propName: String, validators: DoubleValidator*)
                         (implicit obj: JsValue): Option[Double] = {
    parseOptionalValue(obj, propName, DE, validators.toList).flatten
  }

  def parseOptionalBoolean(propName: String, validators: BooleanValidator*)
                          (implicit obj: JsValue): Option[Boolean] = {
    parseOptionalValue(obj, propName, B, validators.toList).flatten
  }

  def parseOptionalBlob(propName: String, validators: ByteArrayValidator*)
                       (implicit obj: JsValue): Option[scala.Array[Byte]] = {
    parseOptionalValue(obj, propName, BA, validators.toList).flatten
  }

  def parseOptionalLocalDate(propName: String, validators: LocalDateValidator*)
                            (implicit obj: JsValue): Option[LocalDate] = {
    parseOptionalValue(obj, propName, LD, validators.toList).flatten
  }

  def parseOptionalMetaFieldLocalDate(propName: String, validators: LocalDateValidator*)
                                     (implicit obj: JsValue,
                                      metaFieldsRequired: Map[String, Boolean]): Option[LocalDate] = {
    if (metaFieldsRequired(propName)) {
      Some(parseLocalDate(propName, validators: _ *))
    } else {
      parseOptionalLocalDate(propName, validators: _ *)
    }
  }

  def parseOptionalZonedDateTime(propName: String, validators: ZonedDateTimeValidator*)
                                (implicit obj: JsValue): Option[ZonedDateTime] = {
    parseOptionalValue(obj, propName, ZDT, validators.toList).flatten
  }

  def parseOptionalLocalTime(propName: String)(implicit obj: JsValue): Option[LocalTime] = {
    parseOptionalLocalTime(obj, propName).flatten
  }

  def parseOptionalLocalDateTime(propName: String, validators: LocalDateTimeValidator*)
                                (implicit obj: JsValue): Option[LocalDateTime] = {
    parseOptionalValue(obj, propName, LDT, validators.toList).flatten
  }

  def updateOptionalJsObject(propName: String, instance: Any, validators: JsObjectValidator*)
                            (implicit obj: JsValue, map: mutable.Map[String, MapLogFieldValue]): Unit = {
    updateOptional(parseOptionalValue(obj, propName, JSO, validators.toList), instance, propName)
  }

  def updateOptionalMetaFieldString(propName: String, instance: Any, validators: StringValidator*)
                                   (implicit obj: JsValue,
                                    map: mutable.Map[String, MapLogFieldValue],
                                    metaFieldsRequired: Map[String, Boolean]): Unit = {
    updateOptionalMetaField(parseOptionalStringValue(propName, validators: _ *), instance, propName)
  }

  def updateOptionalString(propName: String, instance: Any, validators: StringValidator*)
                          (implicit obj: JsValue, map: mutable.Map[String, MapLogFieldValue]): Unit = {
    updateOptional(parseOptionalStringValue(propName, validators: _ *), instance, propName)
  }

  def updateReplaceableOptionalString(propNameOriginal: String,
                                      propNameReplaceable: String,
                                      instance: Any,
                                      validators: StringValidator*)
                                     (implicit obj: JsValue, map: mutable.Map[String, MapLogFieldValue]): Unit = {
    updateOptional(parseOptionalStringValue(propNameOriginal, validators: _ *), instance, propNameReplaceable)
  }

  def updateOptionalInt(propName: String, instance: Any, validators: IntValidator*)
                       (implicit obj: JsValue, map: mutable.Map[String, MapLogFieldValue]): Unit = {
    updateOptional(parseOptionalValue(obj, propName, I, validators.toList), instance, propName)
  }

  def updateOptionalMetaFieldInt(propName: String, instance: Any, validators: IntValidator*)
                                (implicit obj: JsValue,
                                 map: mutable.Map[String, MapLogFieldValue],
                                 metaFieldsRequired: Map[String, Boolean]): Unit = {
    updateOptionalMetaField(parseOptionalValue(obj, propName, I, validators.toList), instance, propName)
  }

  def updateOptionalDayOfWeek(propName: String, instance: Any, validators: DayOfWeekValidator*)
                             (implicit obj: JsValue, map: mutable.Map[String, MapLogFieldValue]): Unit = {
    updateOptional(parseOptionalValue(obj, propName, DOW, validators.toList), instance, propName)
  }

  def updateOptionalMonth(propName: String, instance: Any, validators: MonthValidator*)
                         (implicit obj: JsValue, map: mutable.Map[String, MapLogFieldValue]): Unit = {
    updateOptional(parseOptionalValue(obj, propName, M, validators.toList), instance, propName)
  }

  def updateOptionalMonthDay(propName: String, instance: Any, validators: MonthDayValidator*)
                            (implicit obj: JsValue, map: mutable.Map[String, MapLogFieldValue]): Unit = {
    updateOptional(parseOptionalValue(obj, propName, MD, validators.toList), instance, propName)
  }

  def updateOptionalForeignKey(propName: String, instance: Any, validators: IntValidator*)
                              (implicit obj: JsValue, map: mutable.Map[String, MapLogFieldValue]): Unit = {
    val id = parseOptionalValue(obj, propName, I, validators.toList)

    validateID(id.flatten, propName)

    updateOptional(id, instance, propName)
  }

  def updateOptionalMetaFieldForeignKey(propName: String, instance: Any, validators: IntValidator*)
                                       (implicit obj: JsValue,
                                        map: mutable.Map[String, MapLogFieldValue],
                                        metaFieldsRequired: Map[String, Boolean]): Unit = {
    val id = parseOptionalValue(obj, propName, I, validators.toList)

    validateID(id.flatten, propName)

    updateOptionalMetaField(id, instance, propName)
  }

  def updateOptionalLong(propName: String, instance: Any, validators: LongValidator*)
                        (implicit obj: JsValue, map: mutable.Map[String, MapLogFieldValue]): Unit = {
    updateOptional(parseOptionalValue(obj, propName, L, validators.toList), instance, propName)
  }

  def updateOptionalDouble(propName: String, instance: Any, validators: DoubleValidator*)
                          (implicit obj: JsValue,
                           map: mutable.Map[String, MapLogFieldValue]): Unit = {
    updateOptional(parseOptionalValue(obj, propName, DE, validators.toList), instance, propName)
  }

  def updateOptionalBoolean(propName: String, instance: Any, validators: BooleanValidator*)
                           (implicit obj: JsValue, map: mutable.Map[String, MapLogFieldValue]): Unit = {
    updateOptional(parseOptionalValue(obj, propName, B, validators.toList), instance, propName)
  }

  def updateOptionalBlob(propName: String, instance: Any, validators: ByteArrayValidator*)
                        (implicit obj: JsValue, map: mutable.Map[String, MapLogFieldValue]): Unit = {
    updateOptional(parseOptionalValue(obj, propName, BA, validators.toList), instance, propName)
  }

  def updateOptionalLocalDate(propName: String, instance: Any, validators: LocalDateValidator*)
                             (implicit obj: JsValue, map: mutable.Map[String, MapLogFieldValue]): Unit = {
    updateOptional(parseOptionalValue(obj, propName, LD, validators.toList), instance, propName)
  }

  def updateOptionalMetaFieldLocalDate(propName: String, instance: Any, validators: LocalDateValidator*)
                                      (implicit obj: JsValue,
                                       map: mutable.Map[String, MapLogFieldValue],
                                       metaFieldsRequired: Map[String, Boolean]): Unit = {
    updateOptionalMetaField(parseOptionalValue(obj, propName, LD, validators.toList), instance, propName)
  }

  def updateOptionalZonedDateTime(propName: String, instance: Any, validators: ZonedDateTimeValidator*)
                                 (implicit obj: JsValue, map: mutable.Map[String, MapLogFieldValue]): Unit = {
    updateOptional(parseOptionalValue(obj, propName, ZDT, validators.toList), instance, propName)
  }

  def updateOptionalLocalTime(propName: String, instance: Any)
                             (implicit obj: JsValue, map: mutable.Map[String, MapLogFieldValue]): Unit = {
    updateOptional(parseOptionalLocalTime(obj, propName), instance, propName)
  }

  def updateOptionalLocalDateTime(propName: String, instance: Any, validators: LocalDateTimeValidator*)
                                 (implicit obj: JsValue, map: mutable.Map[String, MapLogFieldValue]): Unit = {
    updateOptional(parseOptionalValue(obj, propName, LDT, validators.toList), instance, propName)
  }

  def updateString(propName: String, instance: Any, validators: StringValidator*)
                  (implicit obj: JsValue, m: mutable.Map[String, MapLogFieldValue]): Unit = {
    update(parseOptionalStringValue(propName, validators: _ *), instance, propName)
  }

  def updateReplaceableString(propNameOriginal: String,
                              propNameReplaceable: String,
                              instance: Any,
                              validators: StringValidator*)
                             (implicit obj: JsValue, m: mutable.Map[String, MapLogFieldValue]): Unit = {
    update(parseOptionalStringValue(propNameOriginal, validators: _ *), instance, propNameReplaceable)
  }

  def updateInt(propName: String, instance: Any, validators: IntValidator*)
               (implicit obj: JsValue, m: mutable.Map[String, MapLogFieldValue]): Unit =
    update(parseOptionalValue(obj, propName, I, validators.toList), instance, propName)

  def updateDayOfWeek(propName: String, instance: Any, validators: DayOfWeekValidator*)
                     (implicit obj: JsValue, m: mutable.Map[String, MapLogFieldValue]): Unit =
    update(parseOptionalValue(obj, propName, DOW, validators.toList), instance, propName)

  def updateMonth(propName: String, instance: Any, validators: MonthValidator*)
                 (implicit obj: JsValue, m: mutable.Map[String, MapLogFieldValue]): Unit =
    update(parseOptionalValue(obj, propName, M, validators.toList), instance, propName)

  def updateMonthDay(propName: String, instance: Any, validators: MonthDayValidator*)
                    (implicit obj: JsValue, m: mutable.Map[String, MapLogFieldValue]): Unit =
    update(parseOptionalValue(obj, propName, MD, validators.toList), instance, propName)

  def updateForeignKey(propName: String, instance: Any, validators: IntValidator*)
                      (implicit obj: JsValue, map: mutable.Map[String, MapLogFieldValue]): Unit = {
    val id = parseOptionalValue(obj, propName, I, validators.toList)

    validateID(id.flatten, propName)

    update(id, instance, propName)
  }

  def updateLong(propName: String, instance: Any, validators: LongValidator*)
                (implicit obj: JsValue, m: mutable.Map[String, MapLogFieldValue]): Unit = {
    update(parseOptionalValue(obj, propName, L, validators.toList), instance, propName)
  }

  def updateDouble(propName: String, instance: Any, validators: DoubleValidator*)
                  (implicit obj: JsValue, m: mutable.Map[String, MapLogFieldValue]): Unit = {
    update(parseOptionalValue(obj, propName, DE, validators.toList), instance, propName)
  }

  def updateBoolean(propName: String, instance: Any, validators: BooleanValidator*)
                   (implicit obj: JsValue, m: mutable.Map[String, MapLogFieldValue]): Unit = {
    update(parseOptionalValue(obj, propName, B, validators.toList), instance, propName)
  }

  def updateBlob(propName: String, instance: Any, validators: ByteArrayValidator*)
                (implicit obj: JsValue, m: mutable.Map[String, MapLogFieldValue]): Unit =
    update(parseOptionalValue(obj, propName, BA, validators.toList), instance, propName)

  def updateLocalDate(propName: String, instance: Any, validators: LocalDateValidator*)
                     (implicit obj: JsValue, m: mutable.Map[String, MapLogFieldValue]): Unit =
    update(parseOptionalValue(obj, propName, LD, validators.toList), instance, propName)

  def updateJsonObject(propName: String, instance: Any, validators: JsObjectValidator*)
                      (implicit obj: JsValue, m: mutable.Map[String, MapLogFieldValue]): Unit = {
    update(parseOptionalValue(obj, propName, JSO, validators.toList), instance, propName)
  }

  def updateZonedDateTime(propName: String, instance: Any, validators: ZonedDateTimeValidator*)
                         (implicit obj: JsValue, m: mutable.Map[String, MapLogFieldValue]): Unit =
    update(parseOptionalValue(obj, propName, ZDT, validators.toList), instance, propName)

  def updateLocalTime(propName: String, instance: Any)
                     (implicit obj: JsValue, m: mutable.Map[String, MapLogFieldValue]): Unit =
    update(parseOptionalLocalTime(obj, propName), instance, propName)

  def updateLocalDateTime(propName: String, instance: Any, validators: LocalDateTimeValidator*)
                         (implicit obj: JsValue, m: mutable.Map[String, MapLogFieldValue]): Unit =
    update(parseOptionalValue(obj, propName, LDT, validators.toList), instance, propName)

  def updateBinaryInt(propName: String, size: Int, instance: Any, validators: IntValidator*)
                     (implicit obj: JsValue, m: mutable.Map[String, MapLogFieldValue]): Unit = {
    val option = parseOptionalValue(obj, propName, S, Nil)
    val intOpt: Option[Option[Int]] = option.map(_.map { value =>
      validateBinaryCharacters(propName, value)
      validateBinaryLength(size, value)

      Integer.parseInt(value, 2)
    })

    intOpt.flatten.foreach { result =>
      validators.foreach { validator =>
        validator(result)
      }
    }

    update(intOpt, instance, propName)
  }

  def update[A](op: Option[Option[A]], instance: Any, propName: String)
               (implicit map: mutable.Map[String, MapLogFieldValue]): Unit = {
    op match {
      case Some(None) =>
        validationError(ErrorCodes.JSON_PROPERTY_IS_REQUIRED(propName))

      case Some(Some(value)) =>
        updateValue(Left(value), instance, propName)

      case _ =>
      // Do not update field if property was not found
    }
  }

  private def updateOptional[A](op: Option[Option[A]], instance: Any, propName: String)
                               (implicit map: mutable.Map[String, MapLogFieldValue]): Unit = {
    op match {
      case Some(option) =>
        updateValue(Right(option), instance, propName)

      case _ =>
      // Do not update field if property was not found
    }
  }

  private def updateOptionalMetaField[A](op: Option[Option[A]], instance: Any, propName: String)
                                        (implicit map: mutable.Map[String, MapLogFieldValue],
                                         metaFieldsRequired: Map[String, Boolean]): Unit = {
    op match {
      case Some(None) if metaFieldsRequired(propName) =>
        validationError(ErrorCodes.JSON_PROPERTY_IS_REQUIRED(propName))

      case Some(option) =>
        updateValue(Right(option), instance, propName)

      case _ =>
      // Do not update field if property was not found
    }
  }

  private def getOldValue(field: Field, instance: Any) = {
    Try {
      (field.get(instance) match {
        case f: Option[_] => f
        case f => Some(f)
      }).map(anyToString)
    }.toOption.flatten
  }

  private def anyToString(v: Any): String = v match {
    case value: Double => String.format("%f", Double.box(value))
    case other => other.toString
  }

  private def updateValue(anyVal: Either[Any, Option[Any]], instance: Any, propName: String)
                         (implicit map: mutable.Map[String, MapLogFieldValue]): Unit = {
    try {
      val field = instance.getClass.getDeclaredField(propName)
      field.setAccessible(true)

      val oldValue = getOldValue(field, instance)

      anyVal match {
        case Left(value) =>
          field.set(instance, value)
          map.put(propName, MapLogFieldValue(oldValue, Some(anyToString(value))))

        case Right(option) =>
          field.set(instance, option)
          map.put(propName, MapLogFieldValue(oldValue, option.map(anyToString)))
      }
    } catch {
      case _: NoSuchFieldException =>
        validationError(ErrorCodes.FIELD_IS_NOT_FOUND_IN_INSTANCE(propName, instance.toString))
      case e: Exception =>
        systemError(s"Unknown exception ${e.getMessage}")
    }
  }

  private def parseValue[A](jsObj: JsValue, // scalastyle:ignore
                            propName: String,
                            t: Class[A],
                            validators: List[A => Any]): A = {
    (jsObj \ propName).toOption match {
      case None => validationError(ErrorCodes.JSON_PROPERTY_NOT_FOUND(propName))
      case Some(JsNull) => validationError(ErrorCodes.JSON_PROPERTY_IS_NULL(propName))
      case Some(jsValue) =>
        val result = Try[A] {
          val res = (t, jsValue) match {
            case (I, JsNumber(v)) => v.toInt
            case (L, JsNumber(v)) => v.toLong
            case (I, JsString(v)) => v.toInt
            case (S, JsString(v)) => filterNonPrintableText(v).trim
            case (S, JsNumber(v)) => v.toString
            case (DE, JsNumber(v)) => v.toDouble
            case (DE, JsString(v)) => v.toDouble
            case (B, JsBoolean(v)) => v
            case (B, JsString(v)) => v.toBoolean
            case (LD, JsNumber(v)) => Instant.ofEpochMilli(v.toLong).atZone(ZoneId.systemDefault()).toLocalDate
            case (LD, JsString(v)) => DateTimeFormat.parseLocalDate(v)
            // case (ZDT, JsNumber(v)) => Instant.ofEpochMilli(v.toLong).atZone(ZoneId.systemDefault())
            case (ZDT, JsString(v)) => DateTimeFormat.parseZonedDateTime(v)
            case (LDT, JsNumber(v)) => Instant.ofEpochMilli(v.toLong).atZone(ZoneId.systemDefault()).toLocalDateTime
            case (LDT, JsString(v)) => DateTimeFormat.parseLocalDateTime(v)
            case (BA, JsString(v)) => Base64.getDecoder.decode(v)
            case (DOW, JsNumber(v)) => DateUtil.fromUtilCalendarWeek(v.toInt)
            case (M, JsNumber(v)) => Month.of(v.toInt)
            case (MD, JsString(v)) => MonthDay.parse(v, DAY_OF_MONTH_FORMATTER)
            case _ => throw new UnsupportedOperationException()
          }

          res.asInstanceOf[A]
        }

        result match {
          case Success(value) =>
            // validate value
            validators.foreach { validator =>
              validator(value)
            }

            value

          case Failure(_) =>
            validationError(ErrorCodes.CAN_NOT_PARSE_FIELD_AS_TYPE(propName, t.getSimpleName))
        }
    }
  }

  private def parseLocalTimeValue[A](jsObj: JsValue, propName: String): LocalTime = {
    (jsObj \ propName).toOption match {
      case None => validationError(ErrorCodes.JSON_PROPERTY_NOT_FOUND(propName))
      case Some(JsNull) => validationError(ErrorCodes.JSON_PROPERTY_IS_NULL(propName))
      case Some(jsValue) =>
        jsValue match {
          case JsNumber(v) => Instant.ofEpochMilli(v.toLong).atZone(ZoneId.systemDefault()).toLocalTime
          case JsString(v) => DateTimeFormat.parseLocalTime(v)
          case js => validationError(ErrorCodes.UNSUPPORTED_TIMESTAMP_FORMAT_FOR_VALUE(Json.stringify(js)))
        }
    }
  }

  def parseOptionalForeignKeyValue(propName: String, validators: IntValidator*)
                                  (implicit obj: JsValue): Option[Option[Int]] = {
    val idOpt = parseOptionalValue(obj, propName, I, validators.toList)
    if (idOpt.flatten.exists(_ < 1)) None else idOpt
  }

  def parseOptionalBooleanValue(propName: String, validators: BooleanValidator*)
                               (implicit obj: JsValue): Option[Option[Boolean]] = {
    parseOptionalValue(obj, propName, B, validators.toList)
  }

  def parseOptionalIntValue(propName: String, validators: IntValidator*)
                           (implicit obj: JsValue): Option[Option[Int]] = {
    parseOptionalValue(obj, propName, I, validators.toList)
  }

  def parseOptionalStringValue(propName: String, validators: StringValidator*)
                              (implicit obj: JsValue): Option[Option[String]] = {
    parseOptionalValue(obj, propName, S, validators.toList)
  }

  def parseOptionalLocalDateValue(propName: String, validators: LocalDateValidator*)
                                 (implicit obj: JsValue): Option[Option[LocalDate]] = {
    parseOptionalValue(obj, propName, LD, validators.toList)
  }

  def parseOptionalDoubleValue(propName: String, validators: DoubleValidator*)
                              (implicit obj: JsValue): Option[Option[Double]] = {
    parseOptionalValue(obj, propName, DE, validators.toList)
  }

  def parseOptionalMetaFieldStringValue(propName: String, validators: StringValidator*)
                                       (implicit obj: JsValue,
                                        metaFieldsRequired: Map[String, Boolean]): Option[Option[String]] = {
    if (metaFieldsRequired(propName)) {
      Some(parseOptionalString(propName, validators: _ *))
    } else {
      parseOptionalStringValue(propName, validators: _ *)
    }
  }

  def parseOptionalMetaFieldLocalDateValue(propName: String, validators: LocalDateValidator*)
                                          (implicit obj: JsValue,
                                           metaFieldsRequired: Map[String, Boolean]): Option[Option[LocalDate]] = {
    if (metaFieldsRequired(propName)) {
      Some(parseOptionalLocalDate(propName, validators: _ *))
    } else {
      parseOptionalLocalDateValue(propName, validators: _ *)
    }
  }

  def parseOptionalMetaFieldForeignKeyValue(propName: String, validators: IntValidator*)
                                           (implicit obj: JsValue,
                                            metaFieldsRequired: Map[String, Boolean]): Option[Option[Int]] = {
    if (metaFieldsRequired(propName)) {
      Some(parseOptionalForeignKey(propName, validators: _ *))
    } else {
      parseOptionalForeignKeyValue(propName, validators: _ *)
    }
  }

  /**
   *
   * @return None, Option[None], Option[Option(_)]
   *         None - value not found
   *         Option[None] - value == null
   *         Option[Option] - value == some_value
   */
  private def parseOptionalValue[A](jsObj: JsValue, //scalastyle:ignore
                                    propName: String,
                                    t: Class[A],
                                    validators: List[A => Any]): Option[Option[A]] = {
    (jsObj \ propName).toOption match {
      case None => None
      case Some(JsNull) => Some(None)
      case Some(jsValue) =>
        val result = Try[A] {
          val res = (t, jsValue) match {
            case (I, JsNumber(v)) => v.toInt
            case (L, JsNumber(v)) => v.toLong
            case (I, JsString(v)) if v.toLowerCase == "null" => None
            case (I, JsString(v)) => v.toInt
            case (S, JsString(v)) =>
              if (v.trim.isEmpty) return Some(None) else filterNonPrintableText(v).trim //scalastyle:ignore
            case (S, JsNumber(v)) => v.toString
            case (JSO, v) => v
            case (DE, JsNumber(v)) => v.toDouble
            case (DE, JsString(v)) => v.toDouble
            case (B, JsBoolean(v)) => v
            case (B, JsString(v)) => v.toBoolean
            case (LD, JsNumber(v)) => Instant.ofEpochMilli(v.toLong).atZone(ZoneId.systemDefault()).toLocalDate
            case (LD, JsString(v)) => DateTimeFormat.parseLocalDate(v)
            // case (ZDT, JsNumber(v)) => Instant.ofEpochMilli(v.toLong).atZone(ZoneId.systemDefault())
            case (ZDT, JsString(v)) => DateTimeFormat.parseZonedDateTime(v)
            case (LDT, JsNumber(v)) => Instant.ofEpochMilli(v.toLong).atZone(ZoneId.systemDefault()).toLocalDateTime
            case (LDT, JsString(v)) => DateTimeFormat.parseLocalDateTime(v)
            case (BA, JsString(v)) => Base64.getDecoder.decode(v)
            case (DOW, JsNumber(v)) => DateUtil.fromUtilCalendarWeek(v.toInt)
            case (M, JsNumber(v)) => Month.of(v.toInt)
            case (MD, JsString(v)) => MonthDay.parse(v, DAY_OF_MONTH_FORMATTER)
            case _ => throw new UnsupportedOperationException()
          }

          res.asInstanceOf[A]
        }

        result match {
          case Success(value) =>
            // validate value
            validators.foreach { validator =>
              validator(value)
            }

            if (value == None) {
              Some(None)
            } else {
              Some(Some(value))
            }

          case Failure(_) =>
            validationError(ErrorCodes.CAN_NOT_PARSE_FIELD_AS_TYPE(propName, t.getSimpleName))
        }
    }
  }

  private def parseOptionalLocalTime(jsObj: JsValue, propName: String): Option[Option[LocalTime]] = {
    (jsObj \ propName).toOption match {
      case None => None
      case Some(JsNull) => Some(None)
      case Some(jsValue) =>
        jsValue match {
          case JsNumber(v) => Some(Some(Instant.ofEpochMilli(v.toLong).atZone(ZoneId.systemDefault()).toLocalTime))
          case JsString(v) => Some(Some(DateTimeFormat.parseLocalTime(v)))
          case js => validationError(ErrorCodes.UNSUPPORTED_LOCAL_TIME_FORMAT_FOR_VALUE(Json.stringify(js)))
        }
    }
  }

  private def filterNonPrintableText(v: String): String = {
    // remove non-printable Unicode characters
    v.split("\n").map(_.replaceAll("\\p{C}", "")).mkString("\n")
  }

  private def parseStringAsBinaryInt(propName: String, value: String, size: Int, validators: IntValidator*): Int = {
    validateBinaryCharacters(propName, value)
    validateBinaryLength(size, value)

    val result = Integer.parseInt(value, 2)

    validators.foreach { validator =>
      validator(result)
    }

    result
  }

  private def validateBinaryCharacters(propName: String, value: String): Unit = {
    if (!value.matches("[01]+"))
      validationError(ErrorCodes.PROPERTY_SHOULD_CONTAIN_BINARY_VALUE(propName, value))
  }

  private def validateBinaryLength(size: Int, value: String): Unit = {
    if (value.length != size)
      validationError(ErrorCodes.BINARY_INT_SIZE_IS_NOT_VALID(size, value.length))
  }

  private def validateID(id: Option[Int], propName: String): Unit = {
    if (id.exists(_ < 1))
      validationError(ErrorCodes.FOREIGN_KEY_SHOULD_BE_GREATER_THAN_ZERO(propName, id.get))
  }

  private def jsFKtoInt(propName: String, item: JsValue): Int = {
    val key = Try(item.as[Int]).getOrElse {
      validationError(ErrorCodes.ELEMENT_IS_NOT_AN_INTEGER_VALUE_IN_ARRAY(item.toString, propName))
    }

    if (key < 1)
      validationError(ErrorCodes.FOREIGN_KEY_SHOULD_BE_GREATER_THAN_ZERO(propName, key))

    key
  }

  implicit val localDateWrites: Writes[LocalDate] = Writes { date =>
    JsString(DateTimeFormat.formatLocalDate(date))
  }

  implicit val localDateTimeWrites: Writes[LocalDateTime] = Writes { date =>
    JsString(DateTimeFormat.formatLocalDateTime(date))
  }
}

object ChildActions {

  def empty[T]: ChildActions[T] = ChildActions[T](Nil, Nil, Nil)
}

case class ChildActions[A](toAdd: Seq[A], toUpdate: Seq[A], toRemove: Seq[Int]) {

  def isEmpty: Boolean = toAdd.isEmpty && toUpdate.isEmpty && toRemove.isEmpty

  def nonEmpty: Boolean = !isEmpty

  def ++=(actions: ChildActions[A]): ChildActions[A] = { //scalastyle:ignore
    ChildActions(
      toAdd ++ actions.toAdd,
      toUpdate ++ actions.toUpdate,
      toRemove ++ actions.toRemove
    )
  }

  def filter(p: A => Boolean): ChildActions[A] = copy(toAdd = toAdd.filter(p), toUpdate = toUpdate.filter(p))

  def filterNot(p: A => Boolean): ChildActions[A] = filter(!p(_))

  def map[B](f: A => B, f2: Int => Int = identity): ChildActions[B] = {
    ChildActions(toAdd.map(f(_)), toUpdate.map(f(_)), toRemove.map(f2))
  }

  def flatMap[B](f: A => Option[B], f2: Int => Int = identity): ChildActions[B] = {
    ChildActions(toAdd.flatMap(f(_)), toUpdate.flatMap(f(_)), toRemove.map(f2))
  }
}

case class MapLogFieldValue(oldValue: Option[String], newValue: Option[String])