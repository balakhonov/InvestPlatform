package org.squeryl.customtypes

import com.balakhonov.invest.models.enums.CandleTimeFrame
import com.balakhonov.invest.util.DateUtil
import org.squeryl.PrimitiveTypeMode
import org.squeryl.dsl._
import play.api.libs.json.{JsArray, JsObject, Json}

import java.sql.Timestamp
import java.time._
import java.util.Date
import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

object RichCustomTypeMode
  extends RichCustomTypeMode

trait RichCustomTypeMode
  extends PrimitiveTypeMode {


  //  implicit val operationTypeTEF: NonPrimitiveJdbcMapper[Int, OperationType.Value, TInt] = {
  //    new NonPrimitiveJdbcMapper[Int, OperationType.Value, TInt](intTEF, this) {
  //      override def sample: OperationType.Value = OperationType.BUY
  //
  //      def convertFromJdbc(v: Int): OperationType.Value = OperationType.apply(v)
  //
  //      def convertToJdbc(ld: OperationType.Value): Int = ld.id
  //    }
  //  }
  //
  //  implicit val orderStatusTEF: NonPrimitiveJdbcMapper[Int, OrderStatus.Value, TInt] = {
  //    new NonPrimitiveJdbcMapper[Int, OrderStatus.Value, TInt](intTEF, this) {
  //      override def sample: OrderStatus.Value = OrderStatus.NEW
  //
  //      def convertFromJdbc(v: Int): OrderStatus.Value = OrderStatus.apply(v)
  //
  //      def convertToJdbc(ld: OrderStatus.Value): Int = ld.id
  //    }
  //  }

//  implicit val candleIntervalTEF: NonPrimitiveJdbcMapper[String, CandleTimeFrame.Value, TString] = {
//    new NonPrimitiveJdbcMapper[String, CandleTimeFrame.Value, TString](stringTEF, this) {
//      override def sample: CandleTimeFrame.Value = CandleTimeFrame._1MIN
//
//      def convertFromJdbc(v: String): CandleTimeFrame.Value = CandleTimeFrame.fromCode(v)
//
//      def convertToJdbc(ld: CandleTimeFrame.Value): String = ld.toString
//    }
//  }

  implicit val dayOfWeekTEF: NonPrimitiveJdbcMapper[Int, DayOfWeek, TInt] = {
    new NonPrimitiveJdbcMapper[Int, DayOfWeek, TInt](intTEF, this) {
      def convertFromJdbc(d: Int): DayOfWeek = {
        if (d == 0) null else DateUtil.fromUtilCalendarWeek(d)
      }

      def convertToJdbc(ld: DayOfWeek): Int = {
        DateUtil.toUtilCalendarWeek(ld)
      }
    }
  }

  val optionDayOfWeekTEF: TypedExpressionFactory[Option[DayOfWeek], TOptionInt]
    with DeOptionizer[Int, DayOfWeek, TInt, Option[DayOfWeek], TOptionInt] = {
    new TypedExpressionFactory[Option[DayOfWeek], TOptionInt]
      with DeOptionizer[Int, DayOfWeek, TInt, Option[DayOfWeek], TOptionInt] {

      val deOptionizer: NonPrimitiveJdbcMapper[Int, DayOfWeek, TInt] = dayOfWeekTEF
    }
  }

  implicit val monthTEF: NonPrimitiveJdbcMapper[Int, Month, TInt] = {
    new NonPrimitiveJdbcMapper[Int, Month, TInt](intTEF, this) {
      def convertFromJdbc(d: Int): Month = {
        if (d == 0) null else Month.of(d)
      }

      def convertToJdbc(ld: Month): Int = {
        ld.getValue
      }
    }
  }

  val optionMonthTEF: TypedExpressionFactory[Option[Month], TOptionInt]
    with DeOptionizer[Int, Month, TInt, Option[Month], TOptionInt] = {
    new TypedExpressionFactory[Option[Month], TOptionInt]
      with DeOptionizer[Int, Month, TInt, Option[Month], TOptionInt] {

      val deOptionizer: NonPrimitiveJdbcMapper[Int, Month, TInt] = monthTEF
    }
  }

  implicit val monthDayTEF: NonPrimitiveJdbcMapper[String, MonthDay, TString] = {
    new NonPrimitiveJdbcMapper[String, MonthDay, TString](stringTEF, this) {
      override def sample: MonthDay = MonthDay.of(1, 1)

      override def defaultColumnLength: Int = 7

      def convertFromJdbc(t: String): MonthDay = {
        if (t == null || t.trim.isEmpty) {
          null
        } else {
          MonthDay.parse(t)
        }
      }

      def convertToJdbc(ld: MonthDay): String = {
        if (ld == null) null else ld.toString
      }
    }
  }

  val optionMonthDayTEF: TypedExpressionFactory[Option[MonthDay], TOptionString]
    with DeOptionizer[String, MonthDay, TString, Option[MonthDay], TOptionString] = {
    new TypedExpressionFactory[Option[MonthDay], TOptionString]
      with DeOptionizer[String, MonthDay, TString, Option[MonthDay], TOptionString] {

      val deOptionizer: NonPrimitiveJdbcMapper[String, MonthDay, TString] = monthDayTEF
    }
  }

  implicit val localDateTEF: NonPrimitiveJdbcMapper[Date, LocalDate, TDate] = {
    new NonPrimitiveJdbcMapper[java.util.Date, LocalDate, TDate](dateTEF, this) {
      def convertFromJdbc(d: java.util.Date): LocalDate = {
        if (d == null) null else Instant.ofEpochMilli(d.getTime).atZone(ZoneId.systemDefault()).toLocalDate
      }

      def convertToJdbc(ld: LocalDate): Date = {
        if (ld == null || ld == LocalDate.MIN) {
          null
        } else {
          new java.util.Date(ld.atStartOfDay(ZoneId.systemDefault()).toInstant.toEpochMilli)
        }
      }
    }
  }

  implicit val optionLocalDateTEF: TypedExpressionFactory[Option[LocalDate], TOptionDate]
    with DeOptionizer[Date, LocalDate, TDate, Option[LocalDate], TOptionDate] = {
    new TypedExpressionFactory[Option[LocalDate], TOptionDate]
      with DeOptionizer[java.util.Date, LocalDate, TDate, Option[LocalDate], TOptionDate] {

      val deOptionizer: NonPrimitiveJdbcMapper[Date, LocalDate, TDate] = localDateTEF
    }
  }

  implicit val zonedDateTimeTEF: NonPrimitiveJdbcMapper[Timestamp, ZonedDateTime, TTimestamp] = {
    new NonPrimitiveJdbcMapper[Timestamp, ZonedDateTime, TTimestamp](timestampTEF, this) {
      def convertFromJdbc(t: Timestamp): ZonedDateTime = {
        if (t == null) null else Instant.ofEpochMilli(t.getTime).atZone(ZoneId.systemDefault())
      }

      def convertToJdbc(zdt: ZonedDateTime): Timestamp = {
        if (zdt == null) null else Timestamp.valueOf(zdt.withZoneSameInstant(ZoneId.systemDefault()).toLocalDateTime.withNano(0))
      }
    }
  }

  implicit val optionZonedDateTimeTEF: TypedExpressionFactory[Option[ZonedDateTime], TOptionTimestamp]
    with DeOptionizer[Timestamp, ZonedDateTime, TTimestamp, Option[ZonedDateTime], TOptionTimestamp] = {
    new TypedExpressionFactory[Option[ZonedDateTime], TOptionTimestamp]
      with DeOptionizer[Timestamp, ZonedDateTime, TTimestamp, Option[ZonedDateTime], TOptionTimestamp] {

      val deOptionizer: NonPrimitiveJdbcMapper[Timestamp, ZonedDateTime, TTimestamp] = zonedDateTimeTEF
    }
  }

  implicit val localTimeTEF: NonPrimitiveJdbcMapper[Timestamp, LocalTime, TTimestamp] = {
    new NonPrimitiveJdbcMapper[Timestamp, LocalTime, TTimestamp](timestampTEF, this) {
      def convertFromJdbc(t: Timestamp): LocalTime = {
        if (t == null) null else Instant.ofEpochMilli(t.getTime).atZone(ZoneId.systemDefault()).toLocalTime
      }

      def convertToJdbc(t: LocalTime): Timestamp = {
        if (t == null) null else Timestamp.valueOf(t.atDate(LocalDate.of(1970, 1, 1)))
      }
    }
  }

  implicit val optionLocalTimeTEF: TypedExpressionFactory[Option[LocalTime], TOptionTimestamp]
    with DeOptionizer[Timestamp, LocalTime, TTimestamp, Option[LocalTime], TOptionTimestamp] = {
    new TypedExpressionFactory[Option[LocalTime], TOptionTimestamp]
      with DeOptionizer[Timestamp, LocalTime, TTimestamp, Option[LocalTime], TOptionTimestamp] {

      val deOptionizer: NonPrimitiveJdbcMapper[Timestamp, LocalTime, TTimestamp] = localTimeTEF
    }
  }

  implicit val localDateTimeTEF: NonPrimitiveJdbcMapper[Timestamp, LocalDateTime, TTimestamp] = {
    new NonPrimitiveJdbcMapper[Timestamp, LocalDateTime, TTimestamp](timestampTEF, this) {
      def convertFromJdbc(t: Timestamp): LocalDateTime = {
        if (t == null) null else Instant.ofEpochMilli(t.getTime).atZone(ZoneId.systemDefault()).toLocalDateTime.withNano(0)
      }

      def convertToJdbc(ld: LocalDateTime): Timestamp = {
        if (ld == null) null else Timestamp.valueOf(ld.withNano(0))
      }
    }
  }

  implicit val optionLocalDateTimeTEF: TypedExpressionFactory[Option[LocalDateTime], TOptionTimestamp]
    with DeOptionizer[Timestamp, LocalDateTime, TTimestamp, Option[LocalDateTime], TOptionTimestamp] = {
    new TypedExpressionFactory[Option[LocalDateTime], TOptionTimestamp]
      with DeOptionizer[Timestamp, LocalDateTime, TTimestamp, Option[LocalDateTime], TOptionTimestamp] {

      val deOptionizer: NonPrimitiveJdbcMapper[Timestamp, LocalDateTime, TTimestamp] = localDateTimeTEF
    }
  }

  implicit val jsObjectTEF: NonPrimitiveJdbcMapper[String, JsObject, TString] = {
    new NonPrimitiveJdbcMapper[String, JsObject, TString](stringTEF, this) {
      def convertFromJdbc(t: String): JsObject = {
        if (t == null || t.trim.isEmpty) {
          Json.obj()
        } else {
          val jsValue = Try(Json.parse(t.trim)) match {
            case Success(v) => v
            case Failure(_) =>
              // try to decode string
              Try(Json.parse(java.net.URLDecoder.decode(t.trim, "UTF-8"))) match {
                case Success(v) => v
                case Failure(_) =>
                  throw new IllegalArgumentException(s"Can't parse value '$t' as json object!")
              }
          }

          Try(jsValue.as[JsObject]).getOrElse {
            throw new IllegalArgumentException(s"Can't parse value '$t' as JsObject!")
          }
        }
      }

      def convertToJdbc(ld: JsObject): String = {
        if (ld == null) null else ld.toString()
      }
    }
  }

  private val optionJsObjectTEF: TypedExpressionFactory[Option[JsObject], TOptionString]
    with DeOptionizer[String, JsObject, TString, Option[JsObject], TOptionString] = {
    new TypedExpressionFactory[Option[JsObject], TOptionString]
      with DeOptionizer[String, JsObject, TString, Option[JsObject], TOptionString] {

      val deOptionizer: NonPrimitiveJdbcMapper[String, JsObject, TString] = jsObjectTEF
    }
  }

  implicit val jsArrayTEF: NonPrimitiveJdbcMapper[String, JsArray, TString] = {
    new NonPrimitiveJdbcMapper[String, JsArray, TString](stringTEF, this) {
      def convertFromJdbc(t: String): JsArray = {
        if (t == null || t.trim.isEmpty) {
          JsArray()
        } else {
          val jsValue = Try(Json.parse(t.trim)) match {
            case Success(v) => v
            case Failure(_) =>
              // try to decode string
              Try(Json.parse(java.net.URLDecoder.decode(t.trim, "UTF-8"))) match {
                case Success(v) => v
                case Failure(_) =>
                  throw new IllegalArgumentException(s"Can't parse value '$t' as json object!")
              }
          }

          Try(jsValue.as[JsArray]).getOrElse {
            throw new IllegalArgumentException(s"Can't parse value '$t' as JsArray!")
          }
        }
      }

      def convertToJdbc(ld: JsArray): String = {
        if (ld == null) null else ld.toString()
      }
    }
  }

  implicit val booleanTEFImplicit: TypedExpressionFactory[Boolean, TBoolean] = PrimitiveTypeSupport.booleanTEF

  private val optionJsArrayTEF: TypedExpressionFactory[Option[JsArray], TOptionString]
    with DeOptionizer[String, JsArray, TString, Option[JsArray], TOptionString] = {
    new TypedExpressionFactory[Option[JsArray], TOptionString]
      with DeOptionizer[String, JsArray, TString, Option[JsArray], TOptionString] {

      val deOptionizer: NonPrimitiveJdbcMapper[String, JsArray, TString] = jsArrayTEF
    }
  }

  //  implicit def operationTypeToTE(s: OperationType.Value): TypedExpression[OperationType.Value, TInt] = operationTypeTEF.create(s)
  //
  //  implicit def orderStatusToTE(s: OrderStatus.Value): TypedExpression[OrderStatus.Value, TInt] = orderStatusTEF.create(s)

  implicit def dayOfWeekToTE(s: DayOfWeek): TypedExpression[DayOfWeek, TInt] = dayOfWeekTEF.create(s)

  implicit def optionDayOfWeekToTE(s: Option[DayOfWeek]): TypedExpression[Option[DayOfWeek], TOptionInt] = {
    optionDayOfWeekTEF.create(s)
  }

  implicit def monthToTE(s: Month): TypedExpression[Month, TInt] = monthTEF.create(s)

  implicit def optionMonthToTE(s: Option[Month]): TypedExpression[Option[Month], TOptionInt] = {
    optionMonthTEF.create(s)
  }

  implicit def monthDayToTE(s: MonthDay): TypedExpression[MonthDay, TString] = monthDayTEF.create(s)

  implicit def optionMonthDayToTE(s: Option[MonthDay]): TypedExpression[Option[MonthDay], TOptionString] = {
    optionMonthDayTEF.create(s)
  }

  implicit def localTimeToTE(s: LocalTime): TypedExpression[LocalTime, TTimestamp] = localTimeTEF.create(s)

  implicit def optionLocalTimeToTE(s: Option[LocalTime]): TypedExpression[Option[LocalTime], TOptionTimestamp] = {
    optionLocalTimeTEF.create(s)
  }

  implicit def zonedDateTimeToTE(s: ZonedDateTime): TypedExpression[ZonedDateTime, TTimestamp] = zonedDateTimeTEF.create(s)

  implicit def optionZonedDateTimeToTE(s: Option[ZonedDateTime]): TypedExpression[Option[ZonedDateTime], TOptionTimestamp] = {
    optionZonedDateTimeTEF.create(s)
  }

  implicit def localDateTimeToTE(s: LocalDateTime): TypedExpression[LocalDateTime, TTimestamp] = localDateTimeTEF.create(s)

  implicit def optionLocalDateTimeToTE(s: Option[LocalDateTime]): TypedExpression[Option[LocalDateTime], TOptionTimestamp] = {
    optionLocalDateTimeTEF.create(s)
  }

  implicit def localDateToTE(s: LocalDate): TypedExpression[LocalDate, TDate] = localDateTEF.create(s)

  implicit def optionLocalDateToTE(s: Option[LocalDate]): TypedExpression[Option[LocalDate], TOptionDate] = {
    optionLocalDateTEF.create(s)
  }

  implicit def jsObjectToTE(s: JsObject): TypedExpression[JsObject, TString] = jsObjectTEF.create(s)

  implicit def optionJsObjectToTE(s: Option[JsObject]): TypedExpression[Option[JsObject], TOptionString] = {
    optionJsObjectTEF.create(s)
  }

  implicit def jsArrayToTE(s: JsArray): TypedExpression[JsArray, TString] = jsArrayTEF.create(s)

  implicit def optionJsArrayToTE(s: Option[JsArray]): TypedExpression[Option[JsArray], TOptionString] = {
    optionJsArrayTEF.create(s)
  }

  implicit def concatArrayBytes[A1, A2, T1, T2](co: ConcatOp[Option[A1], Option[A2], T1, T2]): TypedExpression[Option[Array[Byte]], TOptionByteArray] = {
    new ConcatOperationNode[Option[Array[Byte]], TOptionByteArray](co.a1, co.a2, PrimitiveTypeSupport.optionByteArrayTEF.createOutMapper)
  }

  def sumDouble(d: TypedExpression[Double, TDouble]): TypedExpression[Option[Double], TOptionDouble] = sum(d)

  def maxDate(d: TypedExpression[LocalDate, TDate]): TypedExpression[Option[LocalDate], TOptionDate] = max(d)

  def maxDateOpt(d: TypedExpression[Option[LocalDate], TOptionDate]): TypedExpression[Option[LocalDate], TOptionDate] = max(d)

}
