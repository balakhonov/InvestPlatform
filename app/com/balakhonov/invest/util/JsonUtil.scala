package com.balakhonov.invest.util

import com.balakhonov.invest.util.DateTimeFormat._
import play.api.libs.json.{JsString, _}

import java.time._

object JsonUtil {

  implicit class DayOfWeekToJson(dayOfWeek: DayOfWeek) {
    def toJson: JsValue = JsNumber(DateUtil.toUtilCalendarWeek(dayOfWeek))
  }

  implicit class MonthToJson(month: Month) {
    def toJson: JsValue = JsNumber(month.getValue)
  }

  implicit class MonthDayToJson(monthDay: MonthDay) {
    def toJson: JsValue = JsString(stringify)

    def stringify: String = new StringBuilder(10)
      .append(if (monthDay.getMonthValue < 10) "0" else "")
      .append(monthDay.getMonthValue)
      .append("-")
      .append(if (monthDay.getDayOfMonth < 10) "0" else "")
      .append(monthDay.getDayOfMonth)
      .toString
  }

  implicit class LocalTimeToJson(date: LocalTime) {
    def toJson: JsValue = JsString(formatLocalTime(date))
  }

  implicit class LocalDateTimeToJson(date: LocalDateTime) {
    def toJson: JsValue = JsString(formatLocalDateTime(date))
  }

  implicit class ZonedDateTimeToJson(date: ZonedDateTime) {
    def toJson: JsValue = JsString(formatZonedDateTime(date))
  }

  implicit class LocalDateToJson(date: java.time.LocalDate) {
    def toJson: JsValue = JsString(formatLocalDate(date))
  }

  implicit class ByteArrayToJson(blob: Array[Byte]) {
    def toJson: JsValue = JsString(BlobUtil.encode(blob))
  }

  implicit class OptionalMonthToJson(month: Option[Month]) {
    def toJson: JsValue = month match {
      case None => JsNull
      case Some(month: Month) => JsNumber(month.getValue)
    }
  }

  implicit class OptionalMonthDayToJson(month: Option[MonthDay]) {
    def toJson: JsValue = month match {
      case None => JsNull
      case Some(monthDay: MonthDay) => JsString(MonthDayToJson(monthDay).stringify)
    }
  }

  implicit class OptionalLocalDateToJson(date: Option[LocalDate]) {
    def toJson: JsValue = date match {
      case None => JsNull
      case Some(date: LocalDate) => JsString(formatLocalDate(date))
    }
  }

  implicit class OptionalLocalTimeToJson(date: Option[LocalTime]) {
    def toJson: JsValue = date.map(formatLocalTime).map(JsString).getOrElse(JsNull)
  }

  implicit class OptionalZonedDateTimeToJson(date: Option[ZonedDateTime]) {
    def toJson: JsValue = date.map(formatZonedDateTime).map(JsString).getOrElse(JsNull)
  }

  implicit class OptionalLocalDateTimeToJson(v: Option[LocalDateTime]) {
    def toJson: JsValue = v match {
      case None => JsNull
      case Some(timestamp) => JsString(formatLocalDateTime(timestamp))
    }
  }

  implicit class RichJsObject(body: JsObject) {
    def appendOpt[T](name: String, valueOpt: Option[T])(implicit w: Writes[T]): JsObject = {
      valueOpt.fold(body) { value =>
        body ++ Json.obj(name -> Json.toJsFieldJsValueWrapper(value))
      }
    }
  }

}