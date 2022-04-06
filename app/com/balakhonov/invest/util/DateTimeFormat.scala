package com.balakhonov.invest.util

import java.time._
import java.time.format.{DateTimeFormatter, DateTimeParseException}
import java.util.TimeZone
import scala.math.BigDecimal.RoundingMode
import scala.util.Try

trait DateTimeFormat {

  def parseZonedDateTime(dateTimeStr: String): ZonedDateTime

  def parseLocalDateTime(dateTimeStr: String): LocalDateTime

  def parseLocalDate(dateStr: String): LocalDate
}

object DateTimeFormat extends DateTimeFormat {

  implicit val defaultDateTimeFormatter: DateTimeFormat = DateTimeFormat

  TimeZone.setDefault(TimeZone.getTimeZone("GMT"))
  System.setProperty("user.timezone", "GMT")

  private val DATE_FORMAT = "yyyy.MM.dd"
  private val DATE_DASHED_FORMAT = "yyyy-MM-dd"
  private val ZONED_DATE_TIME_PARSE = "yyyy-MM-dd'T'HH:mm:ss[XXX][X]"
  private val ZONED_DATE_TIME_FORMAT = "yyyy-MM-dd'T'HH:mm:ssZ"
  private val ZONED_DATE_TIME_FORMAT_SHORT = "MM/dd/yyyy HH:mm z"
  private val LOCAL_DATE_TIME_FORMAT = "yyyy-MM-dd HH:mm:ss"
  private val LOCAL_DATE_NOTIFICATION_FORMAT = "MM/dd/yyyy"
  private val SHORT_DATE_FORMAT = "M/d/yy"
  private val LOCAL_DATE_TIME_NOTIFICATION_FORMAT = "MM/dd/yyyy hh:mm a"
  private val LOCAL_TIME_PARSE_FORMAT = "H:m"
  private val LOCAL_TIME_FORMAT = "HH:mm"
  private val HOURS_FORMAT = "H"

  private val localDateFormat = DateTimeFormatter.ofPattern(DATE_FORMAT)
  private val localDateDashedFormat = DateTimeFormatter.ofPattern(DATE_DASHED_FORMAT)
  private val zonedDateTimeParseFormat = DateTimeFormatter.ofPattern(ZONED_DATE_TIME_PARSE)
  private val zonedDateTimeFormat = DateTimeFormatter.ofPattern(ZONED_DATE_TIME_FORMAT)
  private val zonedDateTimeFormatShort = DateTimeFormatter.ofPattern(ZONED_DATE_TIME_FORMAT_SHORT)
  private val localDateTimeFormat = DateTimeFormatter.ofPattern(LOCAL_DATE_TIME_FORMAT)
  private val localDateNotificationFormat = DateTimeFormatter.ofPattern(LOCAL_DATE_NOTIFICATION_FORMAT)
  private val shortLocalDateFormat = DateTimeFormatter.ofPattern(SHORT_DATE_FORMAT)
  private val localDateTimeNotificationFormat = DateTimeFormatter.ofPattern(LOCAL_DATE_TIME_NOTIFICATION_FORMAT)
  private val localTimeParseFormat = DateTimeFormatter.ofPattern(LOCAL_TIME_PARSE_FORMAT)
  private val localTimeFormat = DateTimeFormatter.ofPattern(LOCAL_TIME_FORMAT)
  private val hoursFormat = DateTimeFormatter.ofPattern(HOURS_FORMAT)

  def formatLocalDate(localDate: LocalDate): String = {
    localDate.format(localDateFormat)
  }

  def formatLocalDate(localDate: LocalDate,
                      format: DateTimeFormatter): String = {
    localDate.format(format)
  }

  def formatLocalDateDashed(localDate: LocalDate): String = {
    localDate.format(localDateDashedFormat)
  }

  def formatZonedDateTime(dateTime: ZonedDateTime): String = {
    zonedDateTimeFormat.format(dateTime)
  }

  def formatZonedDateTimeShort(dateTime: ZonedDateTime): String = {
    zonedDateTimeFormatShort.format(dateTime)
  }

  /**
   * Return the string representation of ZonedDateTime.
   *
   * @param zdt
   * @return Raw string date time with the applied current user's time zone
   */
  def displayZonedDateTime(zdt: ZonedDateTime, zoneId: ZoneId): String = {
    //    val zoneId = LoggedInRequest.currentRequest.fold(ZoneId.systemDefault())(_.performer.zoneId)
    val ldt = zdt.withZoneSameInstant(zoneId).toLocalDateTime

    localDateTimeFormat.format(ldt)
  }

  def formatLocalDateTime(dateTime: LocalDateTime): String = {
    localDateTimeFormat.format(dateTime)
  }

  def formatNotificationLocalDate(dateTime: LocalDate): String = {
    localDateNotificationFormat.format(dateTime)
  }

  def formatNotificationLocalDateTime(dateTime: LocalDateTime): String = {
    localDateTimeNotificationFormat.format(dateTime)
  }

  def formatLocalTime(time: LocalTime): String = {
    localTimeFormat.format(time)
  }

  override def parseLocalDate(dateStr: String): LocalDate = {
    Try {
      LocalDate.parse(dateStr, localDateFormat)
    }.getOrElse {
      throw new IllegalStateException(s"Value($dateStr) can't be parse as LocalDate($DATE_FORMAT) format!")
    }
  }

  def isLocalDate(dateStr: String): Boolean = Try(LocalDate.parse(dateStr, localDateFormat)).isSuccess

  def tryParseLocalDate(dateStr: String, dateFormat: String): Option[LocalDate] = {
    try {
      Some(LocalDate.parse(dateStr, DateTimeFormatter.ofPattern(dateFormat)))
    } catch {
      case _: DateTimeParseException => None
    }
  }

  def parseLocalDate(dateStr: String, dateFormat: String): LocalDate = {
    Try {
      LocalDate.parse(dateStr, DateTimeFormatter.ofPattern(dateFormat))
    }.getOrElse {
      throw new IllegalStateException(s"Value($dateStr) can't be parse as LocalDate($dateFormat) format!")
    }
  }

  def parseLocalDate(dateStr: String, dateFormat: Seq[String]): LocalDate = {
    dateFormat match {
      case head :: tail =>
        Try {
          LocalDate.parse(dateStr, DateTimeFormatter.ofPattern(head))
        }.getOrElse {
          parseLocalDate(dateStr, tail)
        }

      case _ =>
        throw new IllegalStateException(s"Value($dateStr) can't be parse as LocalDate format!")
    }
  }

  override def parseZonedDateTime(dateStr: String): ZonedDateTime = {
    Try {
      val str = dateStr.replaceAll(".000Z", "+0000").replaceAll("Z", "+0000")
      ZonedDateTime.parse(str, zonedDateTimeParseFormat)
    }.getOrElse {
      throw new IllegalStateException(s"Value($dateStr) can't be parse as ZonedDateTime($ZONED_DATE_TIME_FORMAT) format!")
    }
  }

  override def parseLocalDateTime(localDateTimeStr: String): LocalDateTime = {
    Try {
      LocalDateTime.parse(localDateTimeStr, localDateTimeFormat)
    }.getOrElse {
      throw new IllegalStateException(s"Value($localDateTimeStr) can't be parse as LocalDateTime($LOCAL_DATE_TIME_FORMAT) format!")
    }
  }

  def parseAnyDateFormat(dateStr: String): LocalDate = {
    Try(LocalDate.parse(dateStr, localDateFormat))
      .orElse(Try(LocalDate.parse(dateStr, localDateNotificationFormat)))
      .orElse(Try(LocalDate.parse(dateStr, localDateDashedFormat)))
      .orElse(Try(LocalDate.parse(dateStr, shortLocalDateFormat)))
      .orElse(Try(LocalDate.parse(dateStr, localDateTimeFormat)))
      .getOrElse {
        throw new IllegalStateException(s"Value($dateStr) can't be parse as LocalDate($DATE_FORMAT) format!")
      }
  }

  def parseLocalTime(dateStr: String): LocalTime = {
    Try(LocalTime.parse(dateStr, localTimeParseFormat))
      .orElse(Try(LocalTime.parse(dateStr, hoursFormat)))
      .orElse(Try(parseLocalDateTime(dateStr).toLocalTime))
      .orElse(Try(parseZonedDateTime(dateStr).toLocalTime))
      .orElse(Try(parseLocalTimeFromDouble(dateStr.toDouble)))
      .getOrElse {
        throw new IllegalStateException(s"Value($dateStr) can't be parse as LocalTime($LOCAL_TIME_FORMAT) format!")
      }
  }

  def convertStringDateDashedToNotification(value: String): String = {
    Try(formatNotificationLocalDate(LocalDate.parse(value, localDateDashedFormat))).getOrElse(value)
  }

  def parseLocalTimeFromDouble(hours: Double): LocalTime = {
    val minutes = Math.round(hours * 60).toInt
    LocalTime.of(minutes / 60, minutes % 60, 0, 0)
  }

  def timeFormat(hours: Double): String = {
    val minutes = Math.round(hours * 60).toInt
    timeFormatFromMinutes(minutes)
  }

  def timeFormat(hours: BigDecimal): String = {
    val minutes = (hours * 60).setScale(0, RoundingMode.UP).toInt
    timeFormatFromMinutes(minutes)
  }

  def timeFormatFromMinutes(totalMinutes: Int): String = {
    val m = FormatUtil.format2D(Math.abs(totalMinutes % 60))
    val h = FormatUtil.format2D(totalMinutes / 60)

    s"$h:$m"
  }

}