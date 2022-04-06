package com.balakhonov.invest.util

import java.time._
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit
import java.util.{Calendar, TimeZone}

object DateUtil {

  implicit val localTimeOrdering: Ordering[LocalTime] = Ordering.by(_.toNanoOfDay)

  implicit def stringToLocalDate(s: String): LocalDate = DateTimeFormat.parseLocalDate(s)

  implicit def stringToLocalDateFormat(s: String, f: String): LocalDate = DateTimeFormat.parseLocalDate(s, f)

  implicit def stringToZonedDateTime(s: String): ZonedDateTime = DateTimeFormat.parseZonedDateTime(s)

  implicit def stringToLocalTime(s: String): LocalTime = DateTimeFormat.parseLocalTime(s)

  implicit def stringToLocalDateTime(s: String): LocalDateTime = DateTimeFormat.parseLocalDateTime(s)

  implicit def localDateExtra(localDate: LocalDate): LocalDateExtra = new LocalDateExtra(localDate)

  def utcToZonedDateTime(timeZone: TimeZone, dateTime: ZonedDateTime): ZonedDateTime = {
    val instant = dateTime.withZoneSameInstant(ZoneOffset.UTC)

    instant.withZoneSameInstant(timeZone.toZoneId)
  }

  def localDateTimeToUtc(timeZone: TimeZone, datetime: LocalDateTime): ZonedDateTime = {
    val localDateTimeInTZ = datetime.atZone(timeZone.toZoneId)
    val localDateTimeInUTC = localDateTimeInTZ.withZoneSameInstant(ZoneOffset.UTC)

    localDateTimeInUTC
  }

  def daysDiff(startDate: LocalDate, endDate: LocalDate): Int = {
    ChronoUnit.DAYS.between(startDate, endDate).toInt
  }

  def daysDiff(startDate: LocalDateTime, endDate: LocalDateTime): Int = {
    ChronoUnit.DAYS.between(startDate, endDate).toInt
  }

  def _max(d1: LocalDate,
           d2: LocalDate): LocalDate = {
    if (d1.isAfter(d2)) d1 else d2
  }

  def _min(d1: LocalDate,
           d2: LocalDate): LocalDate = {
    if (d1.isAfter(d2)) d2 else d1
  }

  def _min(d1: LocalDateTime,
           d2: LocalDateTime): LocalDateTime = {
    if (d1.isAfter(d2)) d2 else d1
  }

  def defineLastDayOfWeek(startDayOfWeek: DayOfWeek): DayOfWeek = {
    startDayOfWeek match {
      case DayOfWeek.SUNDAY => DayOfWeek.SATURDAY
      case DayOfWeek.MONDAY => DayOfWeek.SUNDAY
      case DayOfWeek.TUESDAY => DayOfWeek.MONDAY
      case DayOfWeek.WEDNESDAY => DayOfWeek.TUESDAY
      case DayOfWeek.THURSDAY => DayOfWeek.WEDNESDAY
      case DayOfWeek.FRIDAY => DayOfWeek.THURSDAY
      case DayOfWeek.SATURDAY => DayOfWeek.FRIDAY
    }
  }

  def fromUtilCalendarWeek(w: Int): DayOfWeek = {
    w match {
      case Calendar.SUNDAY => DayOfWeek.SUNDAY
      case Calendar.MONDAY => DayOfWeek.MONDAY
      case Calendar.TUESDAY => DayOfWeek.TUESDAY
      case Calendar.WEDNESDAY => DayOfWeek.WEDNESDAY
      case Calendar.THURSDAY => DayOfWeek.THURSDAY
      case Calendar.FRIDAY => DayOfWeek.FRIDAY
      case Calendar.SATURDAY => DayOfWeek.SATURDAY
      case _ => throw new IllegalStateException(s"Unknown com.balakhonov.invest.util.Calendar week($w)!")
    }
  }

  def toUtilCalendarWeek(w: DayOfWeek): Int = {
    w match {
      case DayOfWeek.SUNDAY => Calendar.SUNDAY
      case DayOfWeek.MONDAY => Calendar.MONDAY
      case DayOfWeek.TUESDAY => Calendar.TUESDAY
      case DayOfWeek.WEDNESDAY => Calendar.WEDNESDAY
      case DayOfWeek.THURSDAY => Calendar.THURSDAY
      case DayOfWeek.FRIDAY => Calendar.FRIDAY
      case DayOfWeek.SATURDAY => Calendar.SATURDAY
    }
  }

  def calcDuration(startTime: LocalTime, endTime: LocalTime): (Int, Int, Double) = {
    val durationMinutes: Double = ChronoUnit.MINUTES.between(startTime, endTime).toDouble

    val hours = ((durationMinutes - durationMinutes % 60) / 60).toInt
    val minutes = (durationMinutes % 60).toInt
    (hours, minutes, durationMinutes)
  }

  def calcDuration(startTime: LocalTime, endTimeOpt: Option[LocalTime]): (Int, Int, Double) = {
    endTimeOpt.fold {
      (0, 0, 0d)
    } { endTime =>
      calcDuration(startTime: LocalTime, endTime: LocalTime)
    }
  }

  def roundMinutes(roundValue: Int, totalMinutes: Int): Int = {
    val totalMinutesRemainder = totalMinutes % 60

    val adjustedMinutes = roundValue match {
      case 0 =>
        totalMinutesRemainder

      case _ =>
        val minutesRemainder = totalMinutesRemainder % roundValue
        val minMinutes = totalMinutesRemainder - minutesRemainder

        val halfOfRoundValue = roundValue / 2.0

        if (minutesRemainder > halfOfRoundValue) {
          minMinutes + roundValue
        } else {
          minMinutes
        }
    }

    val adjustedTotalMinutes = totalMinutes - totalMinutesRemainder + adjustedMinutes

    adjustedTotalMinutes
  }

  def firstDayOfYear(year: Int): LocalDate = LocalDate.of(year, Month.JANUARY, 1)

  def lastDayOfYear(year: Int): LocalDate = LocalDate.of(year, Month.DECEMBER, 31)

  def firstDayOfMonth(date: LocalDate): LocalDate = date.withDayOfMonth(1)

  def lastDayOfMonth(date: LocalDate): LocalDate = date.withDayOfMonth(date.getMonth.length(date.isLeapYear))

  def least(a: LocalTime, b: LocalTime): LocalTime = {
    if (a.isBefore(b)) a else b
  }

  def least(a: LocalDateTime, b: LocalDateTime): LocalDateTime = {
    if (a.isBefore(b)) a else b
  }

  def greater(a: LocalTime, b: LocalTime): LocalTime = {
    if (a.isAfter(b)) a else b
  }

  def greater(a: LocalDateTime, b: LocalDateTime): LocalDateTime = {
    if (a.isAfter(b)) a else b
  }

  def convertDateTimeWithTimeZone(dateTime: LocalDateTime,
                                  fromTimeZone: TimeZone,
                                  toTimeZone: TimeZone): LocalDateTime = {
    val delta = ChronoUnit.MINUTES.between(dateTime.atZone(toTimeZone.toZoneId), dateTime.atZone(fromTimeZone.toZoneId))

    dateTime.plusMinutes(delta)
  }

  def isDurationInHours(minutes: Int): Boolean = {
    minutes % 60 == 0
  }

  def defineYearEnd(yearEndDate: String, yearEndYear: Int): LocalDate =
    LocalDate.parse(s"$yearEndDate-$yearEndYear", DateTimeFormatter.ofPattern("MM-dd-yyyy"))

  def prevYearAdjustedDate(currentDate: LocalDate): LocalDate = {
    currentDate.plusDays(1).minusYears(1)
  }

  def prevYearAdjustedDateTime(currentDateTime: LocalDateTime): LocalDateTime = {
    currentDateTime.plusDays(1).minusYears(1)
  }

}