package com.balakhonov.invest.util

import java.time._
import java.time.temporal.{ChronoUnit, WeekFields}

final class LocalDateExtra(val date: LocalDate) extends AnyVal {

  import java.time.temporal.TemporalAdjusters._

  /**
   * Converts this instant to the number of milliseconds
   */
  def getMillis: Long = date.atStartOfDay(ZoneId.systemDefault()).toInstant.toEpochMilli

  def weekOfYear(weekFields: WeekFields = WeekFields.ISO): Int = {
    date.get(weekFields.weekOfWeekBasedYear())
  }

  def atStartOfMonth: LocalDate = {
    date.`with`(firstDayOfMonth())
  }

  def atEndOfMonth: LocalDate = {
    date.`with`(lastDayOfMonth())
  }

  def atStartOfYear: LocalDate = {
    date.`with`(firstDayOfYear())
  }

  def atEndOfYear: LocalDate = {
    date.`with`(lastDayOfYear())
  }

  def atStartWeekDate(firstDayOfWeek: DayOfWeek): LocalDate = {
    var _date = date
    while (_date.getDayOfWeek != firstDayOfWeek) {
      _date = _date.plusDays(-1)
    }

    _date
  }

  def atEndWeekDate(lastDayOfWeek: DayOfWeek): LocalDate = {
    var _date = date
    val _lastDayOfWeek = DateUtil.defineLastDayOfWeek(lastDayOfWeek)

    while (_date.getDayOfWeek != _lastDayOfWeek) {
      _date = _date.plusDays(1)
    }

    _date
  }

  def atEndOfDay: LocalDateTime = LocalDateTime.of(date, LocalTime.MAX)

  def isBeforeOrEquals(other: LocalDate): Boolean = date.isBefore(other) || date.isEqual(other)

  def isAfterOrEquals(other: LocalDate): Boolean = date.isAfter(other) || date.isEqual(other)

  def isNotEquals(other: LocalDate): Boolean = !date.isEqual(other)

  def min(other: LocalDate): LocalDate = if (other.isBefore(date)) other else date

  /**
   * @return is between [StartDate, EndDate]
   */
  def isBetween(startDate: LocalDate, endDate: LocalDate): Boolean = {
    isAfterOrEquals(startDate) && isBeforeOrEquals(endDate)
  }

  def isInRange(startDate: LocalDate, endDate: Option[LocalDate]): Boolean = {
    isAfterOrEquals(startDate) && endDate.forall(isBeforeOrEquals)
  }

  /**
   * Week number since 01.01.1970
   *
   * @return
   */
  def weekNumber: Int = {
    val firstDayOfWeek: DayOfWeek = DayOfWeek.MONDAY

    val diff = ChronoUnit.DAYS.between(LocalDate.of(1970, 1, 1), date).toInt

    val shift = firstDayOfWeek match {
      case DayOfWeek.MONDAY => 3
      case DayOfWeek.TUESDAY => 2
      case DayOfWeek.WEDNESDAY => 1
      case DayOfWeek.THURSDAY => 0
      case DayOfWeek.FRIDAY => -1
      case DayOfWeek.SATURDAY => -2
      case DayOfWeek.SUNDAY => -3
    }

    (diff + 7 + shift) / 7
  }

  def isOddWeek: Boolean = weekNumber % 2 != 0

  def to(until: LocalDate): Seq[LocalDate] = {
    val numberOfDays: Long = ChronoUnit.DAYS.between(date, until)
    for (day <- 0 to numberOfDays.toInt) yield date.plusDays(day)
  }

  def atFirstDayOfQuarter: LocalDate = {
    atStartOfMonth.plusMonths(-((date.getMonthValue - 1) % 3))
  }

  def atFirstDayOfSubsequentQuarter: LocalDate = {
    atStartOfMonth.plusMonths(-((date.getMonthValue - 1) % 3) + 3)
  }

  def atTime(time: LocalTime, zoneId: ZoneId): ZonedDateTime =
    date.atTime(time).atZone(zoneId)

  def previousDay: LocalDate = date.minusDays(1)
}