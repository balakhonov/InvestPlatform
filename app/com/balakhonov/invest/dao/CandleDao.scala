package com.balakhonov.invest.dao

import com.balakhonov.invest.models.db.Candle
import com.balakhonov.invest.models.enums.CandleTimeFrame
import com.balakhonov.invest.models.enums.CandleTimeFrame.CandleTimeFrame
import org.squeryl.customtypes.RichCustomTypeMode._

import java.time.{LocalDate, LocalDateTime}
import javax.inject.{Inject, Singleton}

@Singleton
case class CandleDao @Inject()(db: DBLibrary) {

  import db._

  def list(): List[Candle] = {
    inTransaction {
      from(candle)(c =>
        select(c)
      ).toList
    }
  }

  def list(figi: String,
           periodStart: LocalDateTime,
           periodEnd: LocalDateTime,
           interval: CandleTimeFrame): List[Candle] = {
    inTransaction {
      from(candle)(c =>
        where(
          c.figi === figi
            and c.dateTime >= periodStart
            and c.dateTime <= periodEnd
            and c.interval === interval
        )
          .select(c)
          .orderBy(c.dateTime)
      ).toList
    }
  }

  def listDaily(figi: String,
                year: Int,
                month: Int,
                from: Int,
                to: Int,
                interval: CandleTimeFrame.Value): List[Candle] = {
    val fromDate = LocalDate.of(year, month, from)
    val toDate = LocalDate.of(year, month, to)

    list(figi, fromDate.atTime(0, 0), toDate.atTime(23, 59), interval)
  }

  def listMonthly(figi: String,
                  year: Int,
                  month: Int,
                  interval: CandleTimeFrame.Value): List[Candle] = {
    val fromDate = LocalDate.of(year, month, 1)
    val toDate = fromDate.plusMonths(1).minusDays(1)

    list(figi, fromDate.atTime(0, 0), toDate.atTime(23, 59), interval)
  }

  def listYearly(figi: String,
                 year: Int,
                 interval: CandleTimeFrame.Value): List[Candle] = {
    val fromDate = LocalDate.of(year, 1, 1)
    val toDate = fromDate.plusYears(1).minusDays(1)

    list(figi, fromDate.atTime(0, 0), toDate.atTime(23, 59), interval)
  }

  def insert(element: Candle): Candle = {
    inTransaction {
      candle.insert(element)
    }
  }

  def insert(elements: Candle*): Unit = {
    inTransaction {
      candle.insert(elements)
    }
  }

}
