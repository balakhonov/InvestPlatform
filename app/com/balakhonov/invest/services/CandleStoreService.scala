package com.balakhonov.invest.services

import com.balakhonov.invest.dao.UtilDAO
import com.balakhonov.invest.models.db.Candle
import com.balakhonov.invest.models.enums.CandleTimeFrame
import com.balakhonov.invest.provider.ClientProvider
import com.balakhonov.invest.util.Executors

import java.time._
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}


@Singleton
case class CandleStoreService @Inject()(utilDAO: UtilDAO) {
  private implicit val ec: ExecutionContextExecutor = ExecutionContext.fromExecutor(Executors.newFixedThreadPool("candle-store", 2))

  def loadDaily(figi: String,
                year: Int,
                month: Int,
                fromDay: Int,
                toDay: Int,
                timeFrame: CandleTimeFrame.Value,
                isRemote: Boolean)
               (implicit client: ClientProvider): Future[CandleHistory] = {
    val future = if (isRemote) {
      val periods = (for (day <- fromDay to toDay) yield MonthDay.of(month, day)).toList

      val resultF = periods.foldLeft(Future.successful[List[(MonthDay, List[Candle])]](Nil)) { case (acc, monthDay) =>
        acc.flatMap { prevList =>
          Thread.sleep(100)
          client.pullHistory(
            figi = figi,
            from = OffsetDateTime.of(year, month, monthDay.getDayOfMonth, 0, 0, 0, 0, ZoneOffset.UTC),
            to = OffsetDateTime.of(year, month, monthDay.getDayOfMonth, 23, 59, 59, 0, ZoneOffset.UTC),
            timeFrame = timeFrame
          ).map { list =>
            prevList.::(monthDay -> list)
          }
        }
      }

      resultF.map(_.sortBy(_._1).toList)
    } else {
      Future.successful(utilDAO.candleDao.listDaily(figi, year, month, fromDay, toDay, timeFrame)
        .groupBy(r => MonthDay.from(r.dateTime))
        .toList
        .sortBy(_._1))
    }

    future.map(CandleHistory)
  }

  def loadYearly(figi: String,
                 year: Int,
                 interval: CandleTimeFrame.Value): Future[CandleHistory] = {
    Future.successful(CandleHistory(utilDAO.candleDao.listYearly(figi, year, interval)
      .groupBy(r => MonthDay.from(r.dateTime))
      .toList
      .sortBy(_._1)))
  }

  def loadHistory(figi: String,
                  timeFrame: CandleTimeFrame.Value)
                 (implicit client: ClientProvider): Future[CandleHistory] = {
    val currentDateTime = LocalDateTime.now()
    val dayShift = currentDateTime.getDayOfWeek match {
      case DayOfWeek.SUNDAY => 2
      case DayOfWeek.MONDAY => 3
      case _ => 1
    }
    val startDateTime = currentDateTime.minusDays(dayShift + 10).withHour(0).withMinute(0)
    if (startDateTime.getMonthValue == currentDateTime.getMonthValue) {
      loadDaily(figi, startDateTime.getYear, startDateTime.getMonthValue, startDateTime.getDayOfMonth, currentDateTime.getDayOfMonth, timeFrame, isRemote = true)
    } else {
      loadDaily(figi, startDateTime.getYear, startDateTime.getMonthValue, startDateTime.getDayOfMonth, startDateTime.getDayOfMonth, timeFrame, isRemote = true)
    }
  }
}

case class CandleHistory(list: List[(MonthDay, List[Candle])]) {
  require(list.flatMap(_._2).nonEmpty, "History can't be empty!")

  val size: Int = list.map(_._2.size).sum

  val candles: Seq[Candle] = list.flatMap(_._2).sortBy(_.dateTime)

  val lastCandle: Candle = candles.last
}