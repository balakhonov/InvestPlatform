package com.balakhonov.invest.provider.tinkoff

import com.balakhonov.invest.models._
import com.balakhonov.invest.models.db.{Candle, Instrument}
import com.balakhonov.invest.models.enums.CandleTimeFrame.CandleTimeFrame
import com.balakhonov.invest.provider.ClientProvider
import com.balakhonov.invest.provider.tinkoff.services.TinkoffClient
import ru.tinkoff.invest.openapi.StreamingContext
import ru.tinkoff.invest.openapi.model.rest.CandleResolution

import java.time.{OffsetDateTime, ZonedDateTime}
import javax.inject.{Inject, Singleton}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.jdk.CollectionConverters._

@Singleton
class TinkoffClientProvider @Inject()(tinkoffClient: TinkoffClient)
  extends ClientProvider {

  override def listInstruments: Future[List[Instrument]] = {
    tinkoffClient.listInstruments.map(_.map(_.asScala))
  }

  override def listActiveOrders: Future[List[Order]] = {
    tinkoffClient.listActiveOrders.map(_.map(_.asScala))
  }

  override def pullHistoryByYear(figi: String,
                                 year: Int,
                                 timeFrame: CandleTimeFrame): Future[Seq[Candle]] = {
    val interval = CandleResolution.fromValue(timeFrame.toString)

    tinkoffClient.listYearlyHistory(figi, year, interval)
  }

  override def pullHistoryByMonth(figi: String,
                                  year: Int,
                                  month: Int,
                                  timeFrame: CandleTimeFrame): Future[Seq[Candle]] = {
    val interval = CandleResolution.fromValue(timeFrame.toString)

    tinkoffClient.listMonthlyHistory(figi, year, month, interval)
  }

  override def pullHistory(figi: String,
                           from: OffsetDateTime,
                           to: OffsetDateTime,
                           timeFrame: CandleTimeFrame): Future[List[Candle]] = {
    val interval = CandleResolution.fromValue(timeFrame.toString)

    tinkoffClient.listHistory(figi, from, to, interval).map { opt =>
      Option(opt.orElseGet(None.orNull)) match {
        case Some(list) => list.getCandles.asScala.map(c => c.asScala).toList
        case None => Nil
      }
    }
  }

  override def limitedOrderToBuy(figi: String,
                                 price: Double,
                                 lots: Int): Future[PlacedLimitOrder] = {
    tinkoffClient.limitedOrderToBuy(figi, price, lots).map(_.asScala)
  }

  override def limitedOrderToSell(figi: String,
                                  price: Double,
                                  lots: Int): Future[PlacedLimitOrder] = {
    tinkoffClient.limitedOrderToSell(figi, price, lots).map(_.asScala)
  }

  override def listOperations(figi: String,
                              from: ZonedDateTime,
                              to: ZonedDateTime): Future[List[Operation]] = {
    tinkoffClient.listOperations(figi, from, to).map(_.map(_.asScala))
  }

  override def orderToClose(orderRef: String): Future[Unit] = {
    tinkoffClient.orderToClose(orderRef)
  }

  def getPublisher: StreamingContext = tinkoffClient.api.getStreamingContext
}
