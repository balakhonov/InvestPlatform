package com.balakhonov.invest.provider

import com.balakhonov.invest.models._
import com.balakhonov.invest.models.db.{Candle, Instrument}
import com.balakhonov.invest.models.enums.CandleTimeFrame.CandleTimeFrame

import java.time.{OffsetDateTime, ZonedDateTime}
import scala.concurrent.Future

trait ClientProvider {

  def listInstruments: Future[List[Instrument]]

  def listActiveOrders: Future[List[Order]]

  def pullHistoryByYear(figi: String,
                        year: Int,
                        timeFrame: CandleTimeFrame): Future[Seq[Candle]]

  def pullHistoryByMonth(figi: String,
                         year: Int,
                         month: Int,
                         timeFrame: CandleTimeFrame): Future[Seq[Candle]]

  def pullHistory(figi: String,
                  from: OffsetDateTime,
                  to: OffsetDateTime,
                  timeFrame: CandleTimeFrame): Future[List[Candle]]

  def limitedOrderToBuy(figi: String, price: Double, lots: Int): Future[PlacedLimitOrder]

  def limitedOrderToSell(figi: String, price: Double, lots: Int): Future[PlacedLimitOrder]

  def listOperations(figi: String,
                     from: ZonedDateTime,
                     to: ZonedDateTime): Future[List[Operation]]

  def orderToClose(orderRef: String): Future[Unit]
}
