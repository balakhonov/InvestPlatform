package com.balakhonov.invest.provider

import com.balakhonov.invest.models.{db, _}
import com.balakhonov.invest.models.db.{Candle, Instrument}
import com.balakhonov.invest.models.enums.{CandleTimeFrame, OperationStatus, OperationType, OrderStatus}
import ru.tinkoff.invest.openapi.model.rest.{Candle => RCandle, MarketInstrument => RInstrument, Operation => ROperation, Order => ROrder, PlacedLimitOrder => RPlacedLimitOrder}
import ru.tinkoff.invest.openapi.model.streaming.CandleInterval
import ru.tinkoff.invest.openapi.model.streaming.StreamingEvent.{Candle => SCandle}

package object tinkoff {

  implicit class RInstrumentRich(instrument: RInstrument) {
    def asScala: Instrument = {
      Instrument(
        id = 0,
        providerId = 1,
        figi = instrument.getFigi,
        name = instrument.getName,
        ticker = instrument.getTicker,
        isin = instrument.getIsin,
        lotSize = instrument.getLot,
        currency = instrument.getCurrency.getValue,
        `type` = instrument.getType.getValue)
    }
  }

  implicit class SCandleRich(candle: SCandle) {
    def asScala: Candle = db.Candle(
      id = 0,
      openPrice = candle.getOpenPrice.doubleValue(),
      closingPrice = candle.getClosingPrice.doubleValue(),
      highestPrice = candle.getHighestPrice.doubleValue(),
      lowestPrice = candle.getLowestPrice.doubleValue(),
      tradingValue = candle.getTradingValue.intValue(),
      dateTime = candle.getDateTime.toLocalDateTime,
      interval = CandleTimeFrame.withName(candle.getInterval.getValue),
      figi = candle.getFigi
    )
  }

  implicit class RCandleRich(candle: RCandle) {
    def asScala: Candle = db.Candle(
      id = 0,
      openPrice = candle.getO.doubleValue(),
      closingPrice = candle.getC.doubleValue(),
      highestPrice = candle.getH.doubleValue(),
      lowestPrice = candle.getL.doubleValue(),
      tradingValue = candle.getV,
      dateTime = candle.getTime.toLocalDateTime,
      interval = CandleTimeFrame.withName(candle.getInterval.getValue),
      figi = candle.getFigi
    )
  }

  implicit class RPlacedLimitOrderRich(order: RPlacedLimitOrder) {
    def asScala: PlacedLimitOrder = PlacedLimitOrder(
      orderRef = order.getOrderId,
      status = OrderStatus.withName(order.getStatus.getValue),
      commission = Option(order.getCommission).map(_.getValue.doubleValue()),
      executedLots = order.getExecutedLots,
      rejectReason = Option(order.getRejectReason),
      message = Option(order.getMessage)
    )
  }

  implicit class ROrderRich(order: ROrder) {
    def asScala: Order = Order(
      figi = order.getFigi,
      orderRef = order.getOrderId,
      operation = OperationType.withName(order.getOperation.getValue),
      status = OrderStatus.withName(order.getStatus.getValue),
      requestedLots = order.getRequestedLots,
      executedLots = order.getExecutedLots
    )
  }

  implicit class ROperationRich(operation: ROperation) {
    def asScala: Operation = Operation(
      figi = operation.getFigi,
      date = operation.getDate.toLocalDateTime,
      status = OperationStatus.withName(operation.getStatus.getValue)
    )
  }
}
