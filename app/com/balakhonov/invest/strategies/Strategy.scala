package com.balakhonov.invest.strategies

import com.balakhonov.invest.models.TradeContext
import com.balakhonov.invest.models.db.Candle
import com.balakhonov.invest.models.enums.CandleTimeFrame.CandleTimeFrame
import com.balakhonov.invest.services.OrderService

import scala.collection.mutable

trait Strategy {
  private[this] var orderServiceOpt: Option[OrderService] = None

  val candlesQueue: mutable.Queue[Candle] = mutable.Queue.empty[Candle]

  val MaxCandlesQueueSize: Int
  val TimeFrame: CandleTimeFrame

  protected def beforeProcess(candle: Candle)
                             (implicit ctx: TradeContext): Unit

  protected def afterProcess(candle: Candle)
                            (implicit ctx: TradeContext): Unit

  protected def receive(candle: Candle)
                       (implicit ctx: TradeContext): Unit

  final def doReceive(candle: Candle)
                     (implicit ctx: TradeContext): Unit = {
    withCandles(candle) {
      receive(candle)
    }
  }

  final def setOrderService(os: OrderService): Unit = {
    orderServiceOpt = Some(os)
  }

  final def orderService: OrderService = orderServiceOpt.getOrElse {
    throw new IllegalStateException("OrderService was not initialized!")
  }

  final def setCandlesQueue(list: Seq[Candle]): Unit = {
    candlesQueue.clear()
    candlesQueue.enqueueAll(list.sortBy(_.dateTime))
  }

  final def historySize: Int = candlesQueue.size

  final def lastCandle: Option[Candle] = candlesQueue.lastOption

  final def lastPrice: Option[Double] = lastCandle.map(_.closingPrice)

  final def clear(): Unit = {
    candlesQueue.clear()
  }

  private[this] def withCandles(candle: Candle)
                               (block: => Unit)
                               (implicit ctx: TradeContext): Unit = {
    def isPriceChanged(c: Candle) = c.closingPrice != candle.closingPrice

    def isTimeFrameChanged(c: Candle) = {
      var prevDateTime = c.dateTime.withSecond(0).withNano(0)
      var newDateTime = candle.dateTime.withSecond(0).withNano(0)
      val delta = TimeFrame match {
        case com.balakhonov.invest.models.enums.CandleTimeFrame._1MIN => 1
        case com.balakhonov.invest.models.enums.CandleTimeFrame._2MIN => 2
        case com.balakhonov.invest.models.enums.CandleTimeFrame._3MIN => 3
        case com.balakhonov.invest.models.enums.CandleTimeFrame._5MIN => 5
        case com.balakhonov.invest.models.enums.CandleTimeFrame._10MIN => 10
        case com.balakhonov.invest.models.enums.CandleTimeFrame._15MIN => 15
        case com.balakhonov.invest.models.enums.CandleTimeFrame._30MIN => 30
        case com.balakhonov.invest.models.enums.CandleTimeFrame.HOUR => 60
      }

      while ((prevDateTime.getMinute % delta) != 0) {
        prevDateTime = prevDateTime.minusMinutes(1)
      }

      while ((newDateTime.getMinute % delta) != 0) {
        newDateTime = newDateTime.minusMinutes(1)
      }

      prevDateTime.toString != newDateTime.toString
    }

    // не обрабатываем ивент если цена не изменилась
    val isApplicableToProcess = {
      lastCandle.forall(c => isPriceChanged(c) || isTimeFrameChanged(c))
    }

    if (lastCandle.forall(isTimeFrameChanged)) {
      candlesQueue.enqueue(candle)
    } else {
      candlesQueue.update(candlesQueue.size - 1, candle)
    }

    // не обрабатываем если цена не изменилась
    if (isApplicableToProcess) {
      beforeProcess(candle)
      block
      afterProcess(candle)
    }

    if (candlesQueue.size > MaxCandlesQueueSize)
      candlesQueue.dequeue()
  }
}