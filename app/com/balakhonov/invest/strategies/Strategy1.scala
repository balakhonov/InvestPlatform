package com.balakhonov.invest.strategies

import com.balakhonov.invest.indicator.EMA
import com.balakhonov.invest.models._
import com.balakhonov.invest.models.db.Candle
import com.balakhonov.invest.models.enums.CandleTimeFrame
import com.balakhonov.invest.models.enums.CandleTimeFrame.CandleTimeFrame
import com.balakhonov.invest.services.BuyConditions
import com.balakhonov.invest.util.ConsoleUtil
import com.balakhonov.invest.util.PriceUtil._
import com.balakhonov.invest.{Downtrend, Flatline, Trend, Uptrend}
import play.api.Logger

import java.time.LocalTime

object Strategy1 {
  case class Settings()
}

case class Strategy1(t1Short: Int, // 5
                     t1Long: Int, // 16
                     t2Short: Int, // 25
                     t2Long: Int, // 40
                     precision: Double, // 0.001
                     deltaPerc1: Double, // 1.4
                     barsCount1: Int, // 30
                     t2MinClosedPriceDeltaPerc2: Double, // 0.7
                     slSettings: StopLossSettings,
                     tpSettings: Either[Double, TakeProfitSettings])
  extends Strategy {

  private val LOG = Logger(this.getClass)

  private val t1PowerBarSize = 3

  private val t2PowerBarSize = 5
  private val t2UpPower1 = 0.8

  private val t3PowerBarSize = 120

  private val trendPowerDiffConst = 0.5d

  private var prevT1Trend: Trend = Flatline
  private var currentT1Trend: Trend = Flatline
  private var currentT2Trend: Trend = Flatline
  private var currentT3Trend: Trend = Flatline

  private val LowThresholdToBuy = 0.4
  private val MidThresholdToBuy = 0.6
  private val HightThresholdToBuy = 1.4

  override val MaxCandlesQueueSize: Int = t1Long * 40
  override val TimeFrame: CandleTimeFrame = CandleTimeFrame._10MIN

  override def beforeProcess(candle: Candle)
                            (implicit ctx: TradeContext): Unit = {
    val prices = candlesQueue.map(_.closingPrice).toList
    val prevPrices = prices.dropRight(1)

    prevT1Trend = calcTrendPower(EMA.trend(t1Short, t1Long, prevPrices, 0.01), t1PowerBarSize)
    currentT1Trend = calcTrendPower(EMA.trend(t1Short, t1Long, prices, 0.01), t1PowerBarSize)
    currentT2Trend = calcTrendPower(EMA.trend(t2Short, t2Long, prices, 0.01), t2PowerBarSize)
    currentT3Trend = calcTrendPower(EMA.trend(250, 320, prices, 0.01), t3PowerBarSize)
  }

  override def afterProcess(candle: Candle)
                           (implicit ctx: TradeContext): Unit = {
    // no code
  }

  override def receive(candle: Candle)
                      (implicit ctx: TradeContext): Unit = {
    printStatistic(candle, ctx)

    defineExitPoint(candle)(ctx)

    defineEntryPoint(candle)(ctx)
  }

  private def printStatistic(candle: Candle,
                             ctx: TradeContext): Unit = {
    val profitPerc = calcProfitPerc(candle)(ctx)

    val price = ConsoleUtil.profit(candle)(ctx)
    val trends = ConsoleUtil.trends(Seq(currentT3Trend, currentT2Trend, prevT1Trend, currentT1Trend))

    if (profitPerc < -LowThresholdToBuy || profitPerc > 0.09) {
      //      print("[%-5s] [%-20s] [%-29s], [%-10s]\r".format(candle.dateTime.toLocalTime, ctx.instrument.getName, price, trends))
      LOG.debug("[%-5s] [%-20s] [%-29s], [%-10s]".format(candle.dateTime.toLocalTime, ctx.instrument.name, price, trends))
    }
  }

  private def defineEntryPoint(candle: Candle)
                              (implicit ctx: TradeContext): Unit = {
    def buy(pattern: String): Option[BuyConditions] = {
      // Если текущая цена акции меньше максимальной цены за выбранный период {barsCount1} на {deltaPerc1} то
      // покупаем тк предполагаем что цена упала и будет разворот
      val isApplicableToBuy = {
        val highestPriceForPeriod = getHighestPrice(barsCount1)
        val perc = calcDeficitPerc(ctx.defineLotPrice(highestPriceForPeriod), ctx.defineLotPrice(candle.closingPrice))

        // TODO дополнительно надо учитывать тренд и не покупать на падающем
        perc > deltaPerc1
      }

      Option.when(isApplicableToBuy)(BuyConditions(pattern))
    }

    lazy val isStopLossReached = {
      Option.when(ctx.hasLotsToSell) {
        lazy val deficitPerc = calcDeficitPerc(candle)

        slSettings.enabled && deficitPerc > slSettings.stopLossPerc
      }.getOrElse(false)
    }

    val patternOpt = (ctx.lotsApplicableToSell.value, prevT1Trend, currentT1Trend) match {
      case (_, _, _) if ctx.hasLotsToSell && isStopLossReached =>
        // цена последней сделки пробила линию поддержки
        // ищем удобную точку входа для покупки новой акции чтобы компенсировать потери
        defineNextEntryPoint(candle)

      case (Nil, Uptrend(_), Uptrend(_)) =>
        // цена растет
        buy("B1")

      case (Nil, Uptrend(_), Flatline) =>
        // цена больше не растет
        None

      case (Nil, Uptrend(_), Downtrend) =>
        // цена развернулась на падение
        None

      case (Nil, Flatline, Uptrend(_)) =>
        // цена увеличивается после бокового тренда
        buy("B4")

      case (Nil, Flatline, Flatline) =>
        // цена не меняется, идет боковик
        buy("B5")

      case (Nil, Flatline, Downtrend) =>
        // цена уменьшается
        None

      case (Nil, Downtrend, Uptrend(_)) =>
        // резкий разворот Uptrend -> Downtrend
        buy("B7")

      case (Nil, Downtrend, Flatline) =>
        // легкий разворот цены
        buy("B8")

      case (Nil, Downtrend, Downtrend) =>
        // цена продолжает падать
        None

      case _ =>
        // no code
        None
    }

    patternOpt.foreach { pattern =>
      orderService.buy(candle, pattern, ctx.bot.value.lotsToBuy)
    }
  }

  private def printCanBuyMessage(candle: Candle,
                                 pattern: String)
                                (implicit ctx: TradeContext): Option[BuyConditions] = {
    val price = ConsoleUtil.profit(candle)
    val trends = ConsoleUtil.trends(Seq(currentT3Trend, currentT2Trend, prevT1Trend, currentT1Trend))
    LOG.info(s"[${LocalTime.now()}] Can buy '${ctx.instrument.name}' '$pattern' [$price] [$trends]")
    None
  }

  private def defineNextEntryPoint(candle: Candle)
                                  (implicit ctx: TradeContext): Option[BuyConditions] = {
    require(ctx.lotsApplicableToSell.value.nonEmpty)
    val deficitPerc = calcDeficitPerc(candle)

    currentT3Trend match {
      case _ if deficitPerc > tb(HightThresholdToBuy) =>
        // покупка при максимальных ебенях
        Some(BuyConditions("MAX_EBENYA"))

      case Downtrend =>
        defineNextDowntrendEntryPoint(candle)

      case Flatline =>
        defineNextFlatlineEntryPoint(candle)

      case Uptrend(_) =>
        defineNextUptrendEntryPoint(candle)
    }
  }

  private def defineNextDowntrendEntryPoint(candle: Candle)
                                           (implicit ctx: TradeContext): Option[BuyConditions] = {
    val deficitPerc = calcDeficitPerc(candle)

    (currentT2Trend, (prevT1Trend, currentT1Trend)) match {
      case (Downtrend, (Downtrend, Downtrend)) =>
        // цена идет вниз, не трогаем до разворота
        None // D1

      case (Downtrend, (Downtrend, Flatline)) if deficitPerc > tb(MidThresholdToBuy) =>
        // цена идет вниз, не трогаем до разворота
        None // D2

      case (Downtrend, (Downtrend, Uptrend(power1))) if deficitPerc > tb(LowThresholdToBuy) =>
        Option.when(power1 > 0.7)(BuyConditions("D3"))

      case (Downtrend, (Flatline, Downtrend)) if deficitPerc > tb(LowThresholdToBuy) =>
        // цена продолжает падать
        None // D4

      case (Downtrend, (Flatline, Flatline)) if deficitPerc > tb(MidThresholdToBuy) =>
        // цена резко развернулась
        Some(BuyConditions("D5"))

      case (Downtrend, (Flatline, Uptrend(power1))) if deficitPerc > tb(LowThresholdToBuy) =>
        Option.when(power1 > 0.5)(BuyConditions("D6"))

      case (Downtrend, (Uptrend(_), Downtrend)) if deficitPerc > tb(MidThresholdToBuy) =>
        printCanBuyMessage(candle, "D7")

      case (Downtrend, (Uptrend(_), Flatline)) if deficitPerc > tb(MidThresholdToBuy) =>
        printCanBuyMessage(candle, "D8")

      case (Downtrend, (Uptrend(_), Uptrend(_))) if deficitPerc > tb(LowThresholdToBuy) =>
        // начала расти
        Some(BuyConditions("D9"))

      case (Flatline, (Downtrend, Downtrend)) if deficitPerc > tb(MidThresholdToBuy) =>
        None // D10

      case (Flatline, (Downtrend, Flatline)) if deficitPerc > tb(MidThresholdToBuy) =>
        printCanBuyMessage(candle, "D11")

      case (Flatline, (Downtrend, Uptrend(_))) if deficitPerc > tb(MidThresholdToBuy) =>
        printCanBuyMessage(candle, "D12")

      case (Flatline, (Flatline, Downtrend)) if deficitPerc > tb(MidThresholdToBuy) =>
        None // D13

      case (Flatline, (Flatline, Flatline)) if deficitPerc > tb(MidThresholdToBuy) =>
        // цена перешла в боковик
        Some(BuyConditions("D14"))

      case (Flatline, (Flatline, Uptrend(_))) if deficitPerc > tb(MidThresholdToBuy) =>
        Some(BuyConditions("D15"))

      case (Flatline, (Uptrend(_), Downtrend)) if deficitPerc > tb(MidThresholdToBuy) =>
        printCanBuyMessage(candle, "D16")

      case (Flatline, (Uptrend(_), Flatline)) if deficitPerc > tb(MidThresholdToBuy) =>
        printCanBuyMessage(candle, "D17")

      case (Flatline, (Uptrend(_), Uptrend(_))) if deficitPerc > tb(MidThresholdToBuy) =>
        // цена растет
        Some(BuyConditions("D18"))

      case (Uptrend(_), (Downtrend, Downtrend)) if deficitPerc > tb(LowThresholdToBuy) =>
        // цена падает, ждем выравнивания на Flatline
        None // D19

      case (Uptrend(_), (Downtrend, Flatline)) if deficitPerc > tb(MidThresholdToBuy) =>
        // цена выравнивается
        Some(BuyConditions("D20"))

      case (Uptrend(_), (Downtrend, Uptrend(_))) if deficitPerc > tb(MidThresholdToBuy) =>
        printCanBuyMessage(candle, "D21")

      case (Uptrend(_), (Flatline, Downtrend)) if deficitPerc > tb(MidThresholdToBuy) =>
        printCanBuyMessage(candle, "D22")

      case (Uptrend(_), (Flatline, Flatline)) if deficitPerc > tb(MidThresholdToBuy) =>
        // цена выравнялась
        Some(BuyConditions("D23"))

      case (Uptrend(_), (Flatline, Uptrend(_))) if deficitPerc > tb(LowThresholdToBuy) =>
        // цена начала расти
        Some(BuyConditions("D24"))

      case (Uptrend(_), (Uptrend(_), Downtrend)) if deficitPerc > tb(MidThresholdToBuy) =>
        printCanBuyMessage(candle, "D25")

      case (Uptrend(_), (Uptrend(_), Flatline)) if deficitPerc > tb(MidThresholdToBuy) =>
        printCanBuyMessage(candle, "D26")

      case (Uptrend(_), (Uptrend(_), Uptrend(_))) if deficitPerc > tb(MidThresholdToBuy) =>
        // цена начала расти
        Some(BuyConditions("D27"))

      case _ =>
        // waiting
        None
    }
  }

  private def defineNextFlatlineEntryPoint(candle: Candle)
                                          (implicit ctx: TradeContext): Option[BuyConditions] = {
    val deficitPerc = calcDeficitPerc(candle)

    (currentT2Trend, (prevT1Trend, currentT1Trend)) match {
      case (Downtrend, (Downtrend, Downtrend)) if deficitPerc > tb(MidThresholdToBuy) =>
        Some(BuyConditions("F1"))

      case (Downtrend, (Downtrend, Flatline)) if deficitPerc > tb(MidThresholdToBuy) =>
        Some(BuyConditions("F2"))

      case (Downtrend, (Downtrend, Uptrend(power1))) if deficitPerc > tb(LowThresholdToBuy) =>
        // цена резко развернулась
        Option.when(deficitPerc > tb(LowThresholdToBuy) && power1 > t2UpPower1)(BuyConditions("F3"))

      case (Downtrend, (Flatline, Downtrend)) if deficitPerc > tb(LowThresholdToBuy) =>
        None // F4

      case (Downtrend, (Flatline, Flatline)) if deficitPerc > tb(MidThresholdToBuy) =>
        Some(BuyConditions("F5"))

      case (Downtrend, (Flatline, Uptrend(_))) if deficitPerc > tb(MidThresholdToBuy) =>
        Some(BuyConditions("F6"))

      case (Downtrend, (Uptrend(_), Downtrend)) if deficitPerc > tb(LowThresholdToBuy) =>
        printCanBuyMessage(candle, "F7")

      case (Downtrend, (Uptrend(_), Flatline)) if deficitPerc > tb(LowThresholdToBuy) =>
        None // F8

      case (Downtrend, (Uptrend(_), Uptrend(_))) if deficitPerc > tb(MidThresholdToBuy) =>
        Some(BuyConditions("F9"))

      case (Flatline, (Downtrend, Downtrend)) if deficitPerc > tb(LowThresholdToBuy) =>
        Some(BuyConditions("F10"))

      case (Flatline, (Downtrend, Flatline)) if deficitPerc > tb(LowThresholdToBuy) =>
        printCanBuyMessage(candle, "F11")

      case (Flatline, (Downtrend, Uptrend(_))) if deficitPerc > tb(LowThresholdToBuy) =>
        printCanBuyMessage(candle, "F12")

      case (Flatline, (Flatline, Downtrend)) if deficitPerc > tb(LowThresholdToBuy) =>
        printCanBuyMessage(candle, "F13")

      case (Flatline, (Flatline, Flatline)) if deficitPerc > tb(MidThresholdToBuy) =>
        Some(BuyConditions("F14"))

      case (Flatline, (Flatline, Uptrend(_))) if deficitPerc > tb(LowThresholdToBuy) =>
        printCanBuyMessage(candle, "F15")

      case (Uptrend(_), (Downtrend, Downtrend)) if deficitPerc > tb(LowThresholdToBuy) =>
        Some(BuyConditions("F16"))

      case (Uptrend(_), (Downtrend, Flatline)) if deficitPerc > tb(LowThresholdToBuy) =>
        printCanBuyMessage(candle, "F17")

      case (Uptrend(_), (Downtrend, Uptrend(_))) if deficitPerc > tb(LowThresholdToBuy) =>
        Some(BuyConditions("F18"))

      case (Uptrend(_), (Flatline, Downtrend)) if deficitPerc > tb(LowThresholdToBuy) =>
        None // F19

      case (Uptrend(_), (Flatline, Flatline)) if deficitPerc > tb(LowThresholdToBuy) =>
        printCanBuyMessage(candle, "F20")

      case (Uptrend(_), (Flatline, Uptrend(_))) if deficitPerc > tb(LowThresholdToBuy) =>
        Some(BuyConditions("F21"))

      case (Uptrend(_), (Uptrend(_), Downtrend)) if deficitPerc > tb(LowThresholdToBuy) =>
        printCanBuyMessage(candle, "F22")

      case (Uptrend(_), (Uptrend(_), Flatline)) if deficitPerc > tb(LowThresholdToBuy) =>
        Some(BuyConditions("F23"))

      case (Uptrend(_), (Uptrend(_), Uptrend(_))) if deficitPerc > tb(LowThresholdToBuy) =>
        Some(BuyConditions("F24"))

      case _ =>
        // waiting
        None // F25
    }
  }

  private def defineNextUptrendEntryPoint(candle: Candle)
                                         (implicit ctx: TradeContext): Option[BuyConditions] = {
    val deficitPerc = calcDeficitPerc(candle)
    (currentT2Trend, (prevT1Trend, currentT1Trend)) match {
      case (Downtrend, (Downtrend, Downtrend)) if deficitPerc > tb(MidThresholdToBuy) =>
        Some(BuyConditions("U1"))

      case (Downtrend, (Downtrend, Flatline)) if deficitPerc > tb(MidThresholdToBuy) =>
        printCanBuyMessage(candle, "U2")

      case (Downtrend, (Downtrend, Uptrend(_))) if deficitPerc > tb(MidThresholdToBuy) =>
        printCanBuyMessage(candle, "U3")

      case (Downtrend, (Flatline, Downtrend)) if deficitPerc > tb(MidThresholdToBuy) =>
        Some(BuyConditions("U4"))

      case (Downtrend, (Flatline, Flatline)) if deficitPerc > tb(MidThresholdToBuy) =>
        Some(BuyConditions("U5"))

      case (Downtrend, (Flatline, Uptrend(_))) if deficitPerc > tb(MidThresholdToBuy) =>
        printCanBuyMessage(candle, "U6")

      case (Downtrend, (Uptrend(_), Downtrend)) if deficitPerc > tb(MidThresholdToBuy) =>
        Some(BuyConditions("U7"))

      case (Downtrend, (Uptrend(_), Flatline)) if deficitPerc > tb(MidThresholdToBuy) =>
        printCanBuyMessage(candle, "U8")

      case (Downtrend, (Uptrend(_), Uptrend(_))) if deficitPerc > tb(LowThresholdToBuy) =>
        // цена резко развернулась
        Some(BuyConditions("U9"))

      case (Flatline, (Downtrend, Downtrend)) if deficitPerc > tb(MidThresholdToBuy) =>
        Some(BuyConditions("U10"))

      case (Flatline, (Downtrend, Flatline)) if deficitPerc > tb(MidThresholdToBuy) =>
        printCanBuyMessage(candle, "U11")

      case (Flatline, (Downtrend, Uptrend(_))) if deficitPerc > tb(MidThresholdToBuy) =>
        printCanBuyMessage(candle, "U12")

      case (Flatline, (Flatline, Downtrend)) if deficitPerc > tb(MidThresholdToBuy) =>
        printCanBuyMessage(candle, "U13")

      case (Flatline, (Flatline, Flatline)) if deficitPerc > tb(MidThresholdToBuy) =>
        Some(BuyConditions("U14"))

      case (Flatline, (Flatline, Uptrend(_))) if deficitPerc > tb(MidThresholdToBuy) =>
        printCanBuyMessage(candle, "U15")

      case (Flatline, (Uptrend(_), Downtrend)) if deficitPerc > tb(MidThresholdToBuy) =>
        printCanBuyMessage(candle, "U16")

      case (Flatline, (Uptrend(_), Flatline)) if deficitPerc > tb(MidThresholdToBuy) =>
        Some(BuyConditions("U17"))

      case (Flatline, (Uptrend(_), Uptrend(_))) if deficitPerc > tb(MidThresholdToBuy) =>
        Some(BuyConditions("U18"))

      case (Uptrend(_), (Downtrend, Downtrend)) if deficitPerc > tb(MidThresholdToBuy) =>
        Some(BuyConditions("U19"))

      case (Uptrend(t2power), (Downtrend, Flatline)) if deficitPerc > tb(MidThresholdToBuy) /*t2MinClosedPriceDeltaPerc2*/ && t2power > t2UpPower1 =>
        // цена резко развернулась
        Some(BuyConditions("U20"))

      case (Uptrend(_), (Uptrend(_), Uptrend(_))) if deficitPerc > /*0.25*/ tb(MidThresholdToBuy) => // TODO
        // цена резко развернулась
        Some(BuyConditions("U21"))

      case _ =>
        // waiting
        None
    }
  }

  /**
   * Минимальная разница в цене для покупки новой позиции на усреднение
   */
  private def tb(base: Double)
                (implicit ctx: TradeContext): Double = {
    (base + Account.TransactionCommission * 100 * 2) + (ctx.lotsApplicableToSell.value.size - 1) * 0.2
  }

  private def calcTrendPower(trend: Trend, barsSize: Int): Trend = {
    trend match {
      case Uptrend(_) =>
        if (candlesQueue.size < barsSize) {
          Uptrend(Double.NaN)
        } else {
          val bars = candlesQueue.takeRight(barsSize)
          val first = bars.head
          val last = bars.last
          val a = 30d
          val b = (last.highestPrice - first.closingPrice) / barsSize * a * a / trendPowerDiffConst
          if (b < 0) {
            Uptrend(Double.NaN)
          } else {
            val c = Math.sqrt(a * a + b * b)
            val power = 1 - a / c
            Uptrend(power)
          }
        }
      case t => t
    }
  }

  private def getHighestPrice(bars: Int): Double = {
    candlesQueue.takeRight(bars).maxBy(_.highestPrice).highestPrice
  }

  private def defineExitPoint(candle: Candle)
                             (implicit ctx: TradeContext): Unit = {
    if (ctx.hasLotsToSell) {
      tpSettings match {
        case Left(minDeltaPercToSell) =>
          // возможность массовой продажи
          val lots = ctx.lotsApplicableToSell.value.filter { lot =>
            val profitPerc = calcProfitPerc(candle, lot)
            profitPerc > minDeltaPercToSell
          }

          if (lots.nonEmpty)
            orderService.sell(lots.map(_.id), candle)

        case Right(_) =>
        //            TakeProfitService.increaseOrSell(limitedOrderCache, candle, prevCandles.toSeq, settings, prevT1Trend, currentT1Trend, {
        //              // send order to sell $(deals.size) shares
        //              orderToSell(candle)
        //            })
      }
    }
  }

}

case class LimitedOrderCache(id: String,
                             orderId: Option[String],
                             deph: Int,
                             price: Double,
                             var takeProfitPrice: Double,
                             var markedToBuy: Boolean,
                             var markedToSell: Boolean)
