package com.balakhonov.invest.strategies

import cats.Eval
import cats.implicits.catsSyntaxOptionId
import com.balakhonov.invest
import com.balakhonov.invest.indicator.{RSI, TrendDetector}
import com.balakhonov.invest.models._
import com.balakhonov.invest.models.db.Candle
import com.balakhonov.invest.models.enums.CandleTimeFrame
import com.balakhonov.invest.models.enums.CandleTimeFrame.CandleTimeFrame
import com.balakhonov.invest.services.BuyConditions
import com.balakhonov.invest.util.ConsoleUtil
import com.balakhonov.invest.util.PriceUtil._
import com.balakhonov.invest.{Downtrend, Flatline, Trend}
import play.api.Logger

import scala.jdk.CollectionConverters._
import scala.util.Random

object RSIStrategy {
  case class Settings()
}

case class UptrendOptions(thresholdToBuy: Double,
                          rsiPeriod: Int = 14,
                          rsiLowThreshold: Int = 27)

case class DowntrendOptions(enabled: Boolean,
                            thresholdToBuy: Double,
                            rsiPeriod: Int = 14,
                            rsiLowThreshold: Int = 27)

case class FirstBuySettings(percDeficitAgainstHighestPrice: Double = 0.3,
                            numberOfBarsToCheck: Int = 60)

case class RSIStrategy(firstBuySettings: FirstBuySettings,
                       minDeltaPercToSell: Double,
                       adjustedDeltaPercToSellMultiplier: Double = 0.6,
                       prevP1TrendBars: Int = 15,
                       currentP1TrendBars: Int = 15,
                       currentP2TrendBars: Int = 50,
                       currentP2TrendMultiplier: Int = 2,
                       currentP3TrendBars: Int = 250,
                       currentP3TrendMultiplier: Int = 10,
                       uptrendOptions: UptrendOptions,
                       downtrendOptions: DowntrendOptions)
  extends Strategy {

  private val LOG = Logger(this.getClass)

  private val DoubleTransactionCommissionPerc: Double = Account.TransactionCommission * 100 * 2

  private var currentP2Trend: Trend = Flatline
  private var currentP3Trend: Trend = Flatline

  override val MaxCandlesQueueSize: Int = 600
  override val TimeFrame: CandleTimeFrame = CandleTimeFrame._10MIN

  override def beforeProcess(candle: Candle)
                            (implicit ctx: TradeContext): Unit = {
    val prices = candlesQueue.map(_.closingPrice).toList

    currentP2Trend = TrendDetector.calc(prices.takeRight(currentP2TrendBars), currentP2TrendMultiplier)
    currentP3Trend = TrendDetector.calc(prices.takeRight(currentP3TrendBars), currentP3TrendMultiplier)
  }

  override def afterProcess(candle: Candle)
                           (implicit ctx: TradeContext): Unit = {
    // no code
  }

  override def receive(candle: Candle)
                      (implicit ctx: TradeContext): Unit = {
    printStatistic(candle)

    defineExitPoint(candle)(ctx)

    defineEntryPoint(candle)(ctx)
  }

  private def printStatistic(candle: Candle)
                            (implicit ctx: TradeContext): Unit = {
    lazy val profitPerc = calcProfitPerc(candle)
    lazy val price = ConsoleUtil.profit(candle)
    lazy val trends = ConsoleUtil.trends(Seq(currentP3Trend, currentP2Trend))

    if (Random.nextInt(75) == 25 || profitPerc > 0.09) {
      val list = candlesQueue.takeRight(300).map(_.closingPrice)
      val delta = 100 - list.min / list.max * 100
      //      print("[%-5s] [%-20s] [%-29s], [%-10s]\r".format(candle.dateTime.toLocalTime, ctx.instrument.name, price, trends))
      LOG.debug("[%-5s] [%-20s] [%-29s], [%-10s], [%-10s]".format(candle.dateTime, ctx.instrument.name, price, delta, trends))
    }
  }

  private def defineEntryPoint(candle: Candle)
                              (implicit ctx: TradeContext): Unit = {
    val patternOpt = {
      val highestPriceForPeriod = Eval.later(getHighestPrice(firstBuySettings.numberOfBarsToCheck))

      defineNextEntryPoint(candle, highestPriceForPeriod)
    }

    patternOpt.foreach { pattern =>
      println("buy")
      orderService.buy(candle, pattern, ctx.bot.value.lotsToBuy)
    }
  }

  private def defineNextEntryPoint(candle: Candle,
                                   highestPriceForPeriod: Eval[Double])
                                  (implicit ctx: TradeContext): Option[BuyConditions] = {
    Option.when(historySize > uptrendOptions.rsiPeriod.max(downtrendOptions.rsiPeriod)) {
      lazy val trends = ConsoleUtil.trends(Seq(currentP3Trend, currentP2Trend))
      lazy val deficitPerc = calcDeficitPerc(candle, highestPriceForPeriod.map(_.some))
      lazy val prices = candlesQueue.map(_.closingPrice).toList.map(Double.box).asJava
      lazy val delta = {
        val list = candlesQueue.takeRight(300).map(_.closingPrice)
        100 - list.min / list.max * 100
      }

      def isApplicableOnDowntrend = {
        lazy val rsi = RSI.calculate(prices, downtrendOptions.rsiPeriod)
        (downtrendOptions.enabled
          && deficitPerc > downtrendOptions.thresholdToBuy
          && deficitPerc > tb(downtrendOptions.thresholdToBuy)
          && rsi < downtrendOptions.rsiLowThreshold)
      }

      def isApplicableOnUptrend = {
        lazy val rsi = RSI.calculate(prices, uptrendOptions.rsiPeriod)
        (deficitPerc > uptrendOptions.thresholdToBuy
          && deficitPerc > tb(uptrendOptions.thresholdToBuy)
          && rsi < uptrendOptions.rsiLowThreshold)
      }

      //      currentP3Trend match {
      //        case invest.Downtrend if isApplicableOnDowntrend && delta < 2 =>
      //          //          LOG.debug(s"[${candle.dateTime}] $trends")
      //          BuyConditions.RSI
      //          None
      //
      //        case invest.Flatline if isApplicableOnUptrend && delta < 1.5 =>
      //          //          LOG.info(s"[${candle.dateTime}] $trends") TODO
      //          BuyConditions.RSI
      //
      //        case invest.Uptrend(_) if isApplicableOnUptrend && currentP2Trend != Downtrend && delta < 2 => // TODO
      //          //          LOG.debug(s"[${candle.dateTime}] $trends")
      //          BuyConditions.RSI
      //
      //        case _ =>
      //          None
      //      }

      currentP3Trend match {
        case invest.Downtrend if isApplicableOnDowntrend =>
          //          LOG.debug(s"[${candle.dateTime}] $trends")
          BuyConditions.RSI

        case invest.Flatline =>
          //          LOG.info(s"[${candle.dateTime}] $trends") TODO
          None

        case invest.Uptrend(_) if isApplicableOnUptrend && currentP2Trend != Downtrend => // TODO
          //          LOG.debug(s"[${candle.dateTime}] $trends")
          BuyConditions.RSI

        case _ =>
          None
      }
    }.flatten
  }

  /**
   * Минимальная разница в цене для покупки новой позиции на усреднение
   */
  private def tb(base: Double)
                (implicit ctx: TradeContext): Double = {
    (base + DoubleTransactionCommissionPerc) + ctx.lotsApplicableToSell.value.size * 0.2
  }

  private def getHighestPrice(bars: Int): Double = {
    candlesQueue.takeRight(bars).maxBy(_.highestPrice).highestPrice
  }

  private def defineExitPoint(candle: Candle)
                             (implicit ctx: TradeContext): Unit = {
    if (ctx.hasLotsToSell) {
      lazy val deltaPercToSell = {
        val list = candlesQueue.takeRight(300).map(_.closingPrice)
        val delta = 100 - list.min / list.max * 100
        Math.max(minDeltaPercToSell, delta * adjustedDeltaPercToSellMultiplier)
      }

      // возможность массовой продажи
      val lots = ctx.lotsApplicableToSell.value.filter { lot =>
        val profitPerc = calcProfitPerc(candle, lot) // TODO Optimize

        if (profitPerc > minDeltaPercToSell) {
          // println("profitPerc", profitPerc, "deltaPercToSell", deltaPercToSell, profitPerc > deltaPercToSell)
        }
        profitPerc > minDeltaPercToSell && profitPerc > deltaPercToSell
      }

      if (lots.nonEmpty) {
        // println("sell")
        orderService.sell(lots.map(_.id), candle)
      }
    }
  }

}