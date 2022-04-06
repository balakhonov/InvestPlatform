package com.balakhonov.invest.services

import com.balakhonov.invest.models.db.Candle
import com.balakhonov.invest.models.{Account, TakeProfitSettings}
import com.balakhonov.invest.strategies.LimitedOrderCache
import com.balakhonov.invest.{Downtrend, Flatline, Trend}

object TakeProfitService {

  def increaseOrSell(limitedOrderCache: LimitedOrderCache,
                     candle: Candle,
                     prevCandles: Seq[Candle],
                     settings: TakeProfitSettings,
                     prevTrend: Trend,
                     currentTrend: Trend,
                     sell: => Unit): Unit = {
    val takeProfitPrice = limitedOrderCache.takeProfitPrice - Account.TransactionCommission * limitedOrderCache.takeProfitPrice * 2

    val potentialNetProfit = candle.closingPrice - Account.TransactionCommission * candle.closingPrice * 2
    val potentialNetProfitPerc = (1 - (limitedOrderCache.price / potentialNetProfit)) * 100

    val isDealProfitable = potentialNetProfitPerc > settings.minDeltaToSellPerc

    if (isDealProfitable) {
      //      println(candle.dateTime, candle.closingPrice, prevTrend, currentTrend)

      //      val takeProfitDelta = (1 - (potentialNetProfit / deal.takeProfitPrice)) * 100
      val takeProfitDelta = 1 - ((potentialNetProfit - limitedOrderCache.price) / (takeProfitPrice - limitedOrderCache.price))

      if (takeProfitDelta > settings.takeProfitSellDelta) {
        //        val T2 = EMA.trend(25, 40, prevCandles.map(_.closingPrice).toList, 0.001)

        (prevTrend, currentTrend) match {
          case (Downtrend, Downtrend) =>
            // Take Profit. Цена упала ниже Take Profit, закрываем сделаку чтобы забрать минимальный доход
            sell

          case (Flatline, Downtrend) =>
            // Take Profit. Цена упала ниже Take Profit, закрываем сделаку чтобы забрать минимальный доход
            sell

          case _ =>
          // no code
        }
      } else {
        // поднимаем тейк профит ближе к цене
        if (potentialNetProfit > takeProfitPrice) {
          limitedOrderCache.takeProfitPrice = potentialNetProfit
        }
      }
    }
  }
}
