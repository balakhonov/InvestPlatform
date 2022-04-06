package com.balakhonov.invest.analyzer

import com.balakhonov.invest.models.{StopLossSettings, TakeProfitSettings}
import com.balakhonov.invest.strategies._

import scala.math.BigDecimal.double2bigDecimal

object StrategyGenerator {

  private[analyzer] def generateStrategy2Strategies: Seq[Strategy] = {
    val list =
    //      for (precision <- 0.001 to 0.5 by 0.05) yield
          for (short <- 5 to 6 by 1) yield
            for (long <- 8 to 10 by 1) yield
              for (t2Short <- 20 to 30 by 1) yield
                for (t2Long <- 30 to 40 by 2) yield
    //      for (delta1 <- 0.3 to 2 by 0.2) yield
    //              for (barsCount1 <- 20 to 60 by 10) yield
    //            for (barsCount2 <- 4 to 6 by 1) yield
    //              for (barsCount3 <- 3 to 5 by 1) yield
              for (t2MinClosedPriceDeltaPerc0 <- 5.5 to 9 by 0.5) yield
                for (t2MinClosedPriceDeltaPerc1 <- 0.7 to 1 by 0.1) yield
                  for (t2MinClosedPriceDeltaPerc2 <- 0.3 to 0.7 by 0.1) yield
      for (stopLossPerc <- 0.1 to 3 by 0.4) yield
//        for (minDeltaPercToSell <- 0.1 to 1 by 0.2) yield
//          for (takeProfitSellDelta <- 0.10 to 0.24 by 0.03) yield {
        {
            Strategy1(
              t1Short = short,
              t1Long = long,
              t2Short = t2Short,
              t2Long = t2Long,
              precision = 0.001, //precision.doubleValue,
              deltaPerc1 = 0.3, // delta1.doubleValue, // 1.4
              barsCount1 = 60, // 30,
              t2MinClosedPriceDeltaPerc2 = t2MinClosedPriceDeltaPerc2.doubleValue, // 0.7
              slSettings = StopLossSettings(
                enabled = true,
                stopLossPerc = stopLossPerc.doubleValue // 0.3
              ),
              tpSettings = Right(TakeProfitSettings(
                enabled = true,
                minDeltaToSellPerc = 0.1, // minDeltaPercToSell.doubleValue, //minDeltaPercToSell.doubleValue, // 0.1
                takeProfitSellDelta = 0.15 // takeProfitSellDelta.doubleValue // 0.15
              ))
            )
          }
    list.flatten.flatten.flatten.flatten.flatten.flatten.flatten.toList
  }

}
