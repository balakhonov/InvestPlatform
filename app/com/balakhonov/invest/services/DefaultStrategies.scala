package com.balakhonov.invest.services

import com.balakhonov.invest.StrategyBuilder
import com.balakhonov.invest.models.StopLossSettings
import com.balakhonov.invest.strategies._

object DefaultStrategies {

  def defineStrategy(code: String): StrategyBuilder = {
    code match {
      case "S1" =>
        DefaultStrategies.s32
      case "RSI" =>
        DefaultStrategies.rsi
    }
  }

  def s32: StrategyBuilder = () => Strategy1(5, 8, 25, 40, 0.001, 0.3, 320, 0.2, StopLossSettings(enabled = true, 0.30), Left(0.1))

  def rsi: StrategyBuilder = () => RSIStrategy(FirstBuySettings(1.2, 75), 0.5, 0.9, 12, 12, 120, 10, 450, 45, UptrendOptions(0.4, 18, 29), DowntrendOptions(enabled = true, 1.0, 50, 29))
}
