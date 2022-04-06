package com.balakhonov.invest.models

case class TakeProfitSettings(enabled: Boolean,
                              minDeltaToSellPerc: Double,
                              takeProfitSellDelta: Double)