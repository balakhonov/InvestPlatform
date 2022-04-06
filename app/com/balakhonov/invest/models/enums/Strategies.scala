package com.balakhonov.invest.models.enums

object Strategies
  extends Enumeration {

  type Strategies = Value

  val SCALP_1M: Value = Value(1, "S1")
  val RSI_10M: Value = Value(2, "S2")

}
