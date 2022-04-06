package com.balakhonov.invest.models.enums

object OperationType
  extends Enumeration {

  type OperationType = Value

  val BUY: Value = Value(1, "Buy")
  val SELL: Value = Value(2, "Sell")

}
