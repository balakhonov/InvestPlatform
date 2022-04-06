package com.balakhonov.invest.models.enums

object CandleTimeFrame
  extends Enumeration {

  type CandleTimeFrame = Value

  val _1MIN: Value = Value(1, "1min")
  val _2MIN: Value = Value(2, "2min")
  val _3MIN: Value = Value(3, "3min")
  val _5MIN: Value = Value(4, "5min")
  val _10MIN: Value = Value(5, "10min")
  val _15MIN: Value = Value(6, "15min")
  val _30MIN: Value = Value(7, "30min")
  val HOUR: Value = Value(8, "hour")
  val _2HOUR: Value = Value(9, "2hour")
  val _4HOUR: Value = Value(10, "4hour")
  val DAY: Value = Value(11, "day")
  val WEEK: Value = Value(12, "week")
  //  val MONTH: Value = Value("month")

}
