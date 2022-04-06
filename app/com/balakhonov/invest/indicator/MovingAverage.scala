package com.balakhonov.invest.indicator

import com.balakhonov.invest._

import scala.math.abs


object SMA
  extends SMA
    with TrendCalculator

trait SMA {
  def calculate(period: Int, _ls: Seq[Double]): Double = {
    val ls = _ls.takeRight(period)

    if (ls.isEmpty) 0.0 else ls.sum / ls.size
  }
}

object EMA
  extends EMA
    with TrendCalculator

trait EMA {
  def calculate(period: Int, ls: Seq[Double]): Double = {
    val k = 2.0 / (period + 1)

    ls.takeRight(period).foldLeft(SMA.calculate(period, ls))(
      (last, s) => (1 - k) * last + k * s
    )
  }
}

trait TrendCalculator {

  def calculate(days: Int, ls: Seq[Double]): Double

  def trend(short: Int,
            long: Int,
            ls: Seq[Double],
            precision: Double): Trend = {
    val shortMA = calculate(short, ls)
    val longMA = calculate(long, ls)
    val delta = (1 - longMA / shortMA) * 100

    if (abs(delta) < precision)
      Flatline
    else if (delta < 0)
      Downtrend
    else
      Uptrend(Double.NaN)
  }
}