package com.balakhonov.invest.indicator

import com.balakhonov.invest.util.MathUtil
import com.balakhonov.invest.{Downtrend, Flatline, Trend, Uptrend}

import scala.math.abs

object TrendDetector {
  type ClosedPrice = Double

  def calc(bars: Seq[ClosedPrice], multiplier: Int = 1): Trend = {
    if (bars.length < 2) {
      Flatline
    } else {
      val periods = Array.tabulate[Double](bars.length)(_ + 1)
      // y = a + b*x
      val (a, b) = MathUtil.linest(periods, bars)

      val yValues = periods.map(a + b * _)
      val minYValue = yValues.min

      val percentages = yValues.map { v =>
        (v - minYValue) / v * 100
      }

      //    percentages.foreach(println(_, "%"))

      val headY = percentages.head
      val lastY = percentages.last
      val headX = periods.head
      val lastX = periods.last

      val angle = Math.toDegrees(Math.atan2(lastY - headY, (lastX - headX) / multiplier))
      if (abs(angle) < 2)
        Flatline
      else
        if (angle < 0)
        Downtrend
      else
        Uptrend(Double.NaN)
    }
  }
}
