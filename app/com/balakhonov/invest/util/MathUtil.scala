package com.balakhonov.invest.util

import org.apache.commons.math3.stat.regression.SimpleRegression

import java.lang.Math.max
import scala.collection.mutable

object MathUtil {

  def linest(x: Array[Double], y: Seq[Double]): (Double, Double) = {
    val data = {
      val b = new mutable.ArrayBuilder.ofRef[Array[Double]]()
      val k = y.knownSize
      b.sizeHint(max(k, x.length))
      var i = 0
      val it = y.iterator
      while (i < x.length && it.hasNext) {
        b += Array(x(i), it.next())
        i += 1
      }
      while (it.hasNext) {
        b += Array(0, it.next())
        i += 1
      }
      while (i < x.length) {
        b += Array(x(i), 0)
        i += 1
      }
      b.result()
    }

    val regression = new SimpleRegression
    regression.addData(data)
    regression.getIntercept -> regression.getSlope
  }
}
