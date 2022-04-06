package com.balakhonov.invest.util

object DoubleUtil {
  def to2p(value: Double): Double = (math rint value * 100) / 100

  def to3p(value: Double): Double = (math rint value * 1000) / 1000
}