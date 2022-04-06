package com.balakhonov.invest.util

import java.text.DecimalFormat

object FormatUtil {

  private val _2Decimals = new DecimalFormat("#.00")
  private val _4Decimals = new DecimalFormat("#.0000")

  def format2D(value: Int): String = "%02d".format(value)

  def format2Decimals(value: Double): String = _2Decimals.format(value)

  def format4Decimals(value: Double): String = _4Decimals.format(value)

}
