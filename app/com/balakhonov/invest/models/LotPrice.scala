package com.balakhonov.invest.models

case class LotPrice(private val price: BigDecimal,
                    units: Int) {
  def totalPrice: Double = (price * units).doubleValue
}