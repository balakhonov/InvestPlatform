package com.balakhonov.invest.services

import ru.tinkoff.invest.openapi.model.rest.PlacedLimitOrder

import scala.concurrent.Future

trait TradePlatform {

  def limitedOrderToBuy(figi: String, price: Double, lots: Int): Future[PlacedLimitOrder]

  def limitedOrderToSelly(figi: String, price: Double, lots: Int): Future[PlacedLimitOrder]

}
