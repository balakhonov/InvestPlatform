package com.balakhonov.invest.models

import cats.Eval
import com.balakhonov.invest.models.db.{Bot, Instrument, Lot}

case class TradeContext(bot: Eval[Bot],
                        instrument: Instrument,
                        lotsApplicableToSell: Eval[Seq[Lot]],
                        lastBoughtLot: Eval[Option[Lot]],
                        hasActiveOrderToBuy: Eval[Boolean],
                        hasActiveOrderToSell: Eval[Boolean]) {

  def hasLotsToSell: Boolean = lotsApplicableToSell.value.nonEmpty

  def minApplicableToSellLotByPrice: Lot = lotsApplicableToSell.value.minBy(_.price)

  def defineLotPrice(price: Double): LotPrice = LotPrice(price, instrument.lotSize)

  def hasActiveOrder: Boolean = hasActiveOrderToBuy.value || hasActiveOrderToSell.value
}