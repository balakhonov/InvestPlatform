package com.balakhonov.invest.util

import cats.Eval
import com.balakhonov.invest.models._
import com.balakhonov.invest.models.db.{Candle, Lot}

object PriceUtil {

  def calcProfitPerc(candle: Candle,
                     lot: Lot)
                    (implicit ctx: TradeContext): Double = {
    calcProfitPerc(
      buy = LotPrice(lot.price, ctx.instrument.lotSize),
      sell = LotPrice(candle.closingPrice, ctx.instrument.lotSize)
    )
  }

  def calcProfitPerc(candle: Candle)
                    (implicit ctx: TradeContext): Double = {
    Option.when(ctx.hasLotsToSell) {
      calcProfitPerc(
        buy = LotPrice(ctx.minApplicableToSellLotByPrice.price, ctx.instrument.lotSize),
        sell = LotPrice(candle.closingPrice, ctx.instrument.lotSize)
      )
    }.getOrElse(0d)
  }

  def calcDeficitPerc(candle: Candle,
                      highestPriceForPeriod: Eval[Option[Double]] = Eval.now(None))
                     (implicit ctx: TradeContext): Double = {
    ctx.lastBoughtLot.value match {
      case Some(lastBoughtLot) if ctx.hasLotsToSell =>
        val deltaPrice = (ctx.minApplicableToSellLotByPrice.price + lastBoughtLot.price) / 2
        calcDeficitPerc(
          buy = LotPrice(deltaPrice, ctx.instrument.lotSize),
          sell = LotPrice(candle.closingPrice, ctx.instrument.lotSize)
        )

      case Some(lastBoughtLot) =>
        val highestPrice = highestPriceForPeriod.value.fold(lastBoughtLot.price)(p => Math.max(p, lastBoughtLot.price))
        calcDeficitPerc(
          buy = LotPrice(highestPrice, ctx.instrument.lotSize),
          sell = LotPrice(candle.closingPrice, ctx.instrument.lotSize)
        )

      case None =>
        highestPriceForPeriod.value.fold(0d) { highestPriceForPeriod =>
          calcDeficitPerc(
            buy = LotPrice(highestPriceForPeriod, ctx.instrument.lotSize),
            sell = LotPrice(candle.closingPrice, ctx.instrument.lotSize)
          )
        }
    }
  }

  def calcProfit(candle: Candle)
                (implicit ctx: TradeContext): Double = {
    calcProfit(
      buy = LotPrice(ctx.minApplicableToSellLotByPrice.price, ctx.instrument.lotSize),
      sell = LotPrice(candle.closingPrice, ctx.instrument.lotSize)
    )
  }

  def calcProfitPerc(buy: LotPrice,
                     sell: LotPrice): Double = {
    val s = sell.totalPrice - calcCommission(sell)
    val b = buy.totalPrice + calcCommission(buy)
    ((s / b - 1) * 100d)
      .setScale(2, BigDecimal.RoundingMode.DOWN)
      .doubleValue
  }

  def calcDeficitPerc(buy: LotPrice,
                      sell: LotPrice): Double = {
    val s = sell.totalPrice - calcCommission(sell)
    val b = buy.totalPrice - calcCommission(buy)
    ((1 - s / b) * 100d)
      .setScale(2, BigDecimal.RoundingMode.DOWN)
      .doubleValue
  }

  def calcProfit(buy: LotPrice,
                 sell: LotPrice): Double = {
    val s = sell.totalPrice - calcCommission(sell)
    val b = buy.totalPrice + calcCommission(buy)
    (s - b)
      .setScale(2, BigDecimal.RoundingMode.DOWN)
      .doubleValue
  }

  def calcCommission(lotPrice: LotPrice): BigDecimal = {
    BigDecimal(lotPrice.totalPrice * Account.TransactionCommission)
      .setScale(2, BigDecimal.RoundingMode.UP)
  }

}
