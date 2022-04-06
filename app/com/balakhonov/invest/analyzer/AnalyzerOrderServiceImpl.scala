package com.balakhonov.invest.analyzer

import cats.implicits.catsSyntaxOptionId
import com.balakhonov.invest.dao.UtilDAO
import com.balakhonov.invest.models.TradeContext
import com.balakhonov.invest.models.db._
import com.balakhonov.invest.models.enums.{OperationType, OrderStatus}
import com.balakhonov.invest.services.{BuyConditions, OrderService}
import com.balakhonov.invest.util.ColorHelper._
import com.balakhonov.invest.util.DoubleUtil.to3p
import com.balakhonov.invest.util.PriceUtil
import play.api.Logger

import java.time.LocalDateTime
import javax.inject.{Inject, Singleton}

@Singleton
case class AnalyzerOrderServiceImpl @Inject()(utilDAO: UtilDAO)
  extends OrderService {
  private val LOG = Logger(this.getClass)

  override def buy(candle: Candle,
                   pattern: BuyConditions,
                   requestedLots: Int)
                  (implicit ctx: TradeContext): Unit = {
    if (ctx.bot.value.canBuy) {
      LOG.debug(s"[${candle.dateTime}] [${withRED("BUY")}] [${ctx.instrument.name}] [C: ${candle.closingPrice}]")

      // Save new Order to Local DB
      val limitedOrder = utilDAO.limitOrderDao.insert(LimitOrder(
        id = 0,
        created = LocalDateTime.now(),
        executed = Some(LocalDateTime.now()),
        figi = ctx.instrument.figi,
        conditions = pattern.details,
        orderRef = Some(s"${candle.id}-ref"),
        operation = OperationType.BUY,
        requestedLots = requestedLots,
        price = candle.closingPrice,
        commission = None,
        status = OrderStatus.FILL,
        executedLots = requestedLots,
        rejectReason = None,
        message = None
      ))
      // save lots
      for (_ <- 1 to requestedLots) {
        utilDAO.lotDao.insert(Lot(
          id = 0,
          created = LocalDateTime.now(),
          orderId = limitedOrder.id,
          figi = limitedOrder.figi,
          orderRef = limitedOrder.orderRef.getOrElse("-"),
          price = limitedOrder.price,
          commission = None,
          orderSellCreated = None,
          orderSellExecuted = None,
          orderSellRef = None,
          netProfit = None,
          isSold = false,
          isMarkedToSell = false
        ))
      }
    }
  }

  override def sell(lotIds: Seq[Int],
                    candle: Candle)
                   (implicit ctx: TradeContext): Unit = {
    if (ctx.bot.value.canSell) {
      val lots = utilDAO.lotDao.listByIds(lotIds, withExclusiveLock = true)
        .filter(!_.isMarkedToSell)

      lots.foreach { lot =>
        val netProfit = PriceUtil.calcProfit(ctx.defineLotPrice(lot.price), ctx.defineLotPrice(candle.closingPrice))

        LOG.debug(s"[${candle.dateTime}] [${withGREEN("SELL")}] [${ctx.instrument.name}] [C: ${candle.closingPrice} x1] [P: ${to3p(netProfit)}]")

        // to lock
        utilDAO.lotDao.update(lot.copy(
          orderSellCreated = LocalDateTime.now().some,
          orderSellExecuted = Some(LocalDateTime.now),
          orderSellRef = s"${candle.id}-ref".some,
          netProfit = netProfit.some,
          isSold = true,
          isMarkedToSell = true
        ))
      }
    }
  }

}
