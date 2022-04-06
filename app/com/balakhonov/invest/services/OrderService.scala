package com.balakhonov.invest.services

import cats.implicits.catsSyntaxOptionId
import com.balakhonov.invest.actors.TradePlatformActor
import com.balakhonov.invest.dao.UtilDAO
import com.balakhonov.invest.models._
import com.balakhonov.invest.models.db.{Candle, LimitOrder}
import com.balakhonov.invest.models.enums.{OperationType, OrderStatus}
import com.balakhonov.invest.streams.TradePlatformEventBus
import com.balakhonov.invest.util.PriceUtil
import org.squeryl.customtypes.RichCustomTypeMode.inTransaction

import java.time.LocalDateTime
import javax.inject.{Inject, Singleton}

trait OrderService {
  def buy(candle: Candle,
          pattern: BuyConditions,
          requestedLots: Int)
         (implicit ctx: TradeContext): Unit

  def sell(lotIds: Seq[Int],
           candle: Candle)
          (implicit ctx: TradeContext): Unit
}

@Singleton
case class OrderServiceImpl @Inject()(eventBus: TradePlatformEventBus,
                                      utilDAO: UtilDAO)
  extends OrderService {

  override def buy(candle: Candle,
                   pattern: BuyConditions,
                   requestedLots: Int)
                  (implicit ctx: TradeContext): Unit = {
    if (ctx.bot.value.canBuy) {
      // Save new Order to Local DB
      val limitedOrder = utilDAO.limitOrderDao.insert(LimitOrder(
        id = 0,
        created = LocalDateTime.now(),
        executed = None,
        figi = ctx.instrument.figi,
        conditions = pattern.details,
        orderRef = None,
        operation = OperationType.BUY,
        requestedLots = requestedLots,
        price = candle.closingPrice,
        commission = None,
        status = OrderStatus.NEW,
        executedLots = 0,
        rejectReason = None,
        message = None
      ))

      // Send event to Trade platform
      eventBus.publish(TradePlatformActor.OrderBuy(ctx.instrument, candle.closingPrice, requestedLots = requestedLots, limitedOrder.id))
    }
  }

  override def sell(lotIds: Seq[Int],
                    candle: Candle)
                   (implicit ctx: TradeContext): Unit = {
    if (ctx.bot.value.canSell) {
      inTransaction {
        val lots = utilDAO.lotDao.listByIds(lotIds, withExclusiveLock = true)
          .filter(!_.isMarkedToSell)

        lots.foreach { lot =>
          val netProfit = PriceUtil.calcProfit(ctx.defineLotPrice(lot.price), ctx.defineLotPrice(candle.closingPrice))

          // to lock
          utilDAO.lotDao.update(lot.copy(
            orderSellCreated = LocalDateTime.now().some,
            netProfit = netProfit.some,
            isMarkedToSell = true
          ))
        }

        if (lots.nonEmpty) {
          // Send order to sell async
          eventBus.publish(TradePlatformActor.OrderSell(ctx.instrument, lots.map(_.id), candle.closingPrice))
        }
      }
    }
  }

}

object BuyConditions {
  val RSI: Option[BuyConditions] = BuyConditions("RSI").some
}
case class BuyConditions(details: String)