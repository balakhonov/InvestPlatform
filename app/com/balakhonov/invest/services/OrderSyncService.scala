package com.balakhonov.invest.services

import cats.implicits.catsSyntaxOptionId
import com.balakhonov.invest.actors.BotActor
import com.balakhonov.invest.exception.OrderAlreadyCancelledOrApplied
import com.balakhonov.invest.models.db.{LimitOrder, Lot}
import com.balakhonov.invest.models.enums.OrderStatus
import com.balakhonov.invest.models.{Order, db}
import com.balakhonov.invest.util.Executors
import org.squeryl.customtypes.RichCustomTypeMode.inTransaction

import java.time.LocalDateTime
import scala.concurrent._
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Try}

object OrderSyncService {
  implicit val ec: ExecutionContextExecutor = ExecutionContext.fromExecutor(Executors.newFixedThreadPool("order-service", 4))
}

trait OrderSyncService {
  bot: BotActor =>

  import OrderSyncService.ec

  protected def syncOrders(orders: Seq[Order]): Unit = {
    withLogger {
      val OrderExecutionTimeoutMin = 4
      val ordersToBuy = utilDAO.limitOrderDao.listNewOnBuy(instrument.figi.some)
      val lotsToSell = utilDAO.lotDao.listMarkedToSell(instrument.figi.some)

      if (ordersToBuy.nonEmpty || lotsToSell.nonEmpty) {
        syncOrdersOnBuy(ordersToBuy, orders, OrderExecutionTimeoutMin)

        syncOrdersOnSell(lotsToSell, orders, OrderExecutionTimeoutMin)
      }
    }
  }

  private def syncOrdersOnBuy(limitedOrders: Seq[LimitOrder],
                              orders: Seq[Order],
                              timeoutMinutes: Int): Unit = {
    limitedOrders.foreach { lo =>
      lo.orderRef.fold[Unit] {
        Option.when(lo.created.isBefore(LocalDateTime.now().minusMinutes(timeoutMinutes))) {
          logger.trace(s"Закрываем BUY заявку(${lo.orderRef}) по таймауту.")
          cancelBuyOrder(lo)
        }
      } { orderRef =>
        orders
          .find(_.orderRef == orderRef)
          .fold[Unit] {
            logger.trace("Order was not found in active orders on external platform. Считаем что он был исполнен. ")
            markAsBought(lo, LocalDateTime.now)
          } { order =>
            Option.when(lo.created.isBefore(LocalDateTime.now().minusMinutes(timeoutMinutes))) {
              logger.trace(s"Закрываем BUY заявку(${order.orderRef}) по таймауту.")
              val resultF = client.orderToClose(order.orderRef)
                .map { _ =>
                  cancelBuyOrder(lo)
                }.recoverWith {
                case OrderAlreadyCancelledOrApplied(figi, msg, code) =>
                  logger.warn(s"Can't close LimitOrder(${lo.id}, $figi) code:$code. $msg")
                  markAsBought(lo, LocalDateTime.now)
                  Future.unit
                case ex: Throwable =>
                  logger.error(s"Can't close LimitOrder(${lo.id}, ${instrument.figi}). ${ex.getMessage}")
                  Future.failed(ex)
              }

              Try(Await.result(resultF, 1.minutes)) match {
                case Failure(_: TimeoutException) =>
                  logger.error(s"TimeoutException occurred while closing LimitOrder(${lo.id}, ${instrument.figi} ${instrument.name}).")
                case Failure(ex: Throwable) =>
                  logger.error(s"Exception occurred while closing LimitOrder(${lo.id}, ${instrument.figi} ${instrument.name}). ${ex.getMessage}", ex)
                case _ => // no code
              }
            }
          }
      }
    }
  }

  private def syncOrdersOnSell(markedToSellLots: Seq[Lot],
                               orders: Seq[Order],
                               timeoutMinutes: Int): Unit = {
    markedToSellLots
      .groupBy(_.orderSellRef)
      .toList
      .foreach { case (orderSellRefOpt, lots) =>
        orderSellRefOpt match {
          case Some(orderSellRef) =>
            orders
              .find(_.orderRef == orderSellRef)
              .fold {
                logger.trace("Order was not found in active orders on external platform. Считаем что он был исполнен. ")
                lots.foreach { lot =>
                  markAsSold(lot)
                }
              } { order =>
                Option.when(lots.head.orderSellCreated.forall(_.isBefore(LocalDateTime.now().minusMinutes(timeoutMinutes)))) {
                  // Закрываем только то количество, что осталось неисполненным
                  val countToClose = order.requestedLots - order.executedLots

                  val future = client.orderToClose(order.orderRef)
                    .map { _ =>
                      lots.take(countToClose).foreach { lot =>
                        logger.trace(s"Закрываем SELL заявку(${lot.orderSellRef}) по таймауту.")
                        cancelSellOrder(lot)
                      }
                    }.recover {
                    case ex: Throwable =>
                      logger.error(s"Can't close Sell Order(${order.orderRef}, ${instrument.figi} ${instrument.name}). ${ex.getMessage}")
                  }

                  Try(Await.result(future, 1.minutes)) match {
                    case Failure(_: TimeoutException) =>
                      logger.error(s"TimeoutException occurred while closing Sell Order(${order.orderRef}, ${instrument.figi} ${instrument.name}).")
                    case Failure(ex: Throwable) =>
                      logger.error(s"Exception occurred while closing Sell Order(${order.orderRef}, ${instrument.figi} ${instrument.name}). ${ex.getMessage}", ex)
                    case _ => // no code
                  }
                }
              }

          case None =>
            // лот был зарезервирован на продажу, но ответ не был возвращен брокером
            lots.foreach { lot =>
              Option.when(lot.orderSellCreated.forall(_.isBefore(LocalDateTime.now().minusMinutes(timeoutMinutes)))) {
                logger.trace(s"Закрываем SELL заявку(${lot.orderSellRef}) по таймауту.")
                cancelSellOrder(lot)
              }
            }
        }
      }
  }

  private def cancelBuyOrder(limitOrder: LimitOrder): Unit = {
    utilDAO.limitOrderDao.update(limitOrder.copy(
      status = OrderStatus.CANCELLED,
      executedLots = 0
    ))
  }

  private def cancelSellOrder(lot: Lot): Unit = {
    utilDAO.lotDao.update(lot.copy(
      orderSellRef = None,
      netProfit = None,
      isSold = false,
      isMarkedToSell = false
    ))
  }

  private def markAsBought(limitOrder: LimitOrder,
                           executed: LocalDateTime): Unit = {
    inTransaction {
      utilDAO.limitOrderDao.update(limitOrder.copy(
        executed = Some(executed),
        status = OrderStatus.FILL,
        executedLots = limitOrder.requestedLots
      ))

      for (_ <- 1 to limitOrder.requestedLots) {
        utilDAO.lotDao.insert(db.Lot(
          id = 0,
          created = executed,
          orderId = limitOrder.id,
          figi = limitOrder.figi,
          orderRef = limitOrder.orderRef.get,
          price = limitOrder.price,
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

  private def markAsSold(lot: Lot): Unit = {
    utilDAO.lotDao.update(lot.copy(
      orderSellExecuted = Some(LocalDateTime.now),
      isSold = true
    ))
  }
}
