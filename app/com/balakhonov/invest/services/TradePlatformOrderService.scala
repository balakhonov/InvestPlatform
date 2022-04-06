package com.balakhonov.invest.services

import cats.implicits.catsSyntaxOptionId
import com.balakhonov.invest.actors.TradePlatformActor.{OrderBuy, OrderSell}
import com.balakhonov.invest.dao.UtilDAO
import com.balakhonov.invest.exception._
import com.balakhonov.invest.models._
import com.balakhonov.invest.models.db.Lot
import com.balakhonov.invest.models.enums.{OperationStatus, OperationType, OrderStatus}
import com.balakhonov.invest.provider.ClientProvider
import com.balakhonov.invest.util.ColorHelper.{withGREEN, withRED}
import com.balakhonov.invest.util.DoubleUtil.to3p
import org.squeryl.customtypes.RichCustomTypeMode.inTransaction
import play.api.Logger

import java.time.ZonedDateTime
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
case class TradePlatformOrderService @Inject()(utilDAO: UtilDAO) {
  private val LOG = Logger(getClass)

  def orderToBuy(ob: OrderBuy)
                (implicit client: ClientProvider,
                 ec: ExecutionContext): Future[Unit] = {
    val requestTime = ZonedDateTime.now()

    def handleException: PartialFunction[Throwable, Unit] = {
      case NotEnoughMoneyToExecuteOrder(figi, msg, code) =>
        LOG.error(s"NotEnoughMoneyToExecuteOrder occurred while OrderBuy(${ob.limitedOrderId}, $figi) operation. $msg, code: $code")
        utilDAO.limitOrderDao.remove(ob.limitedOrderId)

      case InstrumentIsDisabledForTrading(figi, msg, code) =>
        LOG.error(s"OpenApiException occurred while OrderBuy(${ob.limitedOrderId}, $figi) operation. $msg, code: $code")
        utilDAO.limitOrderDao.remove(ob.limitedOrderId)

      case ex@OrderException(figi, msg, code) =>
        LOG.error(s"OpenApiException occurred while OrderBuy(${ob.limitedOrderId}, $figi) operation. $msg, code: $code", ex)
        utilDAO.limitOrderDao.remove(ob.limitedOrderId)

      case ex: com.fasterxml.jackson.core.JsonParseException =>
        LOG.error(s"JsonParseException occurred while OrderBuy(${ob.limitedOrderId}, ${ob.instrument.figi}) operation. ${ex.getMessage}")
        syncLastBuyOrder(ob, requestTime)

      case ex: java.util.concurrent.CompletionException =>
        LOG.error(s"CompletionException occurred while OrderBuy(${ob.limitedOrderId}, ${ob.instrument.figi}) operation. ${ex.getMessage}")
        syncLastBuyOrder(ob, requestTime)

      case ex: java.net.SocketTimeoutException =>
        LOG.error(s"SocketTimeoutException occurred while OrderBuy(${ob.limitedOrderId}, ${ob.instrument.figi}) operation. ${ex.getMessage}")
        syncLastBuyOrder(ob, requestTime)

      case ex =>
        LOG.error(s"Exception occurred while OrderBuy(${ob.limitedOrderId}, ${ob.instrument.figi}) operation. ${ex.getMessage}", ex)
        utilDAO.limitOrderDao.remove(ob.limitedOrderId)
    }

    client.limitedOrderToBuy(ob.instrument.figi, ob.price, ob.requestedLots)
      .map { order =>
        handleOrderToBuyResponse(
          ob = ob,
          orderId = order.orderRef,
          status = order.status,
          commission = order.commission,
          executedLots = order.executedLots,
          rejectReason = order.rejectReason,
          message = order.message
        )
      }
      .recover(handleException)
  }

  def orderToSell(os: OrderSell)
                 (implicit client: ClientProvider,
                  ec: ExecutionContext): Future[Unit] = {
    def handleException: PartialFunction[Throwable, Unit] = {
      def resetLots(): Unit = {
        inTransaction {
          val lots = utilDAO.lotDao.listByIds(os.lotIds, withExclusiveLock = true)

          lots.foreach { lot =>
            utilDAO.lotDao.update(lot.copy(
              orderSellRef = None,
              netProfit = None,
              isSold = false,
              isMarkedToSell = false
            ))
          }
        }
      }

      val result: PartialFunction[Throwable, Unit] = {
        case NotEnoughMoneyToExecuteOrder(figi, msg, code) =>
          LOG.error(s"NotEnoughMoneyToExecuteOrder occurred while OrderSell operation. Lots(${os.lotIds}). $msg, code: $code")
          resetLots()

        case ex@OrderException(figi, msg, code) =>
          LOG.error(s"OpenApiException occurred while OrderSell operation. Lots(${os.lotIds}). $msg, code: $code", ex)
          resetLots()

        case ex =>
          LOG.error(s"Exception occurred while OrderSell operation. Lots(${os.lotIds}). ${ex.getMessage}", ex)
          resetLots()
      }

      result
    }

    client.limitedOrderToSell(os.instrument.figi, os.price, os.lotIds.size)
      .map(handleOrderToBuyResponse(os, _))
      .recover(handleException)
  }

  private def handleOrderToBuyResponse(ob: OrderBuy,
                                       orderId: String,
                                       status: OrderStatus.Value,
                                       commission: Option[Double],
                                       executedLots: Int,
                                       rejectReason: Option[String],
                                       message: Option[String]): Unit = {
    status match {
      case OrderStatus.NEW =>
        LOG.info(s"[${withRED("BUY")}($status)] [${ob.instrument.name}] [C: ${ob.price}] [ID: $orderId]")

        inTransaction {
          val limitedOrder = utilDAO.limitOrderDao.getById(ob.limitedOrderId, withExclusiveLock = true)

          val updatedLimitedOrder = limitedOrder.copy(
            orderRef = orderId.some,
            commission = commission,
            status = status,
            executedLots = executedLots,
            rejectReason = rejectReason,
            message = message
          )

          utilDAO.limitOrderDao.update(updatedLimitedOrder)
        }

      case _ =>
        LOG.warn(s"[${withRED("BUY")}($status)] [${ob.instrument.name}] [C: ${ob.price}] [ID: $orderId] [$rejectReason] [$message]")
    }
  }

  private def handleOrderToBuyResponse(os: OrderSell,
                                       order: PlacedLimitOrder): Unit = {
    order.status match {
      case OrderStatus.NEW =>
        inTransaction {
          val lots = utilDAO.lotDao.listByIds(os.lotIds, withExclusiveLock = true)

          lots.foreach { lot =>
            LOG.info(s"[${withGREEN("SELL")}(${order.status})] [${os.instrument.name}] [C: ${os.price} x${os.lotIds.size}] [P: ${to3p(lot.netProfit.get)}] [ID: ${order.orderRef}]")

            utilDAO.lotDao.update(lot.copy(
              orderSellRef = order.orderRef.some,
            ))
          }
        }

      case _ =>
        LOG.warn(s"[${withGREEN("SELL")}(${order.status})] [${os.instrument.name}] [C: ${os.price} x${os.lotIds.size}] [ID: ${order.orderRef}]")
    }
  }

  private def syncLastBuyOrder(ob: OrderBuy,
                               requestTime: ZonedDateTime)
                              (implicit client: ClientProvider,
                               ec: ExecutionContext): Unit = {
    LOG.info("Ищем в активных заявках")
    val resultF = client.listActiveOrders

    resultF.foreach { list =>
      val activeOrderOpt = list
        .filter(_.figi == ob.instrument.figi)
        .find(_.operation == OperationType.BUY)

      activeOrderOpt match {
        case Some(order) =>
          handleOrderToBuyResponse(
            ob = ob,
            orderId = order.orderRef,
            status = order.status,
            commission = None,
            executedLots = order.executedLots,
            rejectReason = None,
            message = None
          )

        case None =>
          LOG.info("Ищем в исполненных операциях")
          val resultF = client.listOperations(ob.instrument.figi, requestTime, ZonedDateTime.now())

          resultF.foreach { list =>
            list.headOption.fold {
              // скорей всего запрос не был обработан брокером
              utilDAO.limitOrderDao.remove(ob.limitedOrderId)
            } { operation =>
              operation.status match {
                case OperationStatus.DONE =>
                  LOG.info("Помечаем заявку как обработанную и создаем лот")
                  markAsBought(ob, operation)

                case OperationStatus.DECLINE =>
                  LOG.info("Ищем в исполненных операциях")
                  utilDAO.limitOrderDao.remove(ob.limitedOrderId)

                case OperationStatus.PROGRESS =>
                  LOG.info("Операция в статусе PROGRESS")
              }
            }
          }
      }
    }
  }

  private def markAsBought(ob: OrderBuy,
                           operation: Operation): Unit = {
    val executed = operation.date

    inTransaction {
      val limitOrder = utilDAO.limitOrderDao.getById(ob.limitedOrderId, withExclusiveLock = true)

      utilDAO.limitOrderDao.update(limitOrder.copy(
        executed = Some(executed),
        status = OrderStatus.FILL,
        executedLots = limitOrder.requestedLots
      ))

      for (_ <- 1 to limitOrder.requestedLots) {
        utilDAO.lotDao.insert(Lot(
          id = 0,
          created = executed,
          orderId = limitOrder.id,
          figi = limitOrder.figi,
          orderRef = limitOrder.orderRef.getOrElse("-"),
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
}
