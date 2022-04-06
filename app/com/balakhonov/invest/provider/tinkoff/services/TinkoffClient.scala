package com.balakhonov.invest.provider.tinkoff.services

import akka.NotUsed
import akka.stream.Materializer
import akka.stream.scaladsl.{Flow, Sink, Source}
import cats.implicits.catsSyntaxOptionId
import com.balakhonov.invest.exception._
import com.balakhonov.invest.models.db.Candle
import com.balakhonov.invest.provider.tinkoff._
import com.balakhonov.invest.util.DateUtil._
import com.balakhonov.invest.util.Executors
import org.reactivestreams.{Subscriber, Subscription}
import ru.tinkoff.invest.openapi.exceptions.OpenApiException
import ru.tinkoff.invest.openapi.model.rest.{CandleResolution, Candles, Currencies, LimitOrderRequest, OperationType, Portfolio, SandboxCurrency, SandboxRegisterRequest, SandboxSetCurrencyBalanceRequest, UserAccounts, MarketInstrument => RInstrument, Operation => ROperation, Order => ROrder, PlacedLimitOrder => RPlacedLimitOrder}
import ru.tinkoff.invest.openapi.model.streaming.StreamingEvent
import ru.tinkoff.invest.openapi.okhttp.OkHttpOpenApi

import java.time.{LocalDate, OffsetDateTime, ZoneOffset, ZonedDateTime}
import java.util.Optional
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}
import scala.jdk.CollectionConverters._
import scala.util.Try

@Singleton
case class TinkoffClient @Inject()()(implicit val mat: Materializer) {
  private val prodToken = ""
  private val sandboxToken = ""
  private val sandboxMode = false

  val api = new OkHttpOpenApi(prodToken, sandboxMode)

  if (api.isSandboxMode)
    api.getSandboxContext.performRegistration(new SandboxRegisterRequest).join

  private implicit val ec: ExecutionContextExecutor = ExecutionContext.fromExecutor(Executors.newFixedThreadPool("tinkoff-client", 4))

  def subscribe(subscriber: Subscriber[Candle]): NotUsed = {
    val candles: Flow[StreamingEvent, Option[Candle], NotUsed] = Flow[StreamingEvent].map(transformToCandle)

    val sb = new Subscriber[Option[Candle]] {
      override def onSubscribe(s: Subscription): Unit = subscriber.onSubscribe(s)

      override def onNext(candle: Option[Candle]): Unit = candle.foreach(subscriber.onNext)

      override def onError(t: Throwable): Unit = subscriber.onError(t)

      override def onComplete(): Unit = subscriber.onComplete()
    }

    Source.fromPublisher(api.getStreamingContext).via(candles).to(Sink.fromSubscriber(sb)).run()
  }

  def listHistory(figi: String,
                  from: OffsetDateTime,
                  to: OffsetDateTime,
                  interval: CandleResolution): Future[Optional[Candles]] = {
    Future {
      api.getMarketContext.getMarketCandles(figi, from, to, interval).join()
    }
  }

  def listMonthlyHistory(figi: String,
                         year: Int,
                         month: Int,
                         interval: CandleResolution): Future[Seq[Candle]] = {
    val fromDate = LocalDate.of(year, month, 1)
    val toDate = fromDate.plusMonths(1).minusDays(1)

    def fetch(date: LocalDate): Future[Seq[Candles]] = {
      Future.fromTry(Try {
        val from = OffsetDateTime.of(year, month, date.getDayOfMonth, 0, 0, 0, 0, ZoneOffset.UTC)
        val to = OffsetDateTime.of(year, month, date.getDayOfMonth, 23, 59, 59, 0, ZoneOffset.UTC)

        listHistory(figi, from, to, interval)
      }).flatten
        .map { opt =>
          Option(opt.orElseGet(None.orNull)).toSeq
        }
    }

    FutureUtil.serialiseFutures(fromDate.to(toDate), Some(100))(fetch)
      .map(_.flatMap(_.getCandles.asScala.map(_.asScala)).toList)
  }

  def listYearlyHistory(figi: String,
                        year: Int,
                        interval: CandleResolution): Future[Seq[Candle]] = {

    def fetch(month: Int): Future[Seq[Candle]] = {
      listMonthlyHistory(figi, year, month, interval).map { list =>
        println(s"listMonthlyHistory $year, $month ${list.size}")
        list
      }
    }

    val range = Range.apply(1, 13)

    FutureUtil.serialiseFutures(list = range, delayOpt = Some(4000))(fetch)
  }

  def listInstruments: Future[List[RInstrument]] = {
    Future {
      val market = api.getMarketContext.getMarketStocks.join
      market.getInstruments.asScala.toList
    }
  }

  def clearSandbox: Future[Unit] = {
    Future[Unit] {
      api.getSandboxContext.clearAll(null).join()
    }
  }

  def getAccounts: Future[UserAccounts] = {
    Future {
      api.getUserContext.getAccounts.join()
    }
  }

  def getPortfolio: Future[Portfolio] = {
    Future {
      api.getPortfolioContext.getPortfolio(null).join()
    }
  }

  def getPortfolioCurrencies: Future[Currencies] = {
    Future {
      api.getPortfolioContext.getPortfolioCurrencies(null).join()
    }
  }

  def resetSandboxCurrencyBalance: Future[Unit] = {
    Future[Unit] {
      var req = new SandboxSetCurrencyBalanceRequest()
        .currency(SandboxCurrency.RUB)
        .balance(new java.math.BigDecimal(300000d))
      api.getSandboxContext.setCurrencyBalance(req, null).join()

      req = new SandboxSetCurrencyBalanceRequest()
        .currency(SandboxCurrency.USD)
        .balance(new java.math.BigDecimal(300000d))
      api.getSandboxContext.setCurrencyBalance(req, null).join()
    }
  }

  def listActiveOrders: Future[List[ROrder]] = {
    Future {
      val list = api.getOrdersContext.getOrders(null).join()
      list.asScala.toList
    }
  }

  def mapOrders: Future[Map[String, List[ROrder]]] = {
    listActiveOrders.map(_.groupBy(_.getFigi).withDefaultValue(Nil))
  }

  def listOperations(figi: String,
                     from: ZonedDateTime,
                     to: ZonedDateTime): Future[List[ROperation]] = {
    Future {
      val operations = api.getOperationsContext.getOperations(from.toOffsetDateTime, to.toOffsetDateTime, figi, null).join()
      operations.getOperations.asScala.toList
    }
  }

  def limitedOrderToBuy(figi: String, price: Double, lots: Int): Future[RPlacedLimitOrder] = {
    val req = new LimitOrderRequest()
      .operation(OperationType.BUY)
      .price(new java.math.BigDecimal(price))
      .lots(lots)

    placeLimitOrder(None.orNull, figi, req)
      .recoverWith {
        case ex: OpenApiException if ex.getMessage == "Недостаточно активов для маржинальной сделки" =>
          Future.failed(NotEnoughMoneyToExecuteOrder(figi, "Недостаточно активов для маржинальной сделки", Option(ex.getCode)))

        case ex: OpenApiException if ex.getMessage == "Instrument is disabled for trading" =>
          Future.failed(InstrumentIsDisabledForTrading(figi, "Недостаточно активов для маржинальной сделки", Option(ex.getCode)))

        case ex: OpenApiException =>
          Future.failed(OrderException(figi, "Недостаточно активов для маржинальной сделки", Option(ex.getCode)))
      }
  }

  def limitedOrderToSell(figi: String, price: Double, lots: Int): Future[RPlacedLimitOrder] = {
    val req = new LimitOrderRequest()
      .operation(OperationType.SELL)
      .price(new java.math.BigDecimal(price))
      .lots(lots)

    placeLimitOrder(None.orNull, figi, req)
      .recoverWith {
        case ex: OpenApiException if ex.getMessage == "Недостаточно активов для маржинальной сделки" =>
          Future.failed(NotEnoughMoneyToExecuteOrder(figi, "Недостаточно активов для маржинальной сделки", Option(ex.getCode)))

        case ex: OpenApiException =>
          Future.failed(OrderException(figi, "Недостаточно активов для маржинальной сделки", Option(ex.getCode)))
      }
  }

  def orderToClose(orderRef: String): Future[Unit] = {
    Future[Unit] {
      api.getOrdersContext.cancelOrder(orderRef, None.orNull).join()
    }.recoverWith {
      case ex: OpenApiException if ex.getMessage == "Поручение уже было снято или исполнено" =>
        Future.failed(OrderAlreadyCancelledOrApplied(orderRef, s"Поручение уже было снято или исполнено для Order($orderRef)", Option(ex.getCode)))
    }
  }

  private def placeLimitOrder(brokerAccountId: String,
                              figi: String,
                              req: LimitOrderRequest): Future[RPlacedLimitOrder] = {
    Future {
      api.getOrdersContext.placeLimitOrder(figi, req, brokerAccountId).join()
    }
  }

  private def transformToCandle(se: StreamingEvent): Option[Candle] = {
    se match {
      case candle: StreamingEvent.Candle =>
        candle.asScala.some
      case _: StreamingEvent.Error =>
        None
      case _: StreamingEvent.InstrumentInfo =>
        None
      case _: StreamingEvent.Orderbook =>
        None
      case _ =>
        None
    }
  }

}

object FutureUtil {
  def serialiseFutures[A, B](list: Seq[A], delayOpt: Option[Int] = None)(func: A => Future[Seq[B]]): Future[Seq[B]] = {
    implicit val singleThreadEc: ExecutionContextExecutor = {
      ExecutionContext.fromExecutor(Executors.newFixedThreadPool("single-thread-pool", 1))
    }

    val empty = Future[Seq[B]](Seq.empty[B])

    list.iterator.foldLeft(empty) { case (fAccum, item) =>
      fAccum.flatMap { accList =>
        delayOpt.foreach { delay =>
          //          println(s"sleep($delay)..")
          Thread.sleep(delay)
        }
        func(item).map { list =>
          accList ++ list
        }
      }
    }
  }
}