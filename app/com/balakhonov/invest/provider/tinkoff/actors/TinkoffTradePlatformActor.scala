package com.balakhonov.invest.provider.tinkoff.actors

import akka.actor.typed.scaladsl.ActorContext
import cats.implicits.catsSyntaxOptionId
import com.balakhonov.invest.actors.BotActor._
import com.balakhonov.invest.actors.TradePlatformActor
import com.balakhonov.invest.dao.UtilDAO
import com.balakhonov.invest.models.db.{Bot, Instrument}
import com.balakhonov.invest.provider.ClientProvider
import com.balakhonov.invest.provider.tinkoff._
import com.balakhonov.invest.services.{CandleStoreService, OrderService, TradePlatformOrderService}
import com.balakhonov.invest.streams.TradePlatformEventBus
import io.reactivex.rxjava3.core.Flowable
import io.reactivex.rxjava3.disposables.Disposable
import play.api.inject.ApplicationLifecycle
import ru.tinkoff.invest.openapi.StreamingContext
import ru.tinkoff.invest.openapi.model.streaming.{CandleInterval, StreamingEvent, StreamingRequest}

case class TinkoffTradePlatformActor(publisher: StreamingContext,
                                     actorContext: ActorContext[TradePlatformActor.Command],
                                     orderService: OrderService,
                                     tpOrderService: TradePlatformOrderService,
                                     candleStoreService: CandleStoreService,
                                     eventBus: TradePlatformEventBus,
                                     utilDAO: UtilDAO,
                                     lifecycle: ApplicationLifecycle)
                                    (implicit client: ClientProvider)
  extends TradePlatformActor(
    tradePlatformPrefixName = "tinkoff",
    actorContext = actorContext,
    orderService = orderService,
    tpOrderService = tpOrderService,
    candleStoreService = candleStoreService,
    eventBus = eventBus,
    utilDAO = utilDAO,
    lifecycle = lifecycle) {

  private val logger = context.log

  private var rxSubscription: Option[Disposable] = None

  override def stopPlatformHook(): Unit = {
    logger.info("Disposing StreamingContext..")
    rxSubscription.foreach(_.dispose())
  }

  override def init(): Unit = {
    subscribeOnPublisher()
  }

  override def onPostStopSignal(): Unit = {
    rxSubscription.foreach(_.dispose())
  }

  override def startBot(instrument: Instrument): Unit = {
    // subscribe platform on external events
    publisher.sendRequest(StreamingRequest.subscribeCandle(instrument.figi, CandleInterval._1MIN))
  }

  private def subscribeOnPublisher(): Unit = {
    rxSubscription = Flowable.fromPublisher(publisher)
      .doOnError { ex =>
        logger.error(ex.getMessage, ex)
      }
      .doOnComplete { () =>
        logger.info("doOnComplete")
      }
      .forEach {
        case candle: StreamingEvent.Candle =>
          eventBus.publish(PriceChanged(candle.asScala))

        case ex: StreamingEvent.Error =>
          logger.error(ex.toString, ex)

        case info: StreamingEvent.InstrumentInfo =>
          logger.info(info.toString)

        case orderbook: StreamingEvent.Orderbook =>
          logger.info(orderbook.toString)
      }.some
  }
}
