package com.balakhonov.invest.provider.tinkoff.actors

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import com.balakhonov.invest.actors.TradePlatformActor
import com.balakhonov.invest.dao.UtilDAO
import com.balakhonov.invest.db.DBManager
import com.balakhonov.invest.provider.tinkoff.TinkoffClientProvider
import com.balakhonov.invest.services.{CandleStoreService, OrderService, TradePlatformOrderService}
import com.balakhonov.invest.streams.TradePlatformEventBus
import com.google.inject.Provides
import play.api.inject.ApplicationLifecycle
import play.api.libs.concurrent.ActorModule

class TinkoffTradePlatformActorFactory
  extends ActorModule {
  type Message = TradePlatformActor.Command

  @Provides
  def create(dbManager: DBManager,
             orderService: OrderService,
             tpOrderService: TradePlatformOrderService,
             candleStoreService: CandleStoreService,
             eventBus: TradePlatformEventBus,
             utilDAO: UtilDAO,
             client: TinkoffClientProvider,
             lifecycle: ApplicationLifecycle): Behavior[TradePlatformActor.Command] = {
    // check DB connection
    dbManager.ping()

    Behaviors.setup[TradePlatformActor.Command](context => TinkoffTradePlatformActor(
      publisher = client.getPublisher,
      actorContext = context,
      orderService = orderService,
      tpOrderService = tpOrderService,
      candleStoreService = candleStoreService,
      eventBus = eventBus,
      utilDAO = utilDAO,
      lifecycle = lifecycle
    )(client))
  }
}