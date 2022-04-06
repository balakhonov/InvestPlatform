package com.balakhonov.modules

import com.balakhonov.invest.dao._
import com.balakhonov.invest.db.{DBManager, DefaultTargetDB, TargetDB}
import com.balakhonov.invest.provider.tinkoff.actors.TinkoffTradePlatformActorFactory
import com.balakhonov.invest.services.{OrderService, OrderServiceImpl}
import com.google.inject.AbstractModule
import net.codingwell.scalaguice.ScalaModule
import play.api.Logger
import play.api.libs.concurrent.AkkaGuiceSupport

import java.util.{Locale, TimeZone}


class BaseModule()
  extends AbstractModule
    with AkkaGuiceSupport
    with ScalaModule {

  private val LOG = Logger(this.getClass)

  LOG.info(s"Init ${getClass.getSimpleName} module")

  override def configure(): Unit = { //scalastyle:ignore
    System.setProperty("user.timezone", "GMT")
    TimeZone.setDefault(TimeZone.getTimeZone("GMT"))
    Locale.setDefault(Locale.US)

    bind[TargetDB].to(classOf[DefaultTargetDB])
    bind[DBManager].asEagerSingleton()
    bind[LotDao].to(classOf[LotDaoImpl])
    bind[LimitOrderDao].to(classOf[LimitOrderDaoImpl])

    bind[OrderService].to(classOf[OrderServiceImpl])

    bindTypedActor(new TinkoffTradePlatformActorFactory, "tinkoff-trade-system")
  }
}