package com.balakhonov.invest.analyzer

import akka.actor.ActorSystem
import akka.stream.Materializer
import cats.Eval
import com.balakhonov.invest.analyzer.model.{AnalysisResult, InmemoryLimitOrderDaoImpl, InmemoryLotDaoImpl}
import com.balakhonov.invest.dao.{LimitOrderDao, LotDao, UtilDAO}
import com.balakhonov.invest.db.{DBManager, LocalTargetDB, TargetDB}
import com.balakhonov.invest.models.db.{Bot, Candle, Instrument}
import com.balakhonov.invest.models.enums.CandleTimeFrame
import com.balakhonov.invest.models.enums.CandleTimeFrame.CandleTimeFrame
import com.balakhonov.invest.models.{LotPrice, TradeContext}
import com.balakhonov.invest.strategies._
import com.balakhonov.invest.util.PriceUtil
import com.google.inject.{AbstractModule, Guice}
import com.typesafe.config.Config
import net.codingwell.scalaguice.ScalaModule
import play.api.db.{DBApiProvider, HikariCPConnectionPool}
import play.api.inject.ApplicationLifecycle
import play.api.{Configuration, Environment, Mode}

import java.io.File
import java.time.LocalDateTime
import java.util.{Locale, TimeZone}
import javax.inject.{Inject, Singleton}
import scala.concurrent.Future
import scala.math.BigDecimal.double2bigDecimal

@Singleton
class StrategyAnalyzerService @Inject()(utilDAO: UtilDAO) {

  def analyse(figi: String,
              strategy: Strategy,
              history: Seq[Candle]): AnalysisResult = {
    val instrument = utilDAO.instrumentDao.getByFigi(figi)
    val bot = Eval.now(utilDAO.botDao.getByFigi(figi))

    doAnalyse(instrument, bot, history, strategy)
  }

  def analyse(figi: String,
              strategies: Seq[Strategy],
              history: Seq[Candle]): Seq[AnalysisResult] = {
    val instrument = utilDAO.instrumentDao.getByFigi(figi)
    val bot = Eval.now(utilDAO.botDao.getByFigi(figi))

    strategies.map(doAnalyse(instrument, bot, history, _))
  }

  def analyse(figi: String,
              history: Seq[Candle]): Strategy => AnalysisResult = {
    val instrument = utilDAO.instrumentDao.getByFigi(figi)
    val bot = Eval.now(utilDAO.botDao.getByFigi(figi))

    doAnalyse(instrument, bot, history, _)
  }

  def analyse(figiSet: Seq[String],
              from: LocalDateTime,
              to: LocalDateTime): Strategy => AnalysisResult = {
    val historyFetcher = loadHistory(CandleTimeFrame._10MIN)

    val list = figiSet.map { figi =>
      val instrument = utilDAO.instrumentDao.getByFigi(figi)
      val bot = Eval.now(utilDAO.botDao.getByFigi(figi))
      val history = historyFetcher.apply(figi, from, to)

      (instrument, bot, history)
    }

    analyze(list, _)
  }

  def analyze(figies: Seq[(Instrument, Eval[Bot], List[Candle])],
              strategy: Strategy): AnalysisResult = {
    figies.foldLeft(AnalysisResult(
      figi = "combo",
      profit = None,
      invested = None,
      opened = 0,
      closed = 0,
      nonClosed = 0,
      strategy = strategy,
      children = Nil
    )) { case (acc, (instrument, bot, history)) =>
      val result = doAnalyse(instrument, bot, history, strategy)

      acc.copy(
        profit = acc.profit.fold(result.profit)(profit => Some(profit + result.profit.getOrElse(0d))),
        invested = acc.invested.fold(result.invested)(invested => Some(invested + result.invested.getOrElse(0d))),
        opened = acc.opened + result.opened,
        closed = acc.closed + result.closed,
        nonClosed = acc.nonClosed + result.nonClosed,
        children = acc.children.+:(result)
      )
    }
  }

  private def doAnalyse(instrument: Instrument,
                        bot: Eval[Bot],
                        history: Seq[Candle],
                        strategy: Strategy): AnalysisResult = {
    utilDAO.lotDao.deleteAll(instrument.figi)
    utilDAO.limitOrderDao.deleteAll(instrument.figi)

    strategy.setOrderService(AnalyzerOrderServiceImpl(utilDAO))

    history.foreach { candle =>
      implicit val ctx: TradeContext = {
        val lotsApplicableToSell = Eval.later(utilDAO.lotDao.listApplicableToSell(instrument.figi))
        val lastBoughtLot = Eval.later(utilDAO.lotDao.last(instrument.figi))
        val hasActiveOrderToBuy = Eval.later(utilDAO.limitOrderDao.hasActiveOrderToBuy(candle.figi))
        val hasActiveOrderToSell = Eval.later(utilDAO.limitOrderDao.hasActiveOrderToSell(candle.figi))

        TradeContext(
          bot = bot,
          instrument = instrument,
          lotsApplicableToSell = lotsApplicableToSell,
          lastBoughtLot = lastBoughtLot,
          hasActiveOrderToBuy = hasActiveOrderToBuy,
          hasActiveOrderToSell = hasActiveOrderToSell
        )
      }

      //      TimeUtil.time(println(_), s"$candle processed in") {
      strategy.doReceive(candle)
      //      }
    }

    val totalProfitOpt = history.lastOption.map(_.closingPrice).map { lastPrice =>
      val existedProfit = utilDAO.lotDao.listTotalProfit.groupBy(_.figi).get(instrument.figi).fold(0d)(_.map(_.profit.getOrElse(0d)).sum)

      val (boughtCount, boughtPrice) = utilDAO.lotDao.mapBought.getOrElse(instrument.figi, 0L -> 0d)

      val lotPrice = LotPrice(lastPrice * boughtCount, instrument.lotSize)
      val potentialProfit = PriceUtil.calcProfit(LotPrice(boughtPrice, instrument.lotSize), lotPrice)

      existedProfit + potentialProfit -> lotPrice.totalPrice
    }

    val opened = utilDAO.lotDao.countAll(instrument.figi)
    val nonClosed = utilDAO.lotDao.countApplicableToSell(instrument.figi)

    AnalysisResult(
      figi = instrument.figi,
      profit = totalProfitOpt.map(_._1),
      invested = totalProfitOpt.map(_._2),
      opened = opened,
      closed = opened - nonClosed,
      nonClosed = nonClosed,
      strategy = strategy,
      children = Nil
    )
  }

  private def loadHistory(interval: CandleTimeFrame): (String, LocalDateTime, LocalDateTime) => List[Candle] = {
    val injector = Guice.createInjector(GuiceModule(useH2DB = false))
    val clazz = getClass.getClassLoader.loadClass("com.balakhonov.invest.dao.UtilDAO")
    val utilDAO = injector.getInstance(clazz).asInstanceOf[UtilDAO]

    utilDAO.candleDao.list(_, _, _, interval)
  }
}

case class GuiceModule(useH2DB: Boolean)
  extends AbstractModule
    with ScalaModule {

  override def configure(): Unit = {
    System.setProperty("user.timezone", "GMT")
    TimeZone.setDefault(TimeZone.getTimeZone("GMT"))
    Locale.setDefault(Locale.US)

    val env = Environment(new File("."), this.getClass.getClassLoader, Mode.Prod)
    val configuration = if (useH2DB) {
      Configuration.from(Map(
        "db.default.driver" -> "org.h2.Driver",
        "db.default.url" -> "jdbc:h2:mem:play;MODE=MYSQL;DB_CLOSE_DELAY=-1;CASE_INSENSITIVE_IDENTIFIERS=TRUE;IGNORECASE=TRUE;"
      )).withFallback(Configuration.load(env))
    } else {
      Configuration.load(env)
    }

    implicit val system: ActorSystem = ActorSystem("QuickStart")
    implicit val mat: Materializer = Materializer(system)

    val dbAPI = createDBApiProvider(env, configuration)

    bind[Environment].toInstance(env)
    bind[Configuration].toInstance(configuration)
    bind[Config].toInstance(configuration.underlying)

    bind[TargetDB].to(classOf[LocalTargetDB])
    bind[DBManager].toInstance(DBManager(dbAPI.get, LocalTargetDB()))

    bind[ActorSystem].toInstance(system)
    bind[Materializer].toInstance(mat)

    bind[LotDao].to(classOf[InmemoryLotDaoImpl])
    bind[LimitOrderDao].to(classOf[InmemoryLimitOrderDaoImpl])
  }

  private def createDBApiProvider(env: Environment,
                                  configuration: Configuration): DBApiProvider = {
    new DBApiProvider(
      environment = env,
      configuration = configuration,
      defaultConnectionPool = new HikariCPConnectionPool(env),
      lifecycle = new ApplicationLifecycle {
        override def addStopHook(hook: () => Future[_]): Unit = {}

        override def stop(): Future[_] = Future.unit
      },
      maybeInjector = None
    )
  }
}
