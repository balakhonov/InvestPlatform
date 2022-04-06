package com.balakhonov.invest.actors

import akka.actor.typed._
import akka.actor.typed.scaladsl.AskPattern._
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}
import akka.util.Timeout
import cats.implicits.catsSyntaxOptionId
import com.balakhonov.invest.actors.BotActor._
import com.balakhonov.invest.actors.TradePlatformActor.{Fail, SpawnBot, StatusResponse, Successful}
import com.balakhonov.invest.dao.UtilDAO
import com.balakhonov.invest.dto.{BotStatusDto, PlatformStatusDto}
import com.balakhonov.invest.models.LotPrice
import com.balakhonov.invest.models.db.{Bot, Instrument}
import com.balakhonov.invest.provider.ClientProvider
import com.balakhonov.invest.services.{CandleStoreService, DefaultStrategies, OrderService, TradePlatformOrderService}
import com.balakhonov.invest.strategies.Strategy
import com.balakhonov.invest.streams.TradePlatformEventBus
import com.balakhonov.invest.streams.TradePlatformEventBus.PlatformClassifier
import com.balakhonov.invest.util.{Executors, PriceUtil}
import com.balakhonov.invest.{StrategyBuilder, _}
import play.api.inject.ApplicationLifecycle

import java.time.LocalDateTime
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}
import scala.util.{Failure, Success, Try}

abstract class TradePlatformActor(tradePlatformPrefixName: String,
                                  actorContext: ActorContext[TradePlatformActor.Command],
                                  orderService: OrderService,
                                  tpOrderService: TradePlatformOrderService,
                                  candleStoreService: CandleStoreService,
                                  eventBus: TradePlatformEventBus,
                                  utilDAO: UtilDAO,
                                  lifecycle: ApplicationLifecycle)
                                 (implicit client: ClientProvider)
  extends AbstractBehavior[TradePlatformActor.Command](actorContext) {
  private val logger = actorContext.log

  private val startedAt = LocalDateTime.now()

  protected val SyncOrdersDelay: FiniteDuration = 20.second

  private implicit val ec: ExecutionContextExecutor = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(
    prefix = s"$tradePlatformPrefixName-trade-platform",
    threads = 8
  ))

  protected def init(): Unit

  protected def stopPlatformHook(): Unit

  protected def startBot(instrument: Instrument): Unit

  protected def onPostStopSignal(): Unit

  internalInit()

  override def onSignal: PartialFunction[Signal, Behavior[TradePlatformActor.Command]] = {
    case MessageAdaptionFailure(ex) =>
      logger.error(s"${getClass.getSimpleName} message was processed with exception", ex)
      this
    case PreRestart =>
      logger.warn(s"${getClass.getSimpleName} will be restarted")
      this
    case PostStop =>
      logger.warn(s"${getClass.getSimpleName} stopped")
      onPostStopSignal()
      Behaviors.same
  }

  override def onMessage(msg: TradePlatformActor.Command): Behavior[TradePlatformActor.Command] = {
    msg match {
      case TradePlatformActor.StartBots =>
        wrapper(launchBots())
        this

      case TradePlatformActor.StartBot(instrument, bot, buildStrategy, ref) =>
        wrapper(loadHistoryAndSpawnBot(instrument, bot, buildStrategy, ref))
        this

      case TradePlatformActor.SpawnBot(instrument, bot, strategy, ref) =>
        wrapper {
          Try(spawnBot(instrument, bot, strategy)) match {
            case Failure(ex) =>
              ref ! Fail(ex)
            case Success(_) =>
              ref ! Successful
          }
        }
        this

      case TradePlatformActor.StopBot(figi) =>
        wrapper(stopBot(figi))
        this

      case ob: TradePlatformActor.OrderBuy =>
        wrapper(tpOrderService.orderToBuy(ob))
        this

      case os: TradePlatformActor.OrderSell =>
        wrapper(tpOrderService.orderToSell(os))
        this

      case TradePlatformActor.SyncOrders =>
        wrapper(syncOrder())
        this

      case TradePlatformActor.Status(ref) =>
        wrapper {
          status.onComplete {
            case Failure(ex) =>
              ref ! Fail(ex)
            case Success(dto) =>
              ref ! StatusResponse(dto)
          }
        }
        this

      case TradePlatformActor.StopPlatform =>
        logger.warn(s"Stopping ${getClass.getSimpleName}")
        Behaviors.stopped
    }
  }

  private def status: Future[PlatformStatusDto] = {
    def defineBotStatus(bot: Bot): Future[(Bot, Boolean, Option[Double], Int, Int)] = {
      actorContext.child(bot.figi) match {
        case Some(ref) =>
          val typedActorRef = ref.unsafeUpcast[BotActor.Command]
          val timeout: Timeout = Timeout(3.seconds)
          val future = typedActorRef.?[BotActor.Reply](ref => BotActor.GetState(ref))(timeout, actorContext.system.scheduler)
          future.map {
            case BotActor.Fail(_) =>
              (bot, false, None, 0, 0)
            case BotActor.State(price, lotSize, lots) =>
              (bot, true, price, lotSize, lots)
          }.recover {
            case _: Throwable =>
              (bot, false, None, 0, 0)
          }

        case None =>
          Future.successful((bot, false, None, 0, 0))
      }
    }

    val listF = {
      val list = utilDAO.botDao.list()
        .map(defineBotStatus)
      Future.sequence(list)
    }

    listF.map { botsWithStatus =>
      val dailyProfits = utilDAO.lotDao.listDailyProfit
      val monthlyProfits = utilDAO.lotDao.listMonthlyProfit
      val totalProfit = utilDAO.lotDao.listTotalProfit

      val dailyProfitByInstrumentMap = dailyProfits.groupBy(_.figi).view.mapValues { list =>
        list.flatMap(_.profit).sum
      }.toMap.withDefaultValue(0d)
      val dailyProfitMap = dailyProfits.groupBy(_.currency).view.mapValues { list =>
        list.flatMap(_.profit).sum
      }.toMap.withDefaultValue(0d)
      val monthlyProfitMap = monthlyProfits.groupBy(_.currency).view.mapValues { list =>
        list.flatMap(_.profit).sum
      }.toMap.withDefaultValue(0d)
      val totalProfitByInstrumentMap = totalProfit.groupBy(_.figi).view.mapValues { list =>
        list.flatMap(_.profit).sum
      }.toMap.withDefaultValue(0d)

      val dailyProfitRUB = dailyProfitMap.get("RUB")
      val dailyProfitUSD = dailyProfitMap.get("USD")
      val monthlyProfitRUB = monthlyProfitMap.get("RUB")
      val monthlyProfitUSD = monthlyProfitMap.get("USD")

      val instruments = utilDAO.instrumentDao.list()
        .groupBy(_.figi)
        .view.mapValues(_.head)
      val boughtMap = utilDAO.lotDao.mapBought

      val bots = botsWithStatus.map { case (bot, isLaunched, lastPriceOpt, lotSize, lots) =>
        val inst = instruments(bot.figi)

        val totalProfit = lastPriceOpt.flatMap { lastPrice =>
          val existedProfit = totalProfitByInstrumentMap(bot.figi)

          val (boughtCount, boughtPrice) = boughtMap(bot.figi)

          val sellPrice = lastPrice * boughtCount
          val potentialProfit = PriceUtil.calcProfit(LotPrice(boughtPrice, lotSize), LotPrice(sellPrice, lotSize))

          Some(existedProfit + potentialProfit)
        }

        BotStatusDto(
          id = bot.id,
          name = inst.name,
          figi = bot.figi,
          canBuy = bot.canBuy,
          canSell = bot.canSell,
          isActive = bot.isActive,
          profit = dailyProfitByInstrumentMap.get(bot.figi),
          currency = inst.currency,
          isLaunched = isLaunched,
          price = lastPriceOpt,
          lotSize = lotSize,
          lots = lots,
          totalProfit = totalProfit
        )
      }

      PlatformStatusDto(
        isActive = true,
        startedAt = startedAt.some,
        dailyProfitRUB = dailyProfitRUB,
        dailyProfitUSD = dailyProfitUSD,
        monthlyProfitRUB = monthlyProfitRUB,
        monthlyProfitUSD = monthlyProfitUSD,
        bots = bots
      )
    }

  }

  private def internalInit(): Unit = {
    eventBus.subscribe(actorContext.self.unsafeUpcast[TradePlatformEventBus.Event], PlatformClassifier)

    init()

    context.self ! TradePlatformActor.StartBots

    actorContext.system.scheduler.scheduleWithFixedDelay(
      initialDelay = 20.second,
      delay = SyncOrdersDelay)(
      { () => actorContext.self ! TradePlatformActor.SyncOrders }
    )

    lifecycle.addStopHook(() => Future(stopPlatformHook()))
  }

  private def launchBots(): Future[Future[Unit]] = {
    val bots = utilDAO.botDao.listActive()
      .map(b => b -> DefaultStrategies.defineStrategy(b.strategy))

    for {
      instruments <- client.listInstruments
    } yield {
      bots
        .flatMap { case (bot, buildStrategy) => instruments.find(_.figi == bot.figi).map { inst => (inst, bot, buildStrategy) } }
        .foldLeft(Future.unit) { case (acc, (instrument, bot, buildStrategy)) =>
          acc.flatMap { _ =>
            // add new Bot to the Trade Platform
            Thread.sleep(3000)
            val timeout: Timeout = Timeout(5.seconds)
            context.self.?[TradePlatformActor.Reply] { ref =>
              TradePlatformActor.StartBot(instrument, bot, buildStrategy, ref)
            }(timeout, context.system.scheduler)
              .map(_ => (): Unit)
              .recoverWith {
                case ex: Throwable =>
                  logger.error(s"BotActor(${instrument.name}) was not started!", ex)
                  throw ex
              }
          }
        }
    }
  }

  private def syncOrder(): Unit = {
    val resultF = Future {
      logger.trace(s"Start Sync Orders.")
      val limitedOrders = utilDAO.limitOrderDao.listNewOnBuy(None)
      val markedToSellLots = utilDAO.lotDao.listMarkedToSell(None)

      Option.when(limitedOrders.nonEmpty || markedToSellLots.nonEmpty) {
        logger.trace(s"Sync Orders. LimitedOrders(${limitedOrders.size}), MarkedToSellLots(${markedToSellLots.size})")

        val resultF = client.listActiveOrders

        resultF.map { orders =>
          logger.trace(s"Sync Orders. Orders(${orders.size})")

          val orderMap = orders.groupBy(_.figi).withDefaultValue(Nil)

          val figis: Set[String] = (limitedOrders.groupBy(_.figi).keys ++ markedToSellLots.groupBy(_.figi).keys).toSet

          figis.foreach { figi =>
            eventBus.publish(SyncOrders(figi, orderMap(figi)))
          }
        }
      }.switch
    }.flatten

    resultF.recover {
      case ex: java.net.SocketTimeoutException =>
        logger.warn(s"TimeoutException occurred while Orders Sync. ${ex.getMessage}.")

      case ex: java.sql.SQLException =>
        logger.error(s"SQLException occurred while Orders Sync. ${ex.getMessage}.")
        actorContext.self ! TradePlatformActor.StopPlatform

      case ex: Throwable =>
        logger.error(s"Exception occurred while Orders Sync. ${ex.getMessage}.", ex)
        actorContext.self ! TradePlatformActor.StopPlatform
    }
  }

  private def loadHistoryAndSpawnBot(instrument: Instrument,
                                     bot: Bot,
                                     buildStrategy: StrategyBuilder,
                                     ref: ActorRef[TradePlatformActor.Reply]): Unit = {
    this.context.pipeToSelf {
      val strategy = buildStrategy()
      strategy.setOrderService(orderService)

      candleStoreService.loadHistory(instrument.figi, strategy.TimeFrame)(client)
        .map { history =>
          strategy.setCandlesQueue(history.candles.takeRight(strategy.MaxCandlesQueueSize))
          strategy
        }.recover {
        case ex: Throwable =>
          logger.error(s"History loading failed for ${getClass.getSimpleName}(${instrument.name})!", ex)
          throw ex
      }
    } { strategy =>
      SpawnBot(instrument, bot, strategy.get, ref)
    }
  }

  private def spawnBot(instrument: Instrument,
                       bot: Bot,
                       strategy: Strategy): Unit = {
    val behavior = Behaviors.supervise(BotActorFactory(strategy, instrument, bot)
      .create(eventBus, candleStoreService, utilDAO, client))
      //            .onFailure(SupervisorStrategy.restart) TODO
      .onFailure(SupervisorStrategy.stop)

    val actorRef = actorContext.spawn(behavior, instrument.figi)

    // subscribe bot on event bus
    val isSubscribed = eventBus.subscribe(actorRef.unsafeUpcast[TradePlatformEventBus.Event], instrument.figi)

    startBot(instrument)

    logger.info(s"BotActor(${instrument.name}, $isSubscribed) was started.")
  }

  private def stopBot(figi: String): Unit = {
    actorContext.child(figi).foreach { actorRef =>
      actorContext.stop(actorRef)
    }
  }

  protected def wrapper(block: => Any): Unit = {
    val result = Try(block) match {
      case Success(result) =>
        result match {
          case f: Future[_] => f
          case _ => Future.successful(result)
        }

      case Failure(ex) => Future.failed(ex)
    }

    result.recover {
      case ex: Throwable =>
        logger.error(s"${getClass.getSimpleName} message was processed with exception", ex)
    }
  }
}

object TradePlatformActor {
  trait Command extends TradePlatformEventBus.Event

  object SyncOrders extends Command

  object StopPlatform extends Command

  case class Status(ref: ActorRef[TradePlatformActor.Reply]) extends Command

  object StartBots extends Command

  case class StartBot(instrument: Instrument,
                      bot: Bot,
                      buildStrategy: StrategyBuilder,
                      ref: ActorRef[TradePlatformActor.Reply]) extends Command

  case class SpawnBot(instrument: Instrument,
                      bot: Bot,
                      strategy: Strategy,
                      ref: ActorRef[TradePlatformActor.Reply]) extends Command

  case class StopBot(figi: String) extends Command

  case class OrderBuy(instrument: Instrument, price: Double, requestedLots: Int, limitedOrderId: Int) extends Command

  case class OrderSell(instrument: Instrument, lotIds: Seq[Int], price: Double) extends Command


  sealed trait Reply

  case class StatusResponse(dto: PlatformStatusDto) extends Reply

  case class Fail(ex: Throwable) extends Reply

  object Successful extends Reply
}