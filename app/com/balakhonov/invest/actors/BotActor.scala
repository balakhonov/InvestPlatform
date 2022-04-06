package com.balakhonov.invest.actors

import akka.actor.typed._
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}
import cats.Eval
import com.balakhonov.invest.actors.BotActor._
import com.balakhonov.invest.dao.UtilDAO
import com.balakhonov.invest.models._
import com.balakhonov.invest.models.db.{Bot, Candle, Instrument}
import com.balakhonov.invest.provider.ClientProvider
import com.balakhonov.invest.services.{BotActorLogger, CandleStoreService, OrderSyncService}
import com.balakhonov.invest.strategies.Strategy
import com.balakhonov.invest.streams.TradePlatformEventBus
import com.balakhonov.invest.streams.TradePlatformEventBus.{Event, InstrumentEvent}
import com.balakhonov.invest.util.{Executors, TimeUtil}
import com.google.inject.Provides
import play.api.libs.concurrent.ActorModule

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}

case class BotActor(botId: Int,
                    strategy: Strategy,
                    instrument: Instrument,
                    eventBus: TradePlatformEventBus,
                    candleStoreService: CandleStoreService,
                    utilDAO: UtilDAO,
                    client: ClientProvider,
                    override val context: ActorContext[BotActor.Command])
  extends AbstractBehavior[BotActor.Command](context)
    with OrderSyncService
    with BotActorLogger {

  private implicit val ec: ExecutionContextExecutor = context.executionContext
  private val chec: ExecutionContextExecutor = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(s"candle-handler-${instrument.figi}", 1))

  private var lastCandlePriceChange: Option[Candle] = None
  private var isCandleInProgress = false

  override def onSignal: PartialFunction[Signal, Behavior[BotActor.Command]] = {
    case MessageAdaptionFailure(ex) =>
      logger.error(s"${getClass.getSimpleName}(${instrument.name}) message was processed with exception", ex)
      this
    case PreRestart =>
      logger.warn(s"${getClass.getSimpleName}(${instrument.name}) will be restarted")
      this
    case PostStop =>
      logger.warn(s"${getClass.getSimpleName}(${instrument.name}) stopped")
      this
  }

  override def onMessage(msg: BotActor.Command): Behavior[BotActor.Command] = {
    msg match {
      case BotActor.PriceChanged(candle) =>
        lastCandlePriceChange = Some(candle)

        if (!isCandleInProgress) {
          isCandleInProgress = true
          context.self ! ProcessCandle(candle.figi)
        }
        this

      case ProcessCandle(_) =>
        lastCandlePriceChange.fold {
          isCandleInProgress = false
        } { candle =>
          lastCandlePriceChange = None
          processCandle(candle)
        }
        this

      case SyncOrders(_, orders) =>
        syncOrders(orders)
        this

      case Ping(ref) =>
        ref ! Pong
        this

      case GetState(ref) =>
        val lots = utilDAO.lotDao.countApplicableToSell(instrument.figi)
        ref ! State(
          price = lastCandlePriceChange.map(_.closingPrice).orElse(strategy.lastPrice),
          lotSize = instrument.lotSize,
          lots = lots
        )
        this

      case BotActor.Stop(_) =>
        logger.info(s"Stopping ${getClass.getSimpleName}(${instrument.figi})")
        Behaviors.stopped

      case _ =>
        this
    }
  }

  private def processCandle(candle: Candle): Unit = {
    val future = Future {
      withLogger {
        if (strategy.historySize > 0) {
          implicit val ctx: TradeContext = {
            val bot = Eval.later(utilDAO.botDao.getById(botId, withExclusiveLock = false))
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

          if (!ctx.hasActiveOrder) {
            strategy.doReceive(candle)
          }
        }
      }
    }(chec)

    TimeUtil.async(time => logger.debug(s"Async Candle(${instrument.name}) processed in ${time}ms"))(future)

    future.onComplete { _ =>
      context.self ! ProcessCandle(candle.figi)
    }
  }

}

object BotActor {

  sealed trait Command extends Event

  trait InstrumentCommand
    extends InstrumentEvent
      with Command

  case class PriceChanged(candle: Candle) extends InstrumentCommand {
    override def figi: String = candle.figi
  }

  case class ProcessCandle(override val figi: String) extends InstrumentCommand

  case class Stop(figi: String) extends InstrumentCommand

  case class SyncOrders(override val figi: String,
                        orders: Seq[Order]) extends InstrumentCommand

  case class Ping(ref: ActorRef[BotActor.Reply]) extends Command

  case class GetState(ref: ActorRef[BotActor.Reply]) extends Command


  trait Reply

  object Pong extends Reply

  case class State(price: Option[Double],
                   lotSize: Int,
                   lots: Int) extends Reply

  case class Fail(ex: Throwable) extends Reply
}

case class BotActorFactory(strategy: Strategy,
                           instrument: Instrument,
                           bot: Bot)
  extends ActorModule {
  type Message = BotActor.Command

  @Provides
  def create(eventBus: TradePlatformEventBus,
             candleStoreService: CandleStoreService,
             utilDAO: UtilDAO,
             client: ClientProvider): Behavior[BotActor.Command] = {
    Behaviors.setup[BotActor.Command](context => new BotActor(bot.id, strategy, instrument, eventBus, candleStoreService, utilDAO, client, context))
  }
}

