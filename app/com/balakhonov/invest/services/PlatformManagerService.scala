package com.balakhonov.invest.services

import akka.actor.typed.scaladsl.AskPattern._
import akka.actor.typed.{ActorRef, Scheduler}
import akka.util.Timeout
import com.balakhonov.invest.actors.TradePlatformActor
import com.balakhonov.invest.dao.UtilDAO
import com.balakhonov.invest.dto.{PlatformStatusDto, UpdateBotDto}

import javax.inject.{Inject, Singleton}
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

@Singleton
class PlatformManagerService @Inject()(tradePlatformActorRef: ActorRef[TradePlatformActor.Command],
                                       scheduler: Scheduler,
                                       utilDAO: UtilDAO) {

  def getPlatformStatus: Future[PlatformStatusDto] = {
    import scala.concurrent.ExecutionContext.Implicits.global

    val timeout: Timeout = Timeout(5.seconds)
    val future: Future[TradePlatformActor.Reply] = tradePlatformActorRef.?[TradePlatformActor.Reply](ref => TradePlatformActor.Status(ref))(timeout, scheduler)

    future.map {
      case TradePlatformActor.StatusResponse(dto) =>
        dto
      case TradePlatformActor.Fail(ex) =>
        throw ex
      case result =>
        throw new Exception(s"Unexpected result '$result'!")
    }.recover {
      case _: Throwable =>
        PlatformStatusDto(
          isActive = false,
          startedAt = None,
          dailyProfitRUB = None,
          dailyProfitUSD = None,
          monthlyProfitRUB = None,
          monthlyProfitUSD = None,
          bots = Nil
        )
    }
  }

  def updateBot(id: Int,
                dto: UpdateBotDto): Unit = {
    val originalBot = utilDAO.botDao.getById(id, withExclusiveLock = false)

    val updatedBot = originalBot.copy(
      canBuy = dto.canBuy.getOrElse(originalBot.canBuy),
      canSell = dto.canSell.getOrElse(originalBot.canSell),
      isActive = dto.isActive.getOrElse(originalBot.isActive)
    )

    utilDAO.botDao.update(updatedBot)

    if (originalBot.isActive != updatedBot.isActive) {
      if (updatedBot.isActive) {
        val strategy = DefaultStrategies.defineStrategy(updatedBot.strategy)
        val instrument = utilDAO.instrumentDao.getByFigi(updatedBot.figi)

        val timeout: Timeout = Timeout(5.seconds)
        tradePlatformActorRef.?[TradePlatformActor.Reply] { ref =>
          TradePlatformActor.StartBot(instrument, updatedBot, strategy, ref)
        }(timeout, scheduler)
      } else {
        tradePlatformActorRef ! TradePlatformActor.StopBot(updatedBot.figi)
      }
    }
  }

}
