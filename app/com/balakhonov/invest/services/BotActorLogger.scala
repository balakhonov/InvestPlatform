package com.balakhonov.invest.services

import com.balakhonov.invest.actors.BotActor
import org.slf4j.Logger

import scala.util.Try

trait BotActorLogger {
  bot: BotActor =>

  protected val logger: Logger = context.log

  protected def withLogger(block: => Unit): Unit = {
    Try(block).recover {
      case ex: java.sql.SQLException =>
        logger.error(s"SQLException occurred while processing actor message. ${ex.getMessage}.")
        context.self ! BotActor.Stop(instrument.figi)

      case ex: Throwable =>
        logger.error(s"Exception occurred while processing actor message. ${ex.getMessage}.", ex)
        context.self ! BotActor.Stop(instrument.figi)
    }
  }
}
