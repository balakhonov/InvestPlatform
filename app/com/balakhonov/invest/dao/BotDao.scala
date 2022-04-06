package com.balakhonov.invest.dao

import com.balakhonov.invest.models.db.Bot
import org.squeryl.customtypes.RichCustomTypeMode._

import javax.inject.{Inject, Singleton}

@Singleton
case class BotDao @Inject()(db: DBLibrary) {

  import db._

  def list(): List[Bot] = {
    inTransaction {
      from(bot)(c =>
        select(c)
      ).toList
    }
  }

  def listActive(): List[Bot] = {
    inTransaction {
      from(bot)(c =>
        where(c.isActive === true)
          .select(c)
      ).toList
    }
  }

  def getById(id: Int, withExclusiveLock: Boolean): Bot = findById(id, withExclusiveLock).get

  def findById(id: Int, withExclusiveLock: Boolean): Option[Bot] = {
    inTransaction {
      val q = from(bot)(c =>
        where(c.id === id)
          .select(c)
      )

      if (withExclusiveLock) {
        q.forUpdate.headOption
      } else {
        q.headOption
      }
    }
  }

  def getByFigi(figi: String): Bot = findByFigi(figi).get

  def findByFigi(figi: String): Option[Bot] = {
    inTransaction {
      from(bot)(c =>
        where(c.figi === figi)
          .select(c)
      ).headOption
    }
  }

  def update(element: Bot): Unit = {
    inTransaction {
      bot.update(element)
    }
  }
}
