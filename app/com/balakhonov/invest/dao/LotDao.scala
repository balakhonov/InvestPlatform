package com.balakhonov.invest.dao

import com.balakhonov.invest.dto.ProfitDto
import com.balakhonov.invest.models.db.Lot
import org.squeryl.customtypes.RichCustomTypeMode._

import java.time._
import java.time.temporal.TemporalAdjusters.firstDayOfMonth
import javax.inject.{Inject, Singleton}

trait LotDao {

  def countAll(figi: String): Int

  def countApplicableToSell(figi: String): Int

  def list(): List[Lot]

  def list(figi: String): List[Lot]

  def listApplicableToSell(figi: String): List[Lot]

  def last(figi: String): Option[Lot]

  def deleteAll(figi: String): Int

  def listMarkedToSell(figi: Option[String]): List[Lot]

  def getById(id: Int, withExclusiveLock: Boolean): Lot

  def findById(id: Int, withExclusiveLock: Boolean): Option[Lot]

  def listByIds(ids: Seq[Int], withExclusiveLock: Boolean): List[Lot]

  def insert(element: Lot): Lot

  def insert(elements: Lot*): Unit

  def update(element: Lot): Unit

  def remove(id: Int): Unit

  def removeAll(): Unit

  def listDailyProfit: List[ProfitDto]

  def listMonthlyProfit: List[ProfitDto]

  def listTotalProfit: List[ProfitDto]

  def mapBought: Map[String, (Long, Double)]

  def bought(figi: String): Option[(Long, Option[Double])]
}

@Singleton
case class LotDaoImpl @Inject()(db: DBLibrary)
  extends LotDao {

  import db._

  def countAll(figi: String): Int = {
    inTransaction {
      from(lot)(c =>
        where(
          c.figi === figi
        )
          .compute(count(c.id))
      ).toInt
    }
  }

  def countApplicableToSell(figi: String): Int = {
    inTransaction {
      from(lot)(c =>
        where(
          c.figi === figi
            and c.isMarkedToSell === false
            and c.isSold === false
        )
          .compute(count)
      ).toInt
    }
  }

  def list(): List[Lot] = {
    inTransaction {
      from(lot)(c =>
        select(c)
      ).toList
    }
  }

  def list(figi: String): List[Lot] = {
    inTransaction {
      from(lot)(c =>
        where(
          c.figi === figi
        )
          .select(c)
      ).toList
    }
  }

  def listApplicableToSell(figi: String): List[Lot] = {
    inTransaction {
      from(lot)(c =>
        where(
          c.figi === figi
            and c.isMarkedToSell === false
            and c.isSold === false
        )
          .select(c)
      ).toList
    }
  }

  def last(figi: String): Option[Lot] = {
    inTransaction {
      from(lot)(c =>
        where(
          c.figi === figi
        )
          .select(c)
          .orderBy(c.id.desc)
      ).headOption
    }
  }

  def deleteAll(figi: String): Int = {
    inTransaction {
      lot.deleteWhere { c =>
        c.figi === figi
      }
    }
  }

  def listMarkedToSell(figi: Option[String]): List[Lot] = {
    inTransaction {
      from(lot)(c =>
        where(
          c.figi === figi.?
            and c.isMarkedToSell === true
            and c.isSold === false
        )
          .select(c)
      ).toList
    }
  }

  def getById(id: Int, withExclusiveLock: Boolean): Lot = findById(id, withExclusiveLock).get

  def findById(id: Int, withExclusiveLock: Boolean): Option[Lot] = {
    inTransaction {
      val q = from(lot)(c =>
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

  def listByIds(ids: Seq[Int], withExclusiveLock: Boolean): List[Lot] = {
    inTransaction {
      val q = from(lot)(c =>
        where(c.id in ids)
          .select(c)
      )

      if (withExclusiveLock) {
        q.forUpdate.toList
      } else {
        q.toList
      }
    }
  }

  def insert(element: Lot): Lot = {
    inTransaction {
      lot.insert(element)
    }
  }

  def insert(elements: Lot*): Unit = {
    inTransaction {
      lot.insert(elements)
    }
  }

  def update(element: Lot): Unit = {
    inTransaction {
      lot.update(element)
    }
  }

  def remove(id: Int): Unit = {
    inTransaction {
      lot.deleteWhere(_.id === id)
    }
  }

  def removeAll(): Unit = {
    inTransaction {
      lot.deleteWhere(_.id.gt(0))
    }
  }

  def listDailyProfit: List[ProfitDto] = {
    val list = inTransaction {
      join(lot, instrument)((l, i) =>
        where(
          l.isSold === true
            and l.orderSellCreated > LocalDate.now.atTime(0, 0)
        )
          .groupBy(i.figi, i.currency)
          .compute(i.figi, i.currency, count(l.id), sum(l.netProfit))
          .on(l.figi === i.figi)
      ).toList.map { gwm =>
        val item = gwm.measures
        ProfitDto(item._1, item._2, item._3, item._4)
      }
    }

    list
  }

  def listMonthlyProfit: List[ProfitDto] = {
    val list = inTransaction {
      join(lot, instrument)((l, i) =>
        where(
          l.isSold === true
            and l.orderSellCreated > LocalDate.now.`with`(firstDayOfMonth()).atTime(0, 0)
        )
          .groupBy(i.figi, i.currency)
          .compute(i.figi, i.currency, count(l.id), sum(l.netProfit))
          .on(l.figi === i.figi)
      ).toList.map { gwm =>
        val item = gwm.measures
        ProfitDto(item._1, item._2, item._3, item._4)
      }
    }

    list
  }

  def listTotalProfit: List[ProfitDto] = {
    val list = inTransaction {
      join(lot, instrument)((l, i) =>
        where(
          l.isSold === true
        )
          .groupBy(i.figi, i.currency)
          .compute(i.figi, i.currency, count(l.id), sum(l.netProfit))
          .on(l.figi === i.figi)
      ).toList.map { gwm =>
        val item = gwm.measures
        ProfitDto(item._1, item._2, item._3, item._4)
      }
    }

    list
  }

  def mapBought: Map[String, (Long, Double)] = {
    val list = inTransaction {
      from(lot)(l =>
        where(
          l.isSold === false
        )
          .groupBy(l.figi)
          .compute(count(l.id), sum(l.price))
      ).toList
    }
    list.map { item =>
      item.key -> (item.measures._1 -> item.measures._2.getOrElse(0d))
    }.toMap.withDefaultValue(0L -> 0d)
  }

  def bought(figi: String): Option[(Long, Option[Double])] = {
    val option = inTransaction {
      from(lot)(l =>
        where(
          l.figi === figi
            and l.isSold === false
        )
          .compute(count(l.id), sum(l.price))
      ).headOption
    }
    option.map { item =>
      item.measures._1 -> item.measures._2
    }
  }

}
