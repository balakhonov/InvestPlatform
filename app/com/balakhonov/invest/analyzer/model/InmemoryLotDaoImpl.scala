package com.balakhonov.invest.analyzer.model

import cats.implicits.catsSyntaxOptionId
import com.balakhonov.invest.dao.LotDao
import com.balakhonov.invest.dto.ProfitDto
import com.balakhonov.invest.models.db.Lot

import javax.inject.{Inject, Singleton}
import scala.collection.mutable

@Singleton
case class InmemoryLotDaoImpl @Inject()()
  extends LotDao {
  private val cache = mutable.ListBuffer[Lot]()
  private var id: Int = 0

  def nextId: Int = {
    id += 1
    id
  }

  override def countAll(figi: String): Int = cache.count(_.figi == figi)

  override def countApplicableToSell(figi: String): Int = cache.result().count { c =>
    c.figi == figi &&
      !c.isMarkedToSell &&
      !c.isSold
  }

  override def list(): List[Lot] = ???

  override def list(figi: String): List[Lot] = ???

  override def listApplicableToSell(figi: String): List[Lot] = cache.result().filter { c =>
    c.figi == figi &&
      !c.isMarkedToSell &&
      !c.isSold
  }

  override def last(figi: String): Option[Lot] = {
    cache.result().filter { c =>
      c.figi == figi
    }.sortBy(_.id).lastOption
  }

  override def deleteAll(figi: String): Int = {
    cache.clear()
    0
  }

  override def listMarkedToSell(figi: Option[String]): List[Lot] = ???

  override def getById(id: Int, withExclusiveLock: Boolean): Lot = ???

  override def findById(id: Int, withExclusiveLock: Boolean): Option[Lot] = ???

  override def listByIds(ids: Seq[Int], withExclusiveLock: Boolean): List[Lot] = {
    cache.result().filter { c =>
      ids.contains(c.id)
    }
  }

  override def insert(element: Lot): Lot = {
    val el = element.copy(id = nextId)
    cache.+=(el)
    el
  }

  override def insert(elements: Lot*): Unit = ???

  override def update(element: Lot): Unit = {
    val index = cache.indexWhere(_.id == element.id)
    cache.update(index, element)
  }

  override def remove(id: Int): Unit = ???

  override def removeAll(): Unit = ???

  override def listDailyProfit: List[ProfitDto] = ???

  override def listMonthlyProfit: List[ProfitDto] = ???

  override def listTotalProfit: List[ProfitDto] = {
    val list = cache.result().filter { c =>
      c.isSold
    }

    if (list.isEmpty) {
      Nil
    } else {
      List(ProfitDto(list.head.figi, "Currency", list.size, list.flatMap(_.netProfit).sum.some))
    }
  }

  override def mapBought: Map[String, (Long, Double)] = {
    cache.result().filter { c =>
      !c.isSold
    }.groupBy(_.figi).view.mapValues { list =>
      list.size.toLong -> list.map(_.price).sum
    }.toMap
  }

  override def bought(figi: String): Option[(Long, Option[Double])] = ???
}
