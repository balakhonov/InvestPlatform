package com.balakhonov.invest.analyzer.model

import com.balakhonov.invest.dao.LimitOrderDao
import com.balakhonov.invest.models.db.LimitOrder

import javax.inject.{Inject, Singleton}
import scala.collection.mutable


@Singleton
case class InmemoryLimitOrderDaoImpl @Inject()()
  extends LimitOrderDao {
  private val cache = mutable.ListBuffer[LimitOrder]()
  private var id: Int = 0

  def nextId: Int = {
    id += 1
    id
  }

  override def list(): List[LimitOrder] = ???

  override def listNewOnBuy(figi: Option[String]): List[LimitOrder] = ???

  override def list(figi: String): List[LimitOrder] = ???

  override def getById(id: Int, withExclusiveLock: Boolean): LimitOrder = ???

  override def insert(element: LimitOrder): LimitOrder = {
    val el = element.copy(id = nextId)
    cache.+=(el)
    el
  }

  override def insert(elements: LimitOrder*): Unit = ???

  override def update(element: LimitOrder): Unit = ???

  override def remove(id: Int): Unit = ???

  override def hasActiveOrderToBuy(figi: String): Boolean = ???

  override def hasActiveOrderToSell(figi: String): Boolean = ???

  override def deleteAll(figi: String): Int = {
    cache.clear()
    0
  }
}
