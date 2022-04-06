package com.balakhonov.invest.dao

import com.balakhonov.invest.models.db.LimitOrder
import com.balakhonov.invest.models.enums.{OperationType, OrderStatus}
import org.squeryl.customtypes.RichCustomTypeMode._

import javax.inject.{Inject, Singleton}

trait LimitOrderDao {

  def list(): List[LimitOrder]

  def listNewOnBuy(figi: Option[String]): List[LimitOrder]

  def list(figi: String): List[LimitOrder]

  def getById(id: Int, withExclusiveLock: Boolean): LimitOrder

  def insert(element: LimitOrder): LimitOrder

  def insert(elements: LimitOrder*): Unit

  def update(element: LimitOrder): Unit

  def remove(id: Int): Unit

  def hasActiveOrderToBuy(figi: String): Boolean

  def hasActiveOrderToSell(figi: String): Boolean

  def deleteAll(figi: String): Int
}

@Singleton
case class LimitOrderDaoImpl @Inject()(db: DBLibrary)
  extends LimitOrderDao {

  import db._

  def list(): List[LimitOrder] = {
    inTransaction {
      from(limitOrder)(c =>
        select(c)
      ).toList
    }
  }

  def listNewOnBuy(figi: Option[String]): List[LimitOrder] = {
    inTransaction {
      from(limitOrder)(c =>
        where(
          c.figi === figi.?
            and c.operation === OperationType.BUY
            and c.status === OrderStatus.NEW
        )
          .select(c)
      ).toList
    }
  }

  def list(figi: String): List[LimitOrder] = {
    inTransaction {
      from(limitOrder)(c =>
        where(
          c.figi === figi
        )
          .select(c)
      ).toList
    }
  }

  def getById(id: Int, withExclusiveLock: Boolean): LimitOrder = {
    inTransaction {
      val q = from(limitOrder)(c =>
        where(c.id === id)
          .select(c)
      )

      if (withExclusiveLock) {
        q.forUpdate.head
      } else {
        q.head
      }
    }
  }

  def insert(element: LimitOrder): LimitOrder = {
    inTransaction {
      limitOrder.insert(element)
    }
  }

  def insert(elements: LimitOrder*): Unit = {
    inTransaction {
      limitOrder.insert(elements)
    }
  }

  def update(element: LimitOrder): Unit = {
    inTransaction {
      limitOrder.update(element)
    }
  }

  def remove(id: Int): Unit = {
    inTransaction {
      limitOrder.deleteWhere(_.id === id)
    }
  }

  def hasActiveOrderToBuy(figi: String): Boolean = {
    inTransaction {
      from(limitOrder)(c =>
        where(
          c.figi === figi
            and c.operation === OperationType.BUY
            and c.status === OrderStatus.NEW
        )
          .compute(count)
      ).toInt > 0
    }
  }

  def hasActiveOrderToSell(figi: String): Boolean = {
    inTransaction {
      from(lot)(c =>
        where(
          c.figi === figi
            and c.isMarkedToSell === true
            and c.isSold === false
        )
          .compute(count)
      ).toInt > 0
    }
  }

  def deleteAll(figi: String): Int = {
    inTransaction {
      limitOrder.deleteWhere { c =>
        c.figi === figi
      }
    }
  }

}
