package com.balakhonov.invest.dao

import com.balakhonov.invest.models.db._
import org.squeryl._
import org.squeryl.customtypes.RichCustomTypeMode._

import javax.inject.{Inject, Singleton}

@Singleton
case class DBLibrary @Inject()()
  extends Schema {

  implicit def createKED[T <: IntPrimaryKeyedEntity]: KeyedEntityDef[T, Int] = new KeyedEntityDef[T, Int] {

    def getId(a: T): Int = a.id

    def isPersisted(a: T): Boolean = a.id > 0

    def idPropertyName = "id"
  }

  val bot: Table[Bot] = table[Bot]("bot")
  val instrument: Table[Instrument] = table[Instrument]("instrument")
  val candle: Table[Candle] = table[Candle]("candle")
  val limitOrder: Table[LimitOrder] = table[LimitOrder]("limit_order")
  val lot: Table[Lot] = table[Lot]("lot")

}

trait IntPrimaryKeyedEntity
  extends KeyedEntity[Int] {
  val id: Int
}
