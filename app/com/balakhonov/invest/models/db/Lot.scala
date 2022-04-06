package com.balakhonov.invest.models.db

import com.balakhonov.invest.dao.IntPrimaryKeyedEntity
import org.squeryl.annotations.Column

import java.time.LocalDateTime

case class Lot(id: Int,
               @Column("created") created: LocalDateTime,
               @Column("order_id") orderId: Int,
               @Column("figi") figi: String,
               @Column("order_ref") orderRef: String,
               @Column("price") price: Double,
               @Column("commission") commission: Option[Double],
               @Column("order_sell_created") orderSellCreated: Option[LocalDateTime],
               @Column("order_sell_executed") orderSellExecuted: Option[LocalDateTime],
               @Column("order_sell_ref") orderSellRef: Option[String],
               @Column("net_profit") netProfit: Option[Double],
               @Column("is_sold") isSold: Boolean,
               @Column("is_marked_to_sell") isMarkedToSell: Boolean)
  extends IntPrimaryKeyedEntity
