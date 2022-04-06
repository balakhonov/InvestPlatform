package com.balakhonov.invest.models.db

import com.balakhonov.invest.dao.IntPrimaryKeyedEntity
import com.balakhonov.invest.models.enums.{OperationType, OrderStatus}
import com.balakhonov.invest.models.enums.OperationType.OperationType
import com.balakhonov.invest.models.enums.OrderStatus.OrderStatus
import org.squeryl.annotations.Column

import java.time.LocalDateTime

case class LimitOrder(id: Int,
                      @Column("created") created: LocalDateTime,
                      @Column("executed") executed: Option[LocalDateTime],
                      @Column("figi") figi: String,
                      @Column("conditions") conditions: String,
                      @Column("order_ref") orderRef: Option[String],
                      @Column("operation") operation: OperationType,
                      @Column("requested_lots") requestedLots: Int,
                      @Column("price") price: Double,
                      @Column("commission") commission: Option[Double],
                      @Column("status") status: OrderStatus,
                      @Column("executed_lots") executedLots: Int,
                      @Column("reject_reason") rejectReason: Option[String],
                      @Column("message") message: Option[String])
  extends IntPrimaryKeyedEntity {
  def this() = this(0, null, None, null, null, None, OperationType.SELL, 0, 0, None, OrderStatus.NEW, 0, None, None)
}
