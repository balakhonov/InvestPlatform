package com.balakhonov.invest.models

import com.balakhonov.invest.models.enums.OrderStatus

case class PlacedLimitOrder(orderRef: String,
                            status: OrderStatus.Value,
                            commission: Option[Double],
                            executedLots: Int,
                            rejectReason: Option[String],
                            message: Option[String])
