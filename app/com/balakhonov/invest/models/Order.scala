package com.balakhonov.invest.models

import com.balakhonov.invest.models.enums.{OperationType, OrderStatus}

case class Order(figi: String,
                 orderRef: String,
                 operation: OperationType.Value,
                 status: OrderStatus.Value,
                 requestedLots: Int,
                 executedLots: Int)
