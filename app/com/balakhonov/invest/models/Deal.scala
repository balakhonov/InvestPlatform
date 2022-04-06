package com.balakhonov.invest.models

import java.time.LocalDateTime

case class Deal(id: String,
                price: Double,
                var takeProfitPrice: Double,
                dateTime: LocalDateTime,
                var hasOrderToSell: Boolean)
