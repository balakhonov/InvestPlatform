package com.balakhonov.invest.models


import com.balakhonov.invest.models.enums.OperationStatus.OperationStatus

import java.time.LocalDateTime

case class Operation(figi: String,
                     date: LocalDateTime,
                     status: OperationStatus)
