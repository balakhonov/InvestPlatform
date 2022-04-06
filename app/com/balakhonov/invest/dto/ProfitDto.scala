package com.balakhonov.invest.dto

case class ProfitDto(figi: String,
                     currency: String,
                     sold: Long,
                     profit: Option[Double])