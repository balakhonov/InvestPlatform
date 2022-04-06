package com.balakhonov.invest.dto

import com.balakhonov.invest.util.JsonUtil.OptionalLocalDateTimeToJson
import play.api.libs.json.{JsObject, Json}

import java.time.LocalDateTime

case class PlatformStatusDto(isActive: Boolean,
                             startedAt: Option[LocalDateTime],
                             dailyProfitRUB: Option[Double],
                             dailyProfitUSD: Option[Double],
                             monthlyProfitRUB: Option[Double],
                             monthlyProfitUSD: Option[Double],
                             bots: Seq[BotStatusDto]) {
  def toJson: JsObject = Json.obj(
    "isActive" -> isActive,
    "startedAt" -> startedAt.toJson,
    "dailyProfitRUB" -> dailyProfitRUB,
    "dailyProfitUSD" -> dailyProfitUSD,
    "monthlyProfitRUB" -> monthlyProfitRUB,
    "monthlyProfitUSD" -> monthlyProfitUSD,
    "bots" -> bots.map(_.toJson)
  )
}

case class BotStatusDto(id: Int,
                        name: String,
                        figi: String,
                        canBuy: Boolean,
                        canSell: Boolean,
                        isActive: Boolean,
                        profit: Option[Double],
                        currency: String,
                        isLaunched: Boolean,
                        price: Option[Double],
                        lotSize: Int,
                        lots: Int,
                        totalProfit: Option[Double]) {
  def toJson: JsObject = Json.obj(
    "id" -> id,
    "name" -> name,
    "figi" -> figi,
    "canBuy" -> canBuy,
    "canSell" -> canSell,
    "isActive" -> isActive,
    "profit" -> profit,
    "currency" -> currency,
    "isLaunched" -> isLaunched,
    "price" -> price,
    "lotSize" -> lotSize,
    "lots" -> lots,
    "totalProfit" -> totalProfit
  )
}
