package com.balakhonov.invest.models.db

import com.balakhonov.invest.dao.IntPrimaryKeyedEntity
import com.balakhonov.invest.models.enums.CandleTimeFrame
import com.balakhonov.invest.util.JsonUtil.LocalDateTimeToJson
import org.squeryl.annotations.Column
import play.api.libs.json.{JsObject, Json}

import java.time.LocalDateTime

case class Candle(id: Int,
                  @Column("open_price") openPrice: Double,
                  @Column("closing_price") closingPrice: Double,
                  @Column("highest_price") highestPrice: Double,
                  @Column("lowest_price") lowestPrice: Double,
                  @Column("trading_value") tradingValue: Int,
                  @Column("date_time") dateTime: LocalDateTime,
                  @Column("interval") interval: CandleTimeFrame.Value,
                  @Column("figi") figi: String)
  extends IntPrimaryKeyedEntity {

  def this() = this(0, 0d, 0d, 0d, 0d, 0, LocalDateTime.now(), CandleTimeFrame._1MIN, None.orNull)

  def isGreen: Boolean = openPrice < closingPrice

  def isRed: Boolean = !isGreen

  def toJson: JsObject = Json.obj(
    "id" -> id,
    "openPrice" -> openPrice,
    "closingPrice" -> closingPrice,
    "highestPrice" -> highestPrice,
    "lowestPrice" -> lowestPrice,
    "tradingValue" -> tradingValue,
    "dateTime" -> dateTime.toJson,
    "interval" -> interval.toString,
    "figi" -> figi
  )

}
