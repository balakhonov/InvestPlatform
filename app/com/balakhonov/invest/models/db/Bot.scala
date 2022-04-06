package com.balakhonov.invest.models.db

import com.balakhonov.invest.dao.IntPrimaryKeyedEntity
import org.squeryl.annotations.Column
import play.api.libs.json.JsObject

case class Bot(id: Int,
               @Column("figi") figi: String,
               @Column("strategy") strategy: String,
               @Column("strategy_settings") strategySettings: Option[JsObject],
               @Column("can_buy") canBuy: Boolean,
               @Column("can_sell") canSell: Boolean,
               @Column("is_active") isActive: Boolean,
               @Column("lots_to_buy") lotsToBuy: Int)
  extends IntPrimaryKeyedEntity
