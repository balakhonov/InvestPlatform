package com.balakhonov.invest.models.db

import org.squeryl.annotations.Column

case class Instrument(@Column("id") id: Int,
                      @Column("provider_id") providerId: Int,
                      @Column("figi") figi: String,
                      @Column("name") name: String,
                      @Column("ticker") ticker: String,
                      @Column("isin") isin: String,
                      @Column("lot_size") lotSize: Int,
                      @Column("currency") currency: String,
                      @Column("type") `type`: String)
