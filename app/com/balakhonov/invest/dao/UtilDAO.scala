package com.balakhonov.invest.dao

import javax.inject.{Inject, Singleton}

@Singleton
case class UtilDAO @Inject()(botDao: BotDao,
                             candleDao: CandleDao,
                             instrumentDao: InstrumentDao,
                             limitOrderDao: LimitOrderDao,
                             lotDao: LotDao)