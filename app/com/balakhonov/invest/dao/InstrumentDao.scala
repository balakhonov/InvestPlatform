package com.balakhonov.invest.dao

import com.balakhonov.invest.models.db.Instrument
import org.squeryl.customtypes.RichCustomTypeMode._

import javax.inject.{Inject, Singleton}

@Singleton
case class InstrumentDao @Inject()(db: DBLibrary) {

  import db._

  def list(): List[Instrument] = {
    inTransaction {
      from(instrument)(c =>
        select(c)
      ).toList
    }
  }

  def list(figi: String): List[Instrument] = {
    inTransaction {
      from(instrument)(c =>
        where(
          c.figi === figi
        )
          .select(c)
      ).toList
    }
  }

  def getByFigi(figi: String): Instrument = {
    inTransaction {
      from(instrument)(c =>
        where(c.figi === figi)
          .select(c)
      ).headOption.get
    }
  }

  def insert(element: Instrument): Instrument = {
    inTransaction {
      instrument.insert(element)
    }
  }

  def insert(elements: Instrument*): Unit = {
    inTransaction {
      instrument.insert(elements)
    }
  }

}
