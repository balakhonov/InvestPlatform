package com.balakhonov.invest.dao

import org.squeryl.adapters.MySQLInnoDBAdapter
import org.squeryl.{Session, SessionFactory}
import play.api.Logger
import play.api.db.Database

import java.net.UnknownHostException

case class InvestDatabaseSessionFactory(db: Database,
                                        adapter: MySQLInnoDBAdapter)
  extends SessionFactory {

  private[this] val LOG = Logger(this.getClass)

  override def newSession: Session = {
    try {
      Session.create(db.getConnection, adapter)
    } catch {
      case ex: UnknownHostException =>
        LOG.error(s"Can't connect to DB host '${db.dataSource.getConnection.getMetaData.getURL}'. ${ex.getMessage}")
        throw ex

      case ex: Exception =>
        LOG.error(s"Can't create DB session", ex)
        throw ex
    }
  }

  def close(): Unit = {
    db.shutdown()
  }
}