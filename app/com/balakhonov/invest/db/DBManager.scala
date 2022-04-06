package com.balakhonov.invest.db

import com.balakhonov.invest.dao.InvestDatabaseSessionFactory
import org.squeryl.SessionFactory
import org.squeryl.adapters.MySQLInnoDBAdapter
import play.api.Logger
import play.api.db.DBApi

import java.sql.Connection
import javax.inject.{Inject, Singleton}

@Singleton
case class DBManager @Inject()(dbApi: DBApi, targetDB: TargetDB) {

  private[this] val LOG = Logger(this.getClass)

  // Set default session factory
  SessionFactory.concreteFactory = {
    val sf = InvestDatabaseSessionFactory(dbApi.database(targetDB.name), new MySQLInnoDBAdapter)

    Some(() => sf.newSession)
  }

  def ping(): Unit = {
    var con: Connection = None.orNull
    try {
      con = dbApi.database(targetDB.name).getConnection
      val result: Boolean = con.createStatement().execute("Select 1")
      if (result) {
        LOG.info(s"DB '${con.getMetaData.getURL}' is reachable.")
      } else {
        throw new Exception(s"DB '${con.getMetaData.getURL}' is not reachable.")
      }
    } finally {
      if (con != null) con.close()
    }
  }
}

trait TargetDB {
  val name: String
}

@Singleton
case class DefaultTargetDB() extends TargetDB {
  override val name: String = "default"
}

@Singleton
case class LocalTargetDB() extends TargetDB {
  override val name: String = "local"
}
