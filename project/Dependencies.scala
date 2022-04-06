import sbt._

object Dependencies {

  private val SquerylVersion = "0.9.16"
  private val MysqlConnectorVersion = "8.0.25"
  private val TinkoffSDKVersion = "0.5.1"

  val databaseDependencies: Seq[ModuleID] = Seq(
    "org.squeryl" %% "squeryl" % SquerylVersion,
    "org.apache.commons" % "commons-dbcp2" % "2.8.0",
    "mysql" % "mysql-connector-java" % MysqlConnectorVersion
  )

  val loggerDependencies: Seq[ModuleID] = Seq(
    "org.slf4j" % "slf4j-simple" % "1.7.30"
  )

  val functionalDependencies: Seq[ModuleID] = Seq(
    "org.typelevel" %% "cats-core" % "2.0.0",
    "org.typelevel" %% "cats-effect" % "2.0.0"
  )

  val tinkoffInvestDependencies: Seq[ModuleID] = Seq(
    "ru.tinkoff.invest" % "openapi-java-sdk-java8" % TinkoffSDKVersion,
    "io.reactivex.rxjava3" % "rxjava" % "3.0.12"
  )

}
