package com.balakhonov.invest.util

import scala.jdk.CollectionConverters.ListHasAsScala

object AccountUtil {

  import scala.concurrent.ExecutionContext.Implicits.global

//  def printAccountInfo(): Unit = for {
//    portfolio <- TinkoffClient.getPortfolio
//    currencies <- TinkoffClient.getPortfolioCurrencies
//  } yield {
//    currencies.getCurrencies.asScala.filter(r => r.getCurrency == Currency.RUB || r.getCurrency == Currency.USD).foreach { currency =>
//      println("Currency:", currency.getCurrency, currency.getBalance.doubleValue(), currency.getBlocked)
//    }
//    portfolio.getPositions.asScala.foreach { position =>
//      println("Positions:", position.getFigi, position.getName, position.getBalance.doubleValue(), position.getLots, Option(position.getAveragePositionPrice))
//    }
//  }
//
//  def printAccountBalance(): Unit = for {
//    currencies <- TinkoffClient.getPortfolioCurrencies
//  } yield {
//    currencies.getCurrencies.asScala.filter(r => r.getCurrency == Currency.RUB || r.getCurrency == Currency.USD).foreach { currency =>
//      println("Currency:", currency.getCurrency, currency.getBalance.doubleValue(), currency.getBlocked)
//    }
//  }
}