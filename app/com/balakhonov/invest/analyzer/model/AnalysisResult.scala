package com.balakhonov.invest.analyzer.model

import com.balakhonov.invest.strategies.Strategy
import com.balakhonov.invest.util.DoubleUtil._

case class AnalysisResult(figi: String,
                          profit: Option[Double],
                          invested: Option[Double],
                          opened: Int,
                          closed: Int,
                          nonClosed: Int,
                          strategy: Strategy,
                          children: Seq[AnalysisResult]) {
  override def toString: String = {
    val details = if (children.nonEmpty)
      s"\n ${
        children.sortBy(_.invested).reverse.map { ch =>
          " -- [%-8s] %-18s %-26s %-11s %-11s %-14s".format(
            s"${ch.figi}",
            s"Invested: ${ch.invested.map(to2p).getOrElse(0d)}",
            s"Profit: ${ch.profit.map(to2p).getOrElse(0d)}(${to2p(ch.perc)}%)",
            s"Opened: ${ch.opened}",
            s"Closed: ${ch.closed}",
            s"Non-Closed: ${ch.nonClosed}"
          )
        }.mkString("\n")
      }"
    else ""

    s"[$figi] Profit: ${profit.map(to2p).getOrElse(0d)}(${to2p(perc)}%) Invested: ${invested.map(to2p).getOrElse(0d)} Opened: $opened Closed: $closed, Non-Closed: $nonClosed $strategy $details"
  }

  def perc: Double = {
    invested match {
      case Some(invested) =>
        val _profit = profit.getOrElse(0d)
        _profit / invested * 100

      case None =>
        profit.fold(0)(_ => 1) * 100
    }
  }
}