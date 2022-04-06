package com.balakhonov

import com.balakhonov.invest.strategies.Strategy
import com.balakhonov.invest.util.DoubleUtil._

import scala.concurrent.ExecutionContext.Implicits
import scala.concurrent.Future

package object invest {

  type StrategyBuilder = () => Strategy

  trait Trend

  case class Uptrend(power: Double) extends Trend {
    override def toString: String = s"Uptrend(${to2p(power)})"
  }

  object Downtrend extends Trend {
    override def toString: String = "Downtrend"
  }

  object Flatline extends Trend {
    override def toString: String = "Flatline"
  }

  implicit class OptionFutureSwitch[A](x: Option[Future[A]]) {

    def switch: Future[Option[A]] = {
      x match {
        case Some(value) => value.map(Some(_))(Implicits.global)
        case None => Future.successful(None)
      }
    }
  }
}
