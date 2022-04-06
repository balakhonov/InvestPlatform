package com.balakhonov.invest.util

import scala.concurrent.Future

object TimeUtil {

  def time[R](logger: (=> String) => Unit, msg: String)(block: => R): R = {
    val t0 = System.currentTimeMillis()
    val result = block
    val diff = System.currentTimeMillis() - t0
    logger(s"$msg [$diff ms]")
    result
  }

  def async[R](logger: Long => Unit)(block: => Future[R]): Future[R] = {
    import scala.concurrent.ExecutionContext.Implicits.global

    val t0 = System.currentTimeMillis()
    val result = block

    result.onComplete { _ =>
      val diff = System.currentTimeMillis() - t0
      logger(diff)
    }

    result
  }

}
