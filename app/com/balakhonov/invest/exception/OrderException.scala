package com.balakhonov.invest.exception

case class OrderException(figi: String, msg: String, code: Option[String]) extends Exception(msg)

/**
 * Говорит о том что предыдущий запрос на закрытие по orderRef
 * уже был либо исполнен либо отменен на стороне брокера
 *
 * @param orderRef Ref to broker's order ID
 */
case class OrderAlreadyCancelledOrApplied(orderRef: String, msg: String, code: Option[String]) extends Exception(msg)

case class NotEnoughMoneyToExecuteOrder(figi: String, msg: String, code: Option[String]) extends Exception(msg)

case class InstrumentIsDisabledForTrading(figi: String, msg: String, code: Option[String]) extends Exception(msg)