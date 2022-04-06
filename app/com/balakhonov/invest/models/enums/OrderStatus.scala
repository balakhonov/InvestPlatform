package com.balakhonov.invest.models.enums

object OrderStatus
  extends Enumeration {

  type OrderStatus = Value

  val NEW: Value = Value(1, "New")
  val PARTIALLYFILL: Value = Value(2, "PartiallyFill")
  val FILL: Value = Value(3, "Fill")
  val CANCELLED: Value = Value(4, "Cancelled")
  val REPLACED: Value = Value(5, "Replaced")
  val PENDINGCANCEL: Value = Value(6, "PendingCancel")
  val REJECTED: Value = Value(7, "Rejected")
  val PENDINGREPLACE: Value = Value(8, "PendingReplace")
  val PENDINGNEW: Value = Value(9, "PendingNew")

}
