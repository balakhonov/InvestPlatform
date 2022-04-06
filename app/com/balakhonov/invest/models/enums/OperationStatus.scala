package com.balakhonov.invest.models.enums

object OperationStatus
  extends Enumeration {

  type OperationStatus = Value

  val DONE: Value = Value(1, "Done")
  val DECLINE: Value = Value(2, "Decline")
  val PROGRESS: Value = Value(3, "Progress")
}
