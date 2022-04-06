package com.balakhonov.validation.codes

import com.balakhonov.validation.util.ErrorCodesConverters._
import com.balakhonov.validation.util.ValidationError

object ErrorCodes {

  case class FOREIGN_KEY_SHOULD_BE_GREATER_THAN_ZERO(propertyName: String, value: Int) extends ValidationError(propertyName, value)

  case class JSON_ARRAY_IS_NOT_FOUND(propName: String) extends ValidationError(propName)

  case class ELEMENT_IS_NOT_AN_INTEGER_VALUE_IN_ARRAY(value: String, propName: String) extends ValidationError(value, propName)

  case class JSON_OBJECT_IS_NOT_FOUND(propName: String) extends ValidationError(propName)

  case class JSON_PROPERTY_NOT_FOUND(property: String) extends ValidationError(property)

  case class JSON_PROPERTY_IS_NULL(property: String) extends ValidationError(property)

  case class JSON_PROPERTY_IS_REQUIRED(property: String) extends ValidationError(property)

  case class FIELD_IS_NOT_FOUND_IN_INSTANCE(fieldName: String, instanceName: String) extends ValidationError(fieldName, instanceName)

  case class CAN_NOT_PARSE_FIELD_AS_TYPE(fieldName: String, fieldType: String) extends ValidationError(fieldName, fieldType)

  case class UNSUPPORTED_TIMESTAMP_FORMAT_FOR_VALUE(value: String) extends ValidationError(value)

  case class UNSUPPORTED_LOCAL_TIME_FORMAT_FOR_VALUE(value: String) extends ValidationError(value)

  case class PROPERTY_SHOULD_CONTAIN_BINARY_VALUE(propertyName: String, value: String) extends ValidationError(propertyName, value)

  case class BINARY_INT_SIZE_IS_NOT_VALID(expected: Int, actual: Int) extends ValidationError(expected, actual)
}
