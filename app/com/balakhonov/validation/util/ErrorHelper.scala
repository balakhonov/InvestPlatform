package com.balakhonov.validation.util

import cats.data.Validated
import cats.syntax.monoid._
import cats.syntax.validated._
import cats.{Monoid, Semigroup}
import play.api.libs.json.Json.JsValueWrapper
import play.api.libs.json.{JsArray, JsObject, JsValue, Json}

object ErrorHelper {

  type MultipleErrorsOr[T] = Validated[MaybeMultipleValidationErrors, T]

  def validatedErrors(isFailure: => Boolean,
                      e: => ValidationError): MultipleErrorsOr[Unit] = {
    if (isFailure) MultipleValidationErrors(Seq(e)).invalid[Unit]
    else ().valid[MultipleValidationErrors]
  }

  def validatedErrors(e: => ValidationError): MultipleErrorsOr[Unit] = {
    validatedErrors(isFailure = true, e)
  }

  def validationError(e: ValidationError): Nothing = throw ValidationException(e)

  def validationError(maybeErrors: MaybeMultipleValidationErrors): Unit = {
    maybeErrors match {
      case errors: MultipleValidationErrors =>
        throw ValidationException(errors)

      case _ =>
    }
  }

  def validationError[T](errorsOr: MultipleErrorsOr[T]): Unit = {
    errorsOr match {
      case Validated.Invalid(errors: MultipleValidationErrors) =>
        throw ValidationException(errors)

      case _ =>
    }
  }

  def validationError(msg: String): Nothing = throw ValidationException(new ValidationError()(_ => "") {
    override val message: Option[String] = Some(msg)
    this.code = ValidationErrorCode("CUSTOM_VALIDATION_EXCEPTION")
  })

  def authorizationError(e: AuthorizationError): Nothing = throw AuthorizationException(e)

  def systemError(message: String): Nothing = throw SystemException(message)

  def systemError(message: String, ex: Throwable): Nothing = throw SystemException(message, ex)

}

object ErrorCodesConverters {

  implicit def errorCode(runtimeClass: Class[_]): String = runtimeClass.getName.stripSuffix("$").split('$').last
}

class ValidationErrorCode(val code: String)

object ValidationErrorCode {

  def apply(code: String): ValidationErrorCode = new ValidationErrorCode(code)
}

class ValidationError(var code: ValidationErrorCode)(val fields: JsValueWrapper*) {

  def this(fields: JsValueWrapper*)(implicit _code: Class[_] => String) = {
    this(ValidationErrorCode(""))(fields: _*)
    this.code = ValidationErrorCode(_code(this.getClass))
  }

  val message: Option[String] = None

  def toJson: JsObject = {
    Json.obj(
      "success" -> false,
      "message" -> message,
      "code" -> code.code,
      "fields" -> Json.arr(fields: _*)
    )
  }
}

case object MultipleValidationErrorsCode
  extends ValidationErrorCode("MULTIPLE_VALIDATION_ERRORS")

sealed trait MaybeMultipleValidationErrors

object MaybeMultipleValidationErrors {

  implicit val multipleValidationErrorsSemigroup: Semigroup[MultipleValidationErrors] = {
    (x: MultipleValidationErrors, y: MultipleValidationErrors) =>
      MultipleValidationErrors(x.validationErrors ++ y.validationErrors)
  }

  implicit val maybeMultipleValidationErrorsMonoid: Monoid[MaybeMultipleValidationErrors] = new Monoid[MaybeMultipleValidationErrors] {

    override def empty: MaybeMultipleValidationErrors = EmptyMultipleValidationErrors

    override def combine(x: MaybeMultipleValidationErrors,
                         y: MaybeMultipleValidationErrors): MaybeMultipleValidationErrors = {
      (x, y) match {
        case (EmptyMultipleValidationErrors, r) => r
        case (l, EmptyMultipleValidationErrors) => l
        case (l: MultipleValidationErrors, r: MultipleValidationErrors) => l |+| r
      }
    }
  }
}

case class MultipleValidationErrors(validationErrors: Seq[ValidationError])
  extends ValidationError(MultipleValidationErrorsCode)(Nil: _*)
    with MaybeMultipleValidationErrors {

  override def toJson: JsObject = {
    Json.obj(
      "success" -> false,
      "message" -> message,
      "code" -> code.code,
      "errors" -> validationErrors.map(_.toJson - "success")
    )
  }
}

case object EmptyMultipleValidationErrors extends MaybeMultipleValidationErrors

object MultipleValidationErrors {

  def apply(validationError: ValidationError): MultipleValidationErrors = {
    MultipleValidationErrors(Seq(validationError))
  }
}

object ValidationError {

  def apply(json: JsValue): ValidationError = {
    val code = (json \ "code").as[String]
    val fields = (json \ "fields").as[JsArray].value

    new ValidationError(ValidationErrorCode(code))(fields)
  }
}

class ValidationTextError(val msg: String) extends ValidationError()(_ => "") {

  override val message: Option[String] = Some(msg)
  this.code = ValidationErrorCode("")

  override def toJson: JsObject = Json.obj(
    "success" -> false,
    "message" -> message,
    "code" -> code.code,
    "fields" -> Json.arr(fields: _*)
  )
}

class AuthorizationError(val message: String, val fields: JsValueWrapper*)(implicit _code: Class[_] => String) {
  val code: String = _code(this.getClass)

  def toJson: JsObject = Json.obj(
    "success" -> false,
    "code" -> code,
    "message" -> message,
    "fields" -> Json.arr(fields: _*)
  )
}

case class AuthorizationException(error: AuthorizationError)
  extends Exception(s"Authorization failed with code ${error.code}, fields: ${error.fields}")

case class SystemException(message: String, cause: Throwable = None.orNull)
  extends Exception(message, cause) {

  def toJson: JsObject = Json.obj(
    "success" -> false,
    "message" -> message
  )

  override def equals(obj: Any): Boolean = {
    obj match {
      case other: SystemException =>
        other.message == message

      case _ => false
    }
  }
}

object SystemException {

  def apply(json: JsValue): SystemException = {
    val message = (json \ "message").as[String]
    SystemException(message)
  }
}

case class ValidationException(error: ValidationError)
  extends Exception(s"Validation failed with code ${error.code.code}, fields: ${error.fields}")