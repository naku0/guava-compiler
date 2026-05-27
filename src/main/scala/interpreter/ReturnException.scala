package interpreter

final case class ReturnException(value: Option[Any])
  extends RuntimeException(null, null, false, false)
