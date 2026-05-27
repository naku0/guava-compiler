package interpreter

import parser.statement.FuncStatement

import scala.collection.mutable

final class RuntimeEnvironment(parent: Option[RuntimeEnvironment] = None) {
  private val values: mutable.Map[String, Option[Any]] =
    mutable.Map.empty

  private val functions: mutable.Map[String, FuncStatement] =
    mutable.Map.empty

  def this(parent: RuntimeEnvironment) =
    this(Some(parent))

  def define(name: String, value: Option[Any]): Unit = {
    values(name) = value
  }

  def assign(name: String, value: Option[Any]): Unit = {
    if (values.contains(name)) {
      values(name) = value
    } else {
      parent match {
        case Some(parentEnvironment) =>
          parentEnvironment.assign(name, value)
        case None =>
          throw new RuntimeException(s"[Runtime Error] Неизвестная переменная '$name'.")
      }
    }
  }

  def get(name: String): Option[Any] = {
    values.get(name) match {
      case Some(value) =>
        value
      case None =>
        parent match {
          case Some(parentEnvironment) =>
            parentEnvironment.get(name)
          case None =>
            throw new RuntimeException(s"[Runtime Error] Неизвестная переменная '$name'.")
        }
    }
  }

  def defineFunction(name: String, function: FuncStatement): Unit = {
    functions(name) = function
  }

  def getFunction(name: String): FuncStatement = {
    functions.get(name) match {
      case Some(function) =>
        function
      case None =>
        parent match {
          case Some(parentEnvironment) =>
            parentEnvironment.getFunction(name)
          case None =>
            throw new RuntimeException(s"[Runtime Error] Неизвестная функция '$name'.")
        }
    }
  }
}
