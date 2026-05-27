package interpreter

import lexer.TokenType
import parser.expression.*
import parser.statement.*

import scala.collection.mutable.ArrayBuffer

final class TreeInterpreter {
  private var environment: RuntimeEnvironment =
    RuntimeEnvironment()

  def interpret(statements: Iterable[Statement]): Unit = {
    try {
      statements.foreach(execute)
    } catch {
      case exception: RuntimeException =>
        Console.err.println(s"[CRITICAL RUNTIME ERROR]: ${exception.getMessage}")
    }
  }

  private def execute(statement: Statement): Unit = {
    statement match {
      case PrintStatement(expression) =>
        printValue(evaluate(expression))

      case VarStatement(name, initializer) =>
        val initValue = initializer.flatMap(evaluate)
        environment.define(name, initValue)

      case ValStatement(name, initializer) =>
        environment.define(name, evaluate(initializer))

      case ExpressionStatement(expression) =>
        evaluate(expression)

      case BlockStatement(statements) =>
        executeBlock(statements, RuntimeEnvironment(environment))

      case IfStatement(condition, thenBranch, elseBranch) =>
        if (isTruthy(evaluate(condition))) {
          execute(thenBranch)
        } else {
          elseBranch.foreach(execute)
        }

      case WhileStatement(condition, body) =>
        while (isTruthy(evaluate(condition))) {
          execute(body)
        }

      case function: FuncStatement =>
        environment.defineFunction(function.name, function)

      case ReturnStatement(value) =>
        throw ReturnException(value.flatMap(evaluate))

      case BreakStatement() =>
        throw RuntimeException("[Runtime Error] 'break' пока не поддерживается интерпретатором.")

      case ContinueStatement() =>
        throw RuntimeException("[Runtime Error] 'continue' пока не поддерживается интерпретатором.")
    }
  }

  private def executeBlock(statements: Iterable[Statement], blockEnvironment: RuntimeEnvironment): Unit = {
    val previousEnvironment = environment
    environment = blockEnvironment

    try {
      statements.foreach(execute)
    } finally {
      environment = previousEnvironment
    }
  }

  private def evaluate(expression: Expression): Option[Any] = {
    expression match {
      case NumberExpression(value) =>
        Some(value)

      case StringExpression(value) =>
        Some(value)

      case VariableExpression(name) =>
        environment.get(name)

      case AssignExpression(name, valueExpression) =>
        val value = evaluate(valueExpression)
        environment.assign(name, value)
        value

      case expression: BinaryExpression =>
        evaluateBinaryExpression(expression)

      case UnaryExpression(operator, rightExpression) =>
        val right = evaluate(rightExpression)
        operator match {
          case TokenType.MINUS =>
            Some(-asNumber(right))
          case TokenType.EXCL =>
            Some(!isTruthy(right))
          case _ =>
            right
        }

      case CallExpression(calleeName, arguments) =>
        evaluateCallExpression(calleeName, arguments)

      case ArrayExpression(elements) =>
        Some(ArrayBuffer.from(elements.map(evaluate)))

      case IndexExpression(arrayExpression, indexExpression) =>
        val array = asArray(evaluate(arrayExpression))
        val index = asIndex(evaluate(indexExpression), array.length)
        array(index)

      case IndexAssignExpression(arrayExpression, indexExpression, valueExpression) =>
        val array = asArray(evaluate(arrayExpression))
        val index = asIndex(evaluate(indexExpression), array.length)
        val value = evaluate(valueExpression)
        array(index) = value
        value

      case IfExpression(condition, thenBranch, elseBranch) =>
        if (isTruthy(evaluate(condition))) evaluate(thenBranch)
        else evaluate(elseBranch)

      case BlockExpression(expressions) =>
        expressions.foldLeft(Option.empty[Any])((_, expression) => evaluate(expression))

      case ReturnExpression(value) =>
        throw ReturnException(evaluate(value))

      case LambdaExpression(_, _) =>
        throw RuntimeException("[Runtime Error] LambdaExpression пока не поддерживается интерпретатором.")
    }
  }

  private def evaluateBinaryExpression(expression: BinaryExpression): Option[Any] = {
    expression.operator match {
      case TokenType.OR =>
        val left = evaluate(expression.left)
        if (isTruthy(left)) Some(true)
        else Some(isTruthy(evaluate(expression.right)))

      case TokenType.AND =>
        val left = evaluate(expression.left)
        if (!isTruthy(left)) Some(false)
        else Some(isTruthy(evaluate(expression.right)))

      case _ =>
        val left = evaluate(expression.left)
        val right = evaluate(expression.right)

        expression.operator match {
          case TokenType.MINUS =>
            Some(asNumber(left) - asNumber(right))
          case TokenType.SLASH =>
            val divisor = asNumber(right)
            if (divisor == 0) {
              throw RuntimeException("[Runtime Error] Деление на ноль!")
            }
            Some(asNumber(left) / divisor)
          case TokenType.STAR =>
            Some(asNumber(left) * asNumber(right))
          case TokenType.PLUS =>
            (left, right) match {
              case (Some(leftNumber: Double), Some(rightNumber: Double)) =>
                Some(leftNumber + rightNumber)
              case (Some(_: String), _) | (_, Some(_: String)) =>
                Some(valueToString(left) + valueToString(right))
              case _ =>
                throw RuntimeException("[Runtime Error] Нельзя применить оператор '+' к этим значениям.")
            }
          case TokenType.GT =>
            Some(asNumber(left) > asNumber(right))
          case TokenType.GTEQ =>
            Some(asNumber(left) >= asNumber(right))
          case TokenType.LT =>
            Some(asNumber(left) < asNumber(right))
          case TokenType.LTEQ =>
            Some(asNumber(left) <= asNumber(right))
          case TokenType.EQEQ =>
            Some(isEqual(left, right))
          case TokenType.NEQ =>
            Some(!isEqual(left, right))
          case _ =>
            throw RuntimeException("[Runtime Error] Неизвестный бинарный оператор.")
        }
    }
  }

  private def evaluateCallExpression(calleeName: String, arguments: List[Expression]): Option[Any] = {
    val functionDeclaration = environment.getFunction(calleeName)
    val argumentValues = arguments.map(evaluate)
    val callEnvironment = RuntimeEnvironment(environment)

    functionDeclaration.args.zipWithIndex.foreach { case (parameterName, index) =>
      callEnvironment.define(parameterName, argumentValues.lift(index).flatten)
    }

    val previousEnvironment = environment
    environment = callEnvironment

    try {
      execute(functionDeclaration.body)
      None
    } catch {
      case ReturnException(value) =>
        value
    } finally {
      environment = previousEnvironment
    }
  }

  private def isTruthy(value: Option[Any]): Boolean = {
    value match {
      case None => false
      case Some(boolean: Boolean) => boolean
      case Some(_) => true
    }
  }

  private def isEqual(left: Option[Any], right: Option[Any]): Boolean = {
    left == right
  }

  private def asNumber(value: Option[Any]): Double = {
    value match {
      case Some(number: Double) => number
      case Some(number: Int) => number.toDouble
      case Some(other) =>
        throw RuntimeException(s"[Runtime Error] Ожидалось число, получено: $other.")
      case None =>
        throw RuntimeException("[Runtime Error] Ожидалось число, получено пустое значение.")
    }
  }

  private def asIndex(value: Option[Any], arrayLength: Int): Int = {
    val rawIndex = asNumber(value)
    if (!rawIndex.isWhole) {
      throw RuntimeException(s"[Runtime Error] Индекс массива должен быть целым числом, получено: $rawIndex.")
    }

    val index = rawIndex.toInt
    if (index < 0 || index >= arrayLength) {
      throw RuntimeException(
        s"[Runtime Error] Индекс массива вне границ: $index. Размер массива: $arrayLength."
      )
    }

    index
  }

  private def asArray(value: Option[Any]): ArrayBuffer[Option[Any]] = {
    value match {
      case Some(array: ArrayBuffer[?]) =>
        array.asInstanceOf[ArrayBuffer[Option[Any]]]
      case Some(other) =>
        throw RuntimeException(s"[Runtime Error] Ожидался массив, получено: $other.")
      case None =>
        throw RuntimeException("[Runtime Error] Ожидался массив, получено пустое значение.")
    }
  }

  private def printValue(value: Option[Any]): Unit = {
    value match {
      case Some(array: ArrayBuffer[?]) =>
        println(array.asInstanceOf[ArrayBuffer[Option[Any]]].map(valueToString).mkString("[", ", ", "]"))
      case Some(innerValue) =>
        println(innerValue)
      case None => println()
    }
  }

  private def valueToString(value: Option[Any]): String = {
    value.fold("")(_.toString)
  }
}
