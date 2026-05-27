package semantic.analyzer

import lexer.TokenType
import parser.expression.*
import parser.statement.*
import semantic.environment.SemanticEnvironment
import semantic.DataType

case class SemanticAnalysisResult(
                                   errors: List[String],
                                   warnings: List[String],
                                   environment: SemanticEnvironment
                                 )

private case class AnalysisState(
                                  errors: List[String] = Nil,
                                  warnings: List[String] = Nil,
                                  environment: SemanticEnvironment = SemanticEnvironment()
                                ) {
  def error(message: String): AnalysisState =
    copy(errors = errors :+ message)

  def warning(message: String): AnalysisState =
    copy(warnings = warnings :+ message)

  def withEnvironment(nextEnvironment: SemanticEnvironment): AnalysisState =
    copy(environment = nextEnvironment)
}

object SemanticAnalyzer {
  def analyze(statements: List[Statement]): SemanticAnalysisResult = {
    val initialState = AnalysisState()
    val finalState = checkUnusedVariables(statements.foldLeft(initialState)(visitStatement))

    SemanticAnalysisResult(
      errors = finalState.errors,
      warnings = finalState.warnings,
      environment = finalState.environment
    )
  }

  private def visitStatement(state: AnalysisState, statement: Statement): AnalysisState = {
    statement match {
      case statement: VarStatement => analyzeVarStatement(state, statement)
      case statement: ValStatement => analyzeValStatement(state, statement)
      case statement: PrintStatement => analyzePrintStatement(state, statement)
      case statement: ExpressionStatement => analyzeExpressionStatement(state, statement)
      case statement: BlockStatement => analyzeBlockStatement(state, statement)
      case statement: IfStatement => analyzeIfStatement(state, statement)
      case statement: WhileStatement => analyzeWhileStatement(state, statement)
      case statement: FuncStatement => analyzeFunctionStatement(state, statement)
      case statement: ReturnStatement => analyzeReturnStatement(state, statement)
      case _: BreakStatement | _: ContinueStatement => state
    }
  }

  private def analyzeFunctionStatement(state: AnalysisState, statement: FuncStatement): AnalysisState = {
    val (environmentWithFunction, defined) =
      state.environment.defineFunction(statement.name, statement.args.length)

    if (!defined) {
      state.error(s"[Semantic Error] Функция '${statement.name}' уже объявлена в этой области видимости.")
    } else {
      val functionEnvironment = SemanticEnvironment(Some(environmentWithFunction))

      val stateWithParameters = statement.args.foldLeft(
        state.withEnvironment(functionEnvironment)
      ) { (currentState, parameter) =>
        val (nextEnvironment, parameterDefined) =
          currentState.environment.defineVariable(parameter, isInitialized = true, DataType.Unknown)

        val nextState = currentState.withEnvironment(nextEnvironment)
        if (parameterDefined) nextState
        else nextState.error(s"[Semantic Error] Параметр '$parameter' уже объявлен в функции '${statement.name}'.")
      }

      val analyzedBody = visitStatement(stateWithParameters, statement.body)
      val withUnusedWarnings = checkUnusedVariables(analyzedBody)

      withUnusedWarnings.withEnvironment(
        withUnusedWarnings.environment.parent.getOrElse(environmentWithFunction)
      )
    }
  }

  private def analyzeVarStatement(state: AnalysisState, statement: VarStatement): AnalysisState = {
    val (stateAfterInitializer, initializerType) = statement.init match {
      case Some(initializer) => visitExpression(state, initializer)
      case None => (state, DataType.Unknown)
    }
    val arrayElementType = statement.init.flatMap(arrayElementTypeOf(_, stateAfterInitializer.environment))

    val (nextEnvironment, defined) =
      stateAfterInitializer.environment.defineVariable(
        statement.name,
        isInitialized = statement.init.nonEmpty,
        dataType = initializerType,
        arrayElementType = arrayElementType
      )

    val nextState = stateAfterInitializer.withEnvironment(nextEnvironment)
    if (defined) nextState
    else nextState.error(s"[Semantic Error] Переменная '${statement.name}' уже объявлена в этой области видимости.")
  }

  private def analyzeValStatement(state: AnalysisState, statement: ValStatement): AnalysisState = {
    val (stateAfterInitializer, initializerType) = visitExpression(state, statement.init)
    val arrayElementType = arrayElementTypeOf(statement.init, stateAfterInitializer.environment)
    val (nextEnvironment, defined) =
      stateAfterInitializer.environment.defineVariable(
        statement.name,
        isInitialized = true,
        dataType = initializerType,
        arrayElementType = arrayElementType
      )

    val nextState = stateAfterInitializer.withEnvironment(nextEnvironment)
    if (defined) nextState
    else nextState.error(s"[Semantic Error] Константа '${statement.name}' уже объявлена в этой области видимости.")
  }

  private def analyzePrintStatement(state: AnalysisState, statement: PrintStatement): AnalysisState = {
    visitExpression(state, statement.exp)._1
  }

  private def analyzeExpressionStatement(state: AnalysisState, statement: ExpressionStatement): AnalysisState = {
    visitExpression(state, statement.exp)._1
  }

  private def analyzeBlockStatement(state: AnalysisState, statement: BlockStatement): AnalysisState = {
    val blockState = state.withEnvironment(SemanticEnvironment(Some(state.environment)))
    val analyzedBlock = statement.statements.foldLeft(blockState)(visitStatement)
    val withUnusedWarnings = checkUnusedVariables(analyzedBlock)
    val outerEnvironment = withUnusedWarnings.environment.parent.getOrElse(state.environment)

    withUnusedWarnings.withEnvironment(outerEnvironment)
  }

  private def analyzeIfStatement(state: AnalysisState, statement: IfStatement): AnalysisState = {
    val (stateAfterCondition, conditionType) = visitExpression(state, statement.condition)

    val checkedCondition =
      if (conditionType != DataType.Bool && conditionType != DataType.Unknown) {
        stateAfterCondition.error(
          s"[Semantic Error] Условие 'if' должно быть логическим выражением (Bool), а получено: $conditionType."
        )
      } else stateAfterCondition

    val withReachabilityWarning =
      if (isAlwaysFalse(statement.condition)) {
        checkedCondition.warning(
          "[Semantic Warning] Обнаружен недостижимый код: ветка 'then' (if) никогда не выполнится."
        )
      } else checkedCondition

    val afterThen = visitStatement(withReachabilityWarning, statement.thenBranch)
    statement.elseBranch.fold(afterThen)(visitStatement(afterThen, _))
  }

  private def analyzeWhileStatement(state: AnalysisState, statement: WhileStatement): AnalysisState = {
    val (stateAfterCondition, conditionType) = visitExpression(state, statement.condition)

    val checkedCondition =
      if (conditionType != DataType.Bool && conditionType != DataType.Unknown) {
        stateAfterCondition.error(
          s"[Semantic Error] Условие 'while' должно быть логическим выражением (Bool), а получено: $conditionType."
        )
      } else stateAfterCondition

    val withReachabilityWarning =
      if (isAlwaysFalse(statement.condition)) {
        checkedCondition.warning(
          "[Semantic Warning] Обнаружен недостижимый код: тело цикла 'while' никогда не выполнится."
        )
      } else checkedCondition

    visitStatement(withReachabilityWarning, statement.body)
  }

  private def analyzeReturnStatement(state: AnalysisState, statement: ReturnStatement): AnalysisState = {
    statement.value match {
      case Some(value) => visitExpression(state, value)._1
      case None => state
    }
  }

  private def checkUnusedVariables(state: AnalysisState): AnalysisState = {
    state.environment.getLocalVariables.foldLeft(state) { (currentState, symbol) =>
      if (!symbol.isUsed) {
        currentState.warning(s"[Semantic Warning] Переменная '${symbol.name}' объявлена, но ни разу не использована.")
      } else currentState
    }
  }

  private def visitExpression(state: AnalysisState, expression: Expression): (AnalysisState, DataType) = {
    expression match {
      case _: NumberExpression => (state, DataType.Number)
      case _: StringExpression => (state, DataType.String)
      case expression: VariableExpression => analyzeVariableExpression(state, expression)
      case expression: AssignExpression => analyzeAssignExpression(state, expression)
      case expression: BinaryExpression => analyzeBinaryExpression(state, expression)
      case expression: UnaryExpression => analyzeUnaryExpression(state, expression)
      case expression: CallExpression => analyzeFunctionCallExpression(state, expression)
      case expression: ArrayExpression => analyzeArrayExpression(state, expression)
      case expression: IndexExpression => analyzeIndexExpression(state, expression)
      case expression: IndexAssignExpression => analyzeIndexAssignExpression(state, expression)
      case expression: IfExpression => analyzeIfExpression(state, expression)
      case expression: BlockExpression => analyzeBlockExpression(state, expression)
      case expression: ReturnExpression => visitExpression(state, expression.value)
      case _: LambdaExpression => (state, DataType.Function)
    }
  }

  private def analyzeVariableExpression(
                                         state: AnalysisState,
                                         expression: VariableExpression
                                       ): (AnalysisState, DataType) = {
    state.environment.getVariable(expression.name) match {
      case Some(symbol) =>
        val usedEnvironment = state.environment.markUsed(expression.name)
        val usedState = state.withEnvironment(usedEnvironment)

        val checkedState =
          if (!symbol.isInitialized) {
            usedState.error(s"[Semantic Error] Использование неинициализированной переменной '${expression.name}'.")
          } else usedState

        (checkedState, symbol.dataType)

      case None =>
        (
          state.error(s"[Semantic Error] Использование необъявленной переменной '${expression.name}'."),
          DataType.Unknown
        )
    }
  }

  private def analyzeAssignExpression(
                                       state: AnalysisState,
                                       expression: AssignExpression
                                     ): (AnalysisState, DataType) = {
    val (stateAfterValue, valueType) = visitExpression(state, expression.value)

    stateAfterValue.environment.getVariable(expression.name) match {
      case Some(symbol) =>
        val checkedState =
          if (
            symbol.dataType != DataType.Unknown &&
              valueType != DataType.Unknown &&
              symbol.dataType != valueType
          ) {
            stateAfterValue.error(
              s"[Semantic Error] Ошибка типов: нельзя присвоить значение типа $valueType переменной '${expression.name}' (ожидался тип ${symbol.dataType})."
            )
          } else stateAfterValue

        val fixedType =
          if (symbol.dataType == DataType.Unknown && valueType != DataType.Unknown) valueType
          else symbol.dataType
        val fixedArrayElementType =
          if (fixedType == DataType.Array) {
            symbol.arrayElementType.orElse(arrayElementTypeOf(expression.value, checkedState.environment))
          } else None

        val nextEnvironment = checkedState.environment
          .setInitialized(expression.name)
          .setType(expression.name, fixedType)
          .setArrayElementType(expression.name, fixedArrayElementType)

        (checkedState.withEnvironment(nextEnvironment), fixedType)

      case None =>
        (
          stateAfterValue.error(s"[Semantic Error] Попытка записи в необъявленную переменную '${expression.name}'."),
          valueType
        )
    }
  }

  private def analyzeArrayExpression(
                                      state: AnalysisState,
                                      expression: ArrayExpression
                                    ): (AnalysisState, DataType) = {
    val (stateAfterElements, elementTypes) =
      expression.elements.foldLeft((state, List.empty[DataType])) {
        case ((currentState, types), element) =>
          val (nextState, elementType) = visitExpression(currentState, element)
          (nextState, elementType :: types)
      }

    val knownTypes = elementTypes.reverse.filter(_ != DataType.Unknown)
    val checkedState =
      if (knownTypes.distinct.length > 1) {
        stateAfterElements.error(
          s"[Semantic Error] Все элементы массива должны быть одного типа. Получено: ${knownTypes.distinct.mkString(", ")}."
        )
      } else stateAfterElements

    (checkedState, DataType.Array)
  }

  private def analyzeIndexExpression(
                                      state: AnalysisState,
                                      expression: IndexExpression
                                    ): (AnalysisState, DataType) = {
    val (stateAfterArray, arrayType) = visitExpression(state, expression.array)
    val (stateAfterIndex, indexType) = visitExpression(stateAfterArray, expression.index)

    val checkedArray =
      if (arrayType != DataType.Array && arrayType != DataType.Unknown) {
        stateAfterIndex.error(s"[Semantic Error] Индексировать можно только массив, получено: $arrayType.")
      } else stateAfterIndex

    val checkedIndex =
      if (indexType != DataType.Number && indexType != DataType.Unknown) {
        checkedArray.error(s"[Semantic Error] Индекс массива должен быть числом (Number), получено: $indexType.")
      } else checkedArray

    (checkedIndex, arrayElementTypeOf(expression.array, checkedIndex.environment).getOrElse(DataType.Unknown))
  }

  private def analyzeIndexAssignExpression(
                                            state: AnalysisState,
                                            expression: IndexAssignExpression
                                          ): (AnalysisState, DataType) = {
    val (stateAfterArray, arrayType) = visitExpression(state, expression.array)
    val (stateAfterIndex, indexType) = visitExpression(stateAfterArray, expression.index)
    val (stateAfterValue, valueType) = visitExpression(stateAfterIndex, expression.value)

    val checkedArray =
      if (arrayType != DataType.Array && arrayType != DataType.Unknown) {
        stateAfterValue.error(s"[Semantic Error] Записывать по индексу можно только в массив, получено: $arrayType.")
      } else stateAfterValue

    val checkedIndex =
      if (indexType != DataType.Number && indexType != DataType.Unknown) {
        checkedArray.error(s"[Semantic Error] Индекс массива должен быть числом (Number), получено: $indexType.")
      } else checkedArray

    val elementType = arrayElementTypeOf(expression.array, checkedIndex.environment)
    val checkedValue =
      elementType match {
        case Some(expectedType) if expectedType != DataType.Unknown && valueType != DataType.Unknown && expectedType != valueType =>
          checkedIndex.error(
            s"[Semantic Error] Ошибка типов: нельзя записать значение типа $valueType в массив с элементами типа $expectedType."
          )
        case _ =>
          checkedIndex
      }

    (checkedValue, valueType)
  }

  private def analyzeFunctionCallExpression(
                                             state: AnalysisState,
                                             expression: CallExpression
                                           ): (AnalysisState, DataType) = {
    val checkedCallee = state.environment.getVariable(expression.calleeName) match {
      case Some(symbol) if symbol.dataType == DataType.Function =>
        val usedEnvironment = state.environment.markUsed(expression.calleeName)
        val usedState = state.withEnvironment(usedEnvironment)

        symbol.arity match {
          case Some(arity) if arity != expression.arguments.length =>
            usedState.error(
              s"[Semantic Error] Неверное количество аргументов при вызове функции '${expression.calleeName}'. Ожидалось: $arity, получено: ${expression.arguments.length}."
            )
          case _ =>
            usedState
        }

      case _ =>
        state.error(s"[Semantic Error] Вызов неопределенной функции '${expression.calleeName}'.")
    }

    val checkedArguments = expression.arguments.foldLeft(checkedCallee) { (currentState, argument) =>
      visitExpression(currentState, argument)._1
    }

    (checkedArguments, DataType.Unknown)
  }

  private def analyzeBinaryExpression(
                                       state: AnalysisState,
                                       expression: BinaryExpression
                                     ): (AnalysisState, DataType) = {
    val (stateAfterLeft, leftType) = visitExpression(state, expression.left)
    val (stateAfterRight, rightType) = visitExpression(stateAfterLeft, expression.right)

    if (leftType == DataType.Unknown || rightType == DataType.Unknown) {
      (stateAfterRight, DataType.Unknown)
    } else {
      expression.operator match {
        case TokenType.PLUS =>
          if (leftType == DataType.String || rightType == DataType.String) (stateAfterRight, DataType.String)
          else if (leftType == DataType.Number && rightType == DataType.Number) (stateAfterRight, DataType.Number)
          else typeError(stateAfterRight, expression.operator, leftType, rightType)

        case TokenType.MINUS | TokenType.STAR | TokenType.SLASH =>
          if (leftType == DataType.Number && rightType == DataType.Number) (stateAfterRight, DataType.Number)
          else {
            (
              stateAfterRight.error(
                s"[Semantic Error] Ошибка типов: оператор '${expression.operator}' работает только с числами (Number). Получено: $leftType и $rightType."
              ),
              DataType.Unknown
            )
          }

        case TokenType.LT | TokenType.GT | TokenType.LTEQ | TokenType.GTEQ =>
          if (leftType == DataType.Number && rightType == DataType.Number) (stateAfterRight, DataType.Bool)
          else {
            (
              stateAfterRight.error(
                s"[Semantic Error] Ошибка типов: операторы сравнения работают только с числами (Number). Получено: $leftType и $rightType."
              ),
              DataType.Unknown
            )
          }

        case TokenType.EQEQ | TokenType.NEQ =>
          val nextState =
            if (leftType != rightType) {
              stateAfterRight.warning(
                s"[Semantic Warning] Сравнение на равенство разных типов ($leftType и $rightType) всегда будет ложным."
              )
            } else stateAfterRight

          (nextState, DataType.Bool)

        case TokenType.AND | TokenType.OR =>
          if (leftType == DataType.Bool && rightType == DataType.Bool) (stateAfterRight, DataType.Bool)
          else {
            (
              stateAfterRight.error(
                s"[Semantic Error] Ошибка типов: логические операторы (&&, ||) требуют тип Bool. Получено: $leftType и $rightType."
              ),
              DataType.Unknown
            )
          }

        case _ =>
          (stateAfterRight, DataType.Unknown)
      }
    }
  }

  private def analyzeUnaryExpression(
                                      state: AnalysisState,
                                      expression: UnaryExpression
                                    ): (AnalysisState, DataType) = {
    val (stateAfterRight, rightType) = visitExpression(state, expression.r)

    if (rightType == DataType.Unknown) {
      (stateAfterRight, DataType.Unknown)
    } else {
      expression.operator match {
        case TokenType.MINUS =>
          if (rightType == DataType.Number) (stateAfterRight, DataType.Number)
          else {
            (
              stateAfterRight.error(
                s"[Semantic Error] Ошибка типов: унарный минус применяется только к числам. Получено: $rightType."
              ),
              DataType.Unknown
            )
          }

        case TokenType.EXCL =>
          if (rightType == DataType.Bool) (stateAfterRight, DataType.Bool)
          else {
            (
              stateAfterRight.error(
                s"[Semantic Error] Ошибка типов: оператор '!' применяется только к Bool. Получено: $rightType."
              ),
              DataType.Unknown
            )
          }

        case _ =>
          (stateAfterRight, rightType)
      }
    }
  }

  private def analyzeIfExpression(
                                   state: AnalysisState,
                                   expression: IfExpression
                                 ): (AnalysisState, DataType) = {
    val (stateAfterCondition, conditionType) = visitExpression(state, expression.condition)
    val checkedState =
      if (conditionType != DataType.Bool && conditionType != DataType.Unknown) {
        stateAfterCondition.error(
          s"[Semantic Error] Условие if-выражения должно быть логическим выражением (Bool), а получено: $conditionType."
        )
      } else stateAfterCondition

    val (stateAfterThen, thenType) = visitExpression(checkedState, expression.thenBranch)
    val (stateAfterElse, elseType) = visitExpression(stateAfterThen, expression.elseBranch)

    if (thenType == elseType) (stateAfterElse, thenType)
    else (stateAfterElse, DataType.Unknown)
  }

  private def analyzeBlockExpression(
                                      state: AnalysisState,
                                      expression: BlockExpression
                                    ): (AnalysisState, DataType) = {
    expression.expressions.foldLeft((state, DataType.Unknown)) {
      case ((currentState, _), innerExpression) => visitExpression(currentState, innerExpression)
    }
  }

  private def typeError(
                         state: AnalysisState,
                         operator: TokenType,
                         leftType: DataType,
                         rightType: DataType
                       ): (AnalysisState, DataType) = {
    (
      state.error(s"[Semantic Error] Ошибка типов: нельзя применить оператор '$operator' к $leftType и $rightType."),
      DataType.Unknown
    )
  }

  private def arrayElementTypeOf(expression: Expression, environment: SemanticEnvironment): Option[DataType] = {
    expression match {
      case ArrayExpression(elements) =>
        val knownTypes = elements.flatMap(staticTypeOf(_, environment)).filter(_ != DataType.Unknown).distinct
        knownTypes match {
          case singleType :: Nil => Some(singleType)
          case Nil => None
          case _ => None
        }
      case VariableExpression(name) =>
        environment.getVariable(name).flatMap(_.arrayElementType)
      case IndexExpression(array, _) =>
        arrayElementTypeOf(array, environment)
      case _ =>
        None
    }
  }

  private def staticTypeOf(expression: Expression, environment: SemanticEnvironment): Option[DataType] = {
    expression match {
      case _: NumberExpression => Some(DataType.Number)
      case _: StringExpression => Some(DataType.String)
      case _: ArrayExpression => Some(DataType.Array)
      case VariableExpression(name) => environment.getVariable(name).map(_.dataType)
      case IndexExpression(array, _) => arrayElementTypeOf(array, environment)
      case UnaryExpression(TokenType.MINUS, expression) =>
        staticTypeOf(expression, environment).filter(_ == DataType.Number)
      case UnaryExpression(TokenType.EXCL, expression) =>
        staticTypeOf(expression, environment).filter(_ == DataType.Bool)
      case BinaryExpression(left, operator, right) =>
        val leftType = staticTypeOf(left, environment)
        val rightType = staticTypeOf(right, environment)

        operator match {
          case TokenType.PLUS if leftType.contains(DataType.String) || rightType.contains(DataType.String) =>
            Some(DataType.String)
          case TokenType.PLUS | TokenType.MINUS | TokenType.STAR | TokenType.SLASH
            if leftType.contains(DataType.Number) && rightType.contains(DataType.Number) =>
            Some(DataType.Number)
          case TokenType.LT | TokenType.GT | TokenType.LTEQ | TokenType.GTEQ | TokenType.EQEQ | TokenType.NEQ =>
            Some(DataType.Bool)
          case TokenType.AND | TokenType.OR
            if leftType.contains(DataType.Bool) && rightType.contains(DataType.Bool) =>
            Some(DataType.Bool)
          case _ =>
            None
        }
      case IfExpression(_, thenBranch, elseBranch) =>
        val thenType = staticTypeOf(thenBranch, environment)
        val elseType = staticTypeOf(elseBranch, environment)
        thenType.filter(t => elseType.contains(t))
      case _ => None
    }
  }

  private def isAlwaysFalse(expression: Expression): Boolean = {
    expression match {
      case BinaryExpression(NumberExpression(left), TokenType.EQEQ, NumberExpression(right)) =>
        left != right
      case BinaryExpression(NumberExpression(left), TokenType.NEQ, NumberExpression(right)) =>
        left == right
      case _ =>
        false
    }
  }
}
