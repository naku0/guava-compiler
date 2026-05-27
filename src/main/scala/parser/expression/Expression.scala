package parser.expression

import lexer.TokenType
import parser.statement.Statement

sealed trait Expression

case class NumberExpression(num: Double) extends Expression
case class StringExpression(str: String) extends Expression
case class VariableExpression(name: String) extends Expression
case class BinaryExpression(left: Expression,
                            operator: TokenType,
                            right: Expression) extends Expression
case class UnaryExpression(operator: TokenType, r: Expression) extends Expression
case class AssignExpression(name: String, value: Expression) extends Expression
case class CallExpression(calleeName: String, arguments: List[Expression]) extends Expression
case class ArrayExpression(elements: List[Expression]) extends Expression
case class IndexExpression(array: Expression, index: Expression) extends Expression
case class IndexAssignExpression(array: Expression, index: Expression, value: Expression) extends Expression
case class LambdaExpression(args: List[String], body: Statement) extends Expression
case class IfExpression(condition: Expression,
                        thenBranch: Expression,
                        elseBranch: Expression) extends Expression
case class BlockExpression(expressions: List[Expression]) extends Expression
case class ReturnExpression(value: Expression) extends Expression
