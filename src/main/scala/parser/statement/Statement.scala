package parser.statement

import parser.expression.Expression

sealed trait Statement

case class ExpressionStatement(exp: Expression) extends Statement
case class PrintStatement(exp: Expression) extends Statement
case class VarStatement(name: String, init: Option[Expression]) extends Statement
case class ValStatement(name: String, init: Expression) extends Statement
case class BlockStatement(statements: List[Statement]) extends Statement
case class IfStatement(condition: Expression,
                       thenBranch: Statement,
                       elseBranch: Option[Statement]) extends Statement
case class WhileStatement(condition: Expression, body: Statement) extends Statement
case class FuncStatement(name: String, args: List[String], body: Statement) extends Statement
case class ReturnStatement(value: Option[Expression]) extends Statement
case class BreakStatement() extends Statement
case class ContinueStatement() extends Statement
