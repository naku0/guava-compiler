package parser

import lexer.{Token, TokenType}
import parser.expression.*
import parser.statement.{BlockStatement, ExpressionStatement, IfStatement, PrintStatement, Statement, VarStatement, WhileStatement}

import scala.annotation.tailrec

case class Parser(tokens: List[Token], position: Int = 0) {
  def isAtTheEnd: Boolean = position >= tokens.length || current.tokenType == TokenType.EOF
  def current: Token = if position < tokens.length then tokens(position) else tokens.last
  def peek: Token = current
  def previous: Token = tokens(position - 1)

  def advance: Parser = copy(position = position + 1)

  def check(types: TokenType*): Boolean =
    !isAtTheEnd && types.contains(current.tokenType)
    
  def consume(tokenType: TokenType, message: String): (Parser, Token) = {
    if (check(tokenType)) (advance, current)
    else throw new Exception(s"[Parser Error] Line ${current.line}: $message")
  }
}

object Parser {
  def parse(tokens: List[Token]): Either[String, List[Statement]] = {
    val parser = Parser(tokens.filter(_.tokenType != TokenType.EOF))
    parseProgram(parser).map(_._1)
  }

  private def parseProgram(parser: Parser): Either[String, (List[Statement], Parser)] = {
    @tailrec
    def loop(s: Parser, acc: List[Statement]): Either[String, (List[Statement], Parser)] = {
      if s.isAtTheEnd then Right((acc.reverse, s))
      else parseDeclaration(s) match {
        case Right((stmt, next)) => loop(next, stmt::acc)
        case Left(error) => Left(error)
      }
    }
    loop(parser, Nil)
  }

  private def parseDeclaration(parser: Parser): Either[String, (Statement, Parser)] = {
    if (parser.check(TokenType.VAR)) parseVarDeclaration(parser.advance)
    else parseStatement(parser)
  }

  private def parseStatement(parser: Parser): Either[String, (Statement, Parser)] = parser.current.tokenType match {
    case TokenType.IF => parseIfStatement(parser.advance)
    case TokenType.WHILE => parseWhileStatement(parser.advance)
    case TokenType.PRINT => parsePrintStatement(parser.advance)
    case TokenType.LBRACE => {
      parseBlock(parser.advance).map{ case(stmt, next) =>
        (BlockStatement(stmt), next)
      }
    }
    case _ => parseExpressionStatement(parser)
  }

  private def parseVarDeclaration(parser: Parser): Either[String, (Statement, Parser)] = {
    val (s1, token) = parser.consume(TokenType.ID, "Ожидается имя переменной.")

    val (s2, init) = if (s1.check(TokenType.EQ)){
      parseExpression(s1.advance) match {
        case Right((expr, s)) => (s, Some(expr))
        case Left(error) => return Left(error)
      }
    } else (s1, None)

    val (s3, _) = s2.consume(TokenType.SEMICOLON, "Ожидается ';' после объявления переменной.")
    Right((VarStatement(token.value, init), s3))
  }

  private def parseIfStatement(parser: Parser): Either[String, (Statement, Parser)] = {
    val (s1, _) = parser.consume(TokenType.LPAREN, "Ожидается '(' после 'if'.")

    val (condition, s2) = parseExpression(s1) match {
      case Right(res) => res
      case Left(err) => return Left(err)
    }

    val (s3, _) = s2.consume(TokenType.RPAREN, "Ожидается ')' после условия 'if'.")

    val (thenBranch, s4) = parseStatement(s3) match {
      case Right(res) => res
      case Left(err) => return Left(err)
    }

    val (s5, elseBranch) = if (s4.check(TokenType.ELSE)){
      parseStatement(s4.advance) match {
        case Right((stmt, s)) => (s, Some(stmt))
        case Left(error) => return Left(error)
      }
    } else (s4, None)
    Right((IfStatement(condition, thenBranch, elseBranch), s5))
  }

  private def parseWhileStatement(parser: Parser): Either[String, (Statement, Parser)] = {
    val (s1, _) = parser.consume(TokenType.LPAREN,  "Ожидается '(' после 'while'.")

    val (cond, s2) = parseExpression(s1) match {
      case Right(res) => res
      case Left(err) => return Left(err)
    }

    val (s3, _) = s2.consume(TokenType.RPAREN, "Ожидается ')' после условия 'while'.")

    val (body, s4) = parseStatement(s3) match {
      case Right(res) => res
      case Left(err) => return Left(err)
    }

    Right((WhileStatement(cond, body), s4))
  }

  private def parsePrintStatement(parser: Parser): Either[String, (Statement, Parser)] = {
    val (value, s1) = parseExpression(parser) match {
      case Right(res) => res
      case Left(err) => return Left(err)
    }

    val (s2, _) = s1.consume(TokenType.SEMICOLON, "Ожидается ';' после значения.")
    Right((PrintStatement(value), s2))
  }

  private def parseExpressionStatement(parser: Parser): Either[String, (Statement, Parser)] = {
    val (expr, s1) = parseExpression(parser) match {
      case Right(result) => result
      case Left(error) => return Left(error)
    }

    val (s2, _) = s1.consume(TokenType.SEMICOLON, "Ожидается ';' после выражения.")
    Right((ExpressionStatement(expr), s2))
  }

  private def parseBlock(parser: Parser): Either[String, (List[Statement], Parser)] = {
    @tailrec
    def loop(s: Parser, acc: List[Statement]): Either[String, (List[Statement], Parser)] = {
      if (s.check(TokenType.RBRACE))
        Right((acc.reverse, s.advance))
      else if (s.isAtTheEnd)
        Left(s"[Parser Error] Line ${s.current.line}: Ожидается '}' после блока.")
      else {
        parseDeclaration(s) match {
          case Right((stmt, next)) => loop(next, stmt :: acc)
          case Left(error) => Left(error)
        }
      }
    }
    loop(parser, Nil)
  }

  private def parseExpression(parser: Parser): Either[String, (Expression, Parser)] = 
    parseAssignment(parser)
    
  private def parseAssignment(parser: Parser): Either[String, (Expression, Parser)] = {
    parseLogicalOr(parser) match {
      case Right((expr, s1)) if s1.check(TokenType.EQ) =>
        val equals = s1.current
        parseAssignment(s1.advance) match {
          case Right((value, s2)) =>
            expr match {
              case varExpr: VariableExpression =>
                Right((AssignExpression(varExpr.name, value), s2))
              case indexExpr: IndexExpression =>
                Right((IndexAssignExpression(indexExpr.array, indexExpr.index, value), s2))
              case _ =>
                Left(s"[Parser Error] Line ${equals.line}: Недопустимая цель для присваивания.")
            }
          case Left(error) => Left(error)
        }
      case Right((expr, s1)) => Right((expr, s1))
      case Left(error) => Left(error)
    }
  }

  private def parseLogicalOr(state: Parser): Either[String, (Expression, Parser)] = {
    parseLogicalAnd(state) match {
      case Right((expr, s)) if s.check(TokenType.OR) =>
        val op = s.current.tokenType
        parseLogicalOr(s.advance) match {
          case Right((right, s2)) =>
            Right((BinaryExpression(expr, op, right), s2))
          case Left(error) => Left(error)
        }
      case Right((expr, s)) => Right((expr, s))
      case Left(error) => Left(error)
    }
  }

  private def parseLogicalAnd(state: Parser): Either[String, (Expression, Parser)] = {
    parseEquality(state) match {
      case Right((expr, s)) if s.check(TokenType.AND) =>
        val op = s.current.tokenType
        parseLogicalAnd(s.advance) match {
          case Right((right, s2)) =>
            Right((BinaryExpression(expr, op, right), s2))
          case Left(error) => Left(error)
        }
      case Right((expr, s)) => Right((expr, s))
      case Left(error) => Left(error)
    }
  }

  private def parseEquality(state: Parser): Either[String, (Expression, Parser)] = {
    parseComparison(state) match {
      case Right((expr, s)) if s.check(TokenType.EQEQ, TokenType.NEQ) =>
        val op = s.current.tokenType
        parseEquality(s.advance) match {
          case Right((right, s2)) =>
            Right((BinaryExpression(expr, op, right), s2))
          case Left(error) => Left(error)
        }
      case Right((expr, s)) => Right((expr, s))
      case Left(error) => Left(error)
    }
  }

  private def parseComparison(state: Parser): Either[String, (Expression, Parser)] = {
    parseTerm(state) match {
      case Right((expr, s)) if s.check(TokenType.LT, TokenType.LTEQ, TokenType.GT, TokenType.GTEQ) =>
        val op = s.current.tokenType
        parseComparison(s.advance) match {
          case Right((right, s2)) =>
            Right((BinaryExpression(expr, op, right), s2))
          case Left(error) => Left(error)
        }
      case Right((expr, s)) => Right((expr, s))
      case Left(error) => Left(error)
    }
  }

  private def parseTerm(state: Parser): Either[String, (Expression, Parser)] = {
    parseFactor(state) match {
      case Right((expr, s)) if s.check(TokenType.PLUS, TokenType.MINUS) =>
        val op = s.current.tokenType
        parseTerm(s.advance) match {
          case Right((right, s2)) =>
            Right((BinaryExpression(expr, op, right), s2))
          case Left(error) => Left(error)
        }
      case Right((expr, s)) => Right((expr, s))
      case Left(error) => Left(error)
    }
  }

  private def parseFactor(state: Parser): Either[String, (Expression, Parser)] = {
    parseUnary(state) match {
      case Right((expr, s)) if s.check(TokenType.STAR, TokenType.SLASH) =>
        val op = s.current.tokenType
        parseFactor(s.advance) match {
          case Right((right, s2)) =>
            Right((BinaryExpression(expr, op, right), s2))
          case Left(error) => Left(error)
        }
      case Right((expr, s)) => Right((expr, s))
      case Left(error) => Left(error)
    }
  }

  private def parseUnary(state: Parser): Either[String, (Expression, Parser)] = {
    if (state.check(TokenType.EXCL, TokenType.MINUS)) {
      val op = state.current.tokenType
      parseUnary(state.advance) match {
        case Right((expr, s)) =>
          Right((UnaryExpression(op, expr), s))
        case Left(error) => Left(error)
      }
    } else parsePostfix(state)
  }

  private def parsePostfix(state: Parser): Either[String, (Expression, Parser)] = {
    parsePrimary(state) match {
      case Right((expr, next)) => parseIndexAccess(expr, next)
      case Left(error) => Left(error)
    }
  }

  @tailrec
  private def parseIndexAccess(expr: Expression, state: Parser): Either[String, (Expression, Parser)] = {
    if (state.check(TokenType.LBRACKET)) {
      parseExpression(state.advance) match {
        case Right((index, s1)) =>
          val (s2, _) = s1.consume(TokenType.RBRACKET, "Ожидается ']' после индекса.")
          parseIndexAccess(IndexExpression(expr, index), s2)
        case Left(error) => Left(error)
      }
    } else {
      Right((expr, state))
    }
  }

  private def parsePrimary(state: Parser): Either[String, (Expression, Parser)] = {
    if (state.check(TokenType.NUMBER)) {
      val value = state.current.value.toDouble
      Right((NumberExpression(value), state.advance))
    } else if (state.check(TokenType.STRING)) {
      Right((StringExpression(state.current.value), state.advance))
    } else if (state.check(TokenType.ID)) {
      Right((VariableExpression(state.current.value), state.advance))
    } else if (state.check(TokenType.LBRACKET)) {
      parseArrayExpression(state.advance)
    } else if (state.check(TokenType.LPAREN)) {
      parseExpression(state.advance) match {
        case Right((expr, s1)) =>
          val (s2, _) = s1.consume(TokenType.RPAREN, "Ожидается ')' после выражения.")
          Right((expr, s2))
        case Left(error) => Left(error)
      }
    } else {
      Left(s"[Parser Error] Line ${state.current.line}: Ожидается выражение.")
    }
  }

  private def parseArrayExpression(parser: Parser): Either[String, (Expression, Parser)] = {
    @tailrec
    def loop(state: Parser, acc: List[Expression]): Either[String, (Expression, Parser)] = {
      if (state.check(TokenType.RBRACKET)) {
        Right((ArrayExpression(acc.reverse), state.advance))
      } else {
        parseExpression(state) match {
          case Right((element, s1)) if s1.check(TokenType.COMMA) =>
            loop(s1.advance, element :: acc)
          case Right((element, s1)) =>
            val (s2, _) = s1.consume(TokenType.RBRACKET, "Ожидается ']' после элементов массива.")
            Right((ArrayExpression((element :: acc).reverse), s2))
          case Left(error) =>
            Left(error)
        }
      }
    }

    loop(parser, Nil)
  }

}
