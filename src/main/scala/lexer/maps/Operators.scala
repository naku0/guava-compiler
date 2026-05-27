package lexer.maps

import lexer.TokenType

object Operators:
  val operators: Map[String, TokenType] = Map(
    "==" -> TokenType.EQEQ,
    "!=" -> TokenType.NEQ,
    "<=" -> TokenType.LTEQ,
    ">=" -> TokenType.GTEQ,
    "->" -> TokenType.RARROW,
    "<-" -> TokenType.LARROW,
    "&&" -> TokenType.AND,
    "||" -> TokenType.OR,
    "+" -> TokenType.PLUS,
    "-" -> TokenType.MINUS,
    "*" -> TokenType.STAR,
    "/" -> TokenType.SLASH,
    "=" -> TokenType.EQ,
    "<" -> TokenType.LT,
    ">" -> TokenType.GT,
    "!" -> TokenType.EXCL,
    "(" -> TokenType.LPAREN,
    ")" -> TokenType.RPAREN,
    "{" -> TokenType.LBRACE,
    "}" -> TokenType.RBRACE,
    "[" -> TokenType.LBRACKET,
    "]" -> TokenType.RBRACKET,
    ";" -> TokenType.SEMICOLON,
    "," -> TokenType.COMMA,
    "!!" -> TokenType.BREAK,
    "~>" -> TokenType.CONTINUE
  )

  def isOperator(char: Char): Boolean =
    operators.keys.exists(_.head == char)

  def isOperator(str: String): Boolean =
    operators.contains(str)

  def getTokenType(str: String): Option[TokenType] =
    operators.get(str)
