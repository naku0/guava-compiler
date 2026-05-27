package lexer.maps

import lexer.TokenType
import lexer.TokenType.*

object Keywords {
  val keywords: Map[String, TokenType] = Map(
    "var" -> VAR,
    "val" -> VAL,
    "def" -> DEF,
    "return" -> RETURN,
    "print" -> PRINT,
    "if" -> IF,
    "else" -> ELSE,
    "while" -> WHILE,
    "continue" -> CONTINUE,
    "break" -> BREAK
   )
}
