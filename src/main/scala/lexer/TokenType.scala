package lexer

enum TokenType:
  case NUMBER, ID, STRING, VAR, VAL

  case PRINT
  case DEF, LARROW, RARROW, RETURN

  case IF, ELSE
  case WHILE, CONTINUE, BREAK

  case PLUS, MINUS, STAR, SLASH
  case EQ, EQEQ, NEQ, EXCL
  case LT, GT, LTEQ, GTEQ
  case AND, OR

  case LPAREN, RPAREN
  case LBRACE, RBRACE
  case LBRACKET, RBRACKET
  case SEMICOLON, COMMA

  case EOF
