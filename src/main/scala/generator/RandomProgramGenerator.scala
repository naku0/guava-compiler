package generator

import lexer.TokenType

import scala.collection.mutable
import scala.util.Random

final class RandomProgramGenerator(random: Random = Random()) {
  private enum ValueKind:
    case Number, String, NumberArray, StringArray

  private val varPool = Vector("x", "y", "z", "alpha", "beta", "count", "total", "index", "sum", "items")
  private val declaredVars = mutable.LinkedHashMap.empty[String, ValueKind]
  private val usedNames = mutable.Set.empty[String]

  private val mathOps = Vector(TokenType.PLUS, TokenType.MINUS, TokenType.STAR, TokenType.SLASH)
  private val compareOps = Vector(TokenType.EQEQ, TokenType.NEQ, TokenType.LT, TokenType.GT, TokenType.LTEQ, TokenType.GTEQ)
  private val logicOps = Vector(TokenType.AND, TokenType.OR)

  def generate(statementCount: Int = 10): String = {
    declaredVars.clear()
    usedNames.clear()

    val builder = StringBuilder()
    (0 until 3).foreach(_ => builder.append(generateVarDeclaration(0)).append('\n'))
    generateBlock(builder, statementCount, 0)
    builder.toString()
  }

  private def generateBlock(builder: StringBuilder, count: Int, indentLevel: Int): Unit = {
    val indent = indentation(indentLevel)

    (0 until count).foreach { _ =>
      val statementType =
        if (indentLevel > 2) random.nextInt(4)
        else random.nextInt(6)

      statementType match {
        case 0 =>
          builder.append(generateVarDeclaration(indentLevel)).append('\n')
        case 1 =>
          builder.append(generateAssignment(indentLevel)).append('\n')
        case 2 =>
          builder.append(s"$indent${lexeme(TokenType.PRINT)} ${generatePrintableExpression()};").append('\n')
        case 3 =>
          builder.append(s"$indent${lexeme(TokenType.IF)} (${generateCondition()}) {").append('\n')
          generateBlock(builder, random.between(1, 4), indentLevel + 1)

          if (random.nextDouble() > 0.5) {
            builder.append(s"$indent} ${lexeme(TokenType.ELSE)} {").append('\n')
            generateBlock(builder, random.between(1, 3), indentLevel + 1)
          }

          builder.append(s"$indent}").append('\n')
        case 4 =>
          builder.append(s"$indent${lexeme(TokenType.WHILE)} (${generateMostlyFalseCondition()}) {").append('\n')
          generateBlock(builder, random.between(1, 3), indentLevel + 1)
          builder.append(s"$indent}").append('\n')
        case _ =>
          builder.append(generateArrayMutation(indentLevel)).append('\n')
      }
    }
  }

  private def generateVarDeclaration(indentLevel: Int): String = {
    val name = freshName()
    val kind = randomKind()
    val initializer = generateExpression(kind)

    if (indentLevel == 0) {
      declaredVars(name) = kind
    }

    s"${indentation(indentLevel)}${lexeme(TokenType.VAR)} $name ${lexeme(TokenType.EQ)} $initializer;"
  }

  private def generateAssignment(indentLevel: Int): String = {
    if (declaredVars.isEmpty) {
      generateVarDeclaration(indentLevel)
    } else {
      val (name, kind) = randomVariable()
      s"${indentation(indentLevel)}$name ${lexeme(TokenType.EQ)} ${generateExpression(kind)};"
    }
  }

  private def generateArrayMutation(indentLevel: Int): String = {
    val arrays = declaredVars.toVector.collect {
      case entry @ (_, ValueKind.NumberArray | ValueKind.StringArray) => entry
    }

    if (arrays.isEmpty) {
      generateVarDeclaration(indentLevel)
    } else {
      val (name, kind) = arrays(random.nextInt(arrays.length))
      val valueKind = if (kind == ValueKind.NumberArray) ValueKind.Number else ValueKind.String
      s"${indentation(indentLevel)}$name[${random.nextInt(3)}] ${lexeme(TokenType.EQ)} ${generateExpression(valueKind)};"
    }
  }

  private def generateExpression(kind: ValueKind): String = {
    kind match {
      case ValueKind.Number =>
        generateNumberExpression()
      case ValueKind.String =>
        generateStringExpression()
      case ValueKind.NumberArray =>
        generateArrayLiteral(ValueKind.Number)
      case ValueKind.StringArray =>
        generateArrayLiteral(ValueKind.String)
    }
  }

  private def generatePrintableExpression(): String = {
    if (declaredVars.nonEmpty && random.nextDouble() > 0.35) {
      val (name, kind) = randomVariable()
      kind match {
        case ValueKind.NumberArray | ValueKind.StringArray if random.nextBoolean() =>
          s"$name[${random.nextInt(3)}]"
        case _ =>
          name
      }
    } else {
      generateExpression(randomKind())
    }
  }

  private def generateNumberExpression(): String = {
    val numberVars = declaredVars.collect { case (name, ValueKind.Number) => name }.toVector

    if (numberVars.nonEmpty && random.nextDouble() > 0.45) {
      if (random.nextDouble() > 0.55) {
        numberVars(random.nextInt(numberVars.length))
      } else {
        val left = randomNumberAtom(numberVars)
        val right = randomNumberAtom(numberVars, nonZero = true)
        val op = mathOps(random.nextInt(mathOps.length))
        s"$left ${lexeme(op)} $right"
      }
    } else {
      random.between(1, 100).toString
    }
  }

  private def generateStringExpression(): String = {
    val stringVars = declaredVars.collect { case (name, ValueKind.String) => name }.toVector
    if (stringVars.nonEmpty && random.nextDouble() > 0.6) {
      stringVars(random.nextInt(stringVars.length))
    } else {
      val words = Vector("guava", "compiler", "scala", "token", "array")
      "\"" + words(random.nextInt(words.length)) + "\""
    }
  }

  private def generateArrayLiteral(elementKind: ValueKind): String = {
    val elements = (0 until 3).map(_ => generateExpression(elementKind)).mkString(", ")
    s"[$elements]"
  }

  private def generateCondition(): String = {
    val condition = generateComparison()

    if (random.nextDouble() > 0.7) {
      val logicOp = logicOps(random.nextInt(logicOps.length))
      s"($condition) ${lexeme(logicOp)} (${generateComparison()})"
    } else {
      condition
    }
  }

  private def generateMostlyFalseCondition(): String = {
    if (random.nextDouble() > 0.35) {
      s"${random.between(1, 40)} ${lexeme(TokenType.EQEQ)} ${random.between(41, 80)}"
    } else {
      generateCondition()
    }
  }

  private def generateComparison(): String = {
    val op = compareOps(random.nextInt(compareOps.length))
    s"${generateNumberExpression()} ${lexeme(op)} ${generateNumberExpression()}"
  }

  private def randomNumberAtom(numberVars: Vector[String], nonZero: Boolean = false): String = {
    if (numberVars.nonEmpty && random.nextBoolean()) {
      numberVars(random.nextInt(numberVars.length))
    } else {
      val lowerBound = if (nonZero) 1 else 0
      random.between(lowerBound, 100).toString
    }
  }

  private def randomKind(): ValueKind = {
    random.nextInt(4) match {
      case 0 => ValueKind.Number
      case 1 => ValueKind.String
      case 2 => ValueKind.NumberArray
      case _ => ValueKind.StringArray
    }
  }

  private def randomVariable(): (String, ValueKind) = {
    val entries = declaredVars.toVector
    entries(random.nextInt(entries.length))
  }

  private def freshName(): String = {
    val baseName = varPool(random.nextInt(varPool.length))
    if (!usedNames.contains(baseName)) {
      usedNames += baseName
      baseName
    } else {
      val name = s"${baseName}${usedNames.size}"
      usedNames += name
      name
    }
  }

  private def indentation(level: Int): String =
    " " * (level * 4)

  private def lexeme(tokenType: TokenType): String = {
    tokenType match {
      case TokenType.VAR => "var"
      case TokenType.PRINT => "print"
      case TokenType.IF => "if"
      case TokenType.ELSE => "else"
      case TokenType.WHILE => "while"
      case TokenType.PLUS => "+"
      case TokenType.MINUS => "-"
      case TokenType.STAR => "*"
      case TokenType.SLASH => "/"
      case TokenType.EQ => "="
      case TokenType.EQEQ => "=="
      case TokenType.NEQ => "!="
      case TokenType.LT => "<"
      case TokenType.GT => ">"
      case TokenType.LTEQ => "<="
      case TokenType.GTEQ => ">="
      case TokenType.AND => "&&"
      case TokenType.OR => "||"
      case other => throw IllegalArgumentException(s"Нет лексемы для генерации токена $other.")
    }
  }
}
