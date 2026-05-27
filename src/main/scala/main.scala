import generator.RandomProgramGenerator
import lexer.Lexer
import parser.Parser
import semantic.analyzer.SemanticAnalyzer

import java.nio.file.{Files, Path}
import scala.util.Try

@main
def main(args: String*): Unit = {
  CliArgs.parse(args.toList) match {
    case CliCommand.Help =>
      println(CliArgs.usage)
    case CliCommand.Generate(outputPath, statementCount) =>
      val code = RandomProgramGenerator().generate(statementCount)
      Files.writeString(Path.of(outputPath), code)
      println(s"Generated $outputPath")
    case CliCommand.Run(inputPath) =>
      runFile(inputPath)
  }
}

private def runFile(inputPath: String): Unit = {
  val source = Reader.readFromFile(inputPath)
  val tokens = Lexer.tokenize(source)

  Parser.parse(tokens) match {
    case Right(statements) =>
      val semanticResult = SemanticAnalyzer.analyze(statements)
      semanticResult.errors.foreach(println)
      semanticResult.warnings.foreach(println)
      if (semanticResult.errors.isEmpty) {
        statements.foreach(println)
      }
    case Left(error) =>
      println(error)
  }
}

private enum CliCommand:
  case Run(inputPath: String)
  case Generate(outputPath: String, statementCount: Int)
  case Help

private object CliArgs {
  val usage: String =
    """Usage:
      |  sbt "run [file]"
      |  sbt "run --file file.guava"
      |  sbt "run --generate [output.guava] [statementCount]"
      |  sbt "run --help"
      |""".stripMargin.trim

  def parse(args: List[String]): CliCommand = {
    args match {
      case Nil =>
        CliCommand.Run(defaultInputPath)
      case "--help" :: Nil | "-h" :: Nil =>
        CliCommand.Help
      case "--file" :: path :: Nil =>
        CliCommand.Run(path)
      case "--generate" :: Nil =>
        CliCommand.Generate("generated.guava", 10)
      case "--generate" :: outputPath :: Nil =>
        CliCommand.Generate(outputPath, 10)
      case "--generate" :: outputPath :: statementCount :: Nil =>
        CliCommand.Generate(outputPath, parseStatementCount(statementCount))
      case inputPath :: Nil if !inputPath.startsWith("--") =>
        CliCommand.Run(inputPath)
      case _ =>
        println(s"Неверные аргументы: ${args.mkString(" ")}")
        CliCommand.Help
    }
  }

  private def defaultInputPath: String = {
    if (Files.exists(Path.of("data.guava"))) "data.guava" else "data"
  }

  private def parseStatementCount(value: String): Int = {
    Try(value.toInt).toOption.filter(_ > 0).getOrElse(10)
  }
}
