import scala.io.Source
import scala.util.{Try, Using, Success, Failure}

object Reader:
  def readFromFile(fileName: String): String = {
    Try {
      Using.resource(Source.fromFile(fileName)) { source =>
        source.getLines().mkString("\n")
      }
    } match {
      case Success(text) if text.nonEmpty => text
      case Success(_) => throw new RuntimeException("Файл пуст")
      case Failure(exception) =>
        throw new RuntimeException(s"Ошибка чтения файла: ${exception.getMessage}")
    }
  }
