import fastparse.core.Parsed
import parser.{CGenerator, CParser}

import scala.scalajs.js.annotation.JSExport

@JSExport
class Main {
  private lazy val parser = new CParser
  private lazy val generator = new CGenerator

//  def main(args: Array[String]): Unit = {
//  }

  @JSExport
  def parse(in: String): String = {
    val parsedRaw = parser.parse(in)
    val parsed = parsedRaw match {
      case Parsed.Success(x, y) => parsedRaw
      case Parsed.Failure(x, y, z) =>
        // Couldn't parse as a full C file, try again as though it's the inside of a function
        parser.parseSnippet(in)
    }

    val parsedText: String = parsed match {
      case Parsed.Success(x, y) =>
        parsed.toString
      case Parsed.Failure(x, y, z) =>
        parsed.toString
    }

    parsedText
  }

  @JSExport
  def generate(in: String): String = {
    val parsedRaw = parser.parse(in)

    val genText: Seq[String] = parsedRaw match {
      case Parsed.Success(x, y) =>
        generator.generate(x)
      case Parsed.Failure(x, y, z) =>
        // Couldn't parse as a full C file, try again as though it's the inside of a function
        val parsedSnippet = parser.parseSnippet(in)
        parsedSnippet match {
          case Parsed.Success(x, y) =>
            generator.generate(x)
          case _ => Seq("")
        }

    }

    genText.mkString(" ")
  }
}
