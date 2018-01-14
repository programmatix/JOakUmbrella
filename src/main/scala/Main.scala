import fastparse.core.Parsed
import parser.CParser

import scala.scalajs.js.annotation.JSExport

@JSExport
class Main {
  private lazy val parser = new CParser

//  def main(args: Array[String]): Unit = {
//  }

  @JSExport
  def parse(in: String): String = {
    val parsed = parser.parse(in)
    parsed match {
      case Parsed.Success(x, y) =>
        parsed.toString
      case Parsed.Failure(x, y, z) =>
        // Couldn't parse as a full C file, try again as though it's the inside of a function
        val parsedSnippet = parser.parseSnippet(in)
        parsedSnippet.toString
    }
  }
}
