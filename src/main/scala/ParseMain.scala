import generating.CGenerator
import parsing._
import pprint.PPrinter

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport

@JSExport
class ParseMain {
  private lazy val parser = new CParser
  private lazy val generator = new CGenerator

  // Generates the AST
  @JSExport
  def parse(in: String): String = {
    val pprintWidth = 3
    parser.parse(in) match {
      case CParseSuccess(x) =>
        js.Dynamic.global.console.info(x.toString)
        PPrinter.BlackWhite.apply(x, width = pprintWidth, height = 1000).toString
      case CParseFail(x)                     =>
        // Couldn't parse as a full C file, try again as though it's the inside of a function
        val parsedSnippet = parser.parseSnippet(in)
        parsedSnippet match {
          case CParseSuccess(y) =>
            PPrinter.BlackWhite.apply(y, width = pprintWidth, height = 1000).toString.replace("\n", "<br>")
          case CParseFail(_)    => parsedSnippet.toString
        }
    }
  }

  // Generates the C translation
  @JSExport
  def generate(in: String): String = {
    val parsedRaw = parser.parse(in)

    val genText: Seq[String] = parsedRaw match {
      case CParseSuccess(x) =>
        generator.generateTranslationUnit(x)
      case CParseFail(x)    =>
        // Couldn't parse as a full C file, try again as though it's the inside of a function
        val parsedSnippet = parser.parseSnippet(in)
        parsedSnippet match {
          case CParseSuccess(y) => generator.generateSeqBlockItem(y)
          case CParseFail(_)    => Seq(parsedSnippet.toString)
        }
    }

    genText.mkString(" ")
  }
}
