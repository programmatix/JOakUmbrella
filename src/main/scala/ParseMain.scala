import generating.{CGeneratedPrinter, CGenerator}
import parsing._
import pprint.PPrinter

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

object ParseMainUtils {
  def formatStringForHtml(in: String): String = {
    in.replace(" ", "&nbsp;").replace("\n", "<br>")
  }
}

@JSExportTopLevel("ParseMain")
class ParseMain {
  private lazy val parser = new CParser
  private lazy val generator = new CGenerator

  // Generates the AST
  @JSExport
  def parse(in: String): String = {
    val pprintWidth = 3

    val out = parser.parse(in) match {
      case CParseSuccess(x) =>
        PPrinter.BlackWhite.apply(x, width = pprintWidth, height = 1000).toString
      case CParseFail(x)    =>
        // Couldn't parse as a full C file, try again as though it's the inside of a function
        val parsedSnippet = parser.parseSnippet(in)
        parsedSnippet match {
          case CParseSuccess(y) =>
            PPrinter.BlackWhite.apply(y, width = pprintWidth, height = 1000).toString
          case CParseFail(_)    => parsedSnippet.toString
        }
    }

    ParseMainUtils.formatStringForHtml(out)
  }

  // Generates the C translation
  @JSExport
  def generate(in: String): String = {
    val parsedRaw = parser.parse(in)

    val genText: String = parsedRaw match {
      case CParseSuccess(x) =>
        CGeneratedPrinter.print(generator.generateTranslationUnit(x))
      case CParseFail(x)    =>
        // Couldn't parse as a full C file, try again as though it's the inside of a function
        val parsedSnippet = parser.parseSnippet(in)
        parsedSnippet match {
          case CParseSuccess(y) => CGeneratedPrinter.print(generator.generateSeqBlockItem(y))
          case CParseFail(_)    => parsedSnippet.toString
        }
    }

    ParseMainUtils.formatStringForHtml(genText)
  }
}
