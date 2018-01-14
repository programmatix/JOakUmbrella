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
    parsed.toString
  }
}
