package generating

import generating.CGenerator._

// Takes a stream of Generated output and prints them
object CGeneratedPrinter {
  private val indentJump = 2

  def print(in: Seq[Generated]): String = {
    val sb = new StringBuilder

    var indent = 0

    in.foreach {
      case v: NewlineOnly =>
        sb += '\n'

      case v: NewlineAndIndent =>
        sb += '\n'
        sb ++= (" " * indent * indentJump)

      case v: NewlineAndIndentUp =>
        sb += '\n'
        indent += 1
        sb ++= (" " * indent * indentJump)

      case v: NewlineAndIndentDown =>
        sb += '\n'
        indent -= 1
        sb ++= (" " * indent * indentJump)

      case v: GenString =>
        val next = v.v
        sb ++= next
          sb ++= " "

      case v: GenStrings =>
        val it = v.v.iterator
        while (it.hasNext) {
          val next = it.next()
          sb ++= next
            sb ++= " "
        }
      case _: IndentUp   =>
        indent += 1
      case _: IndentDown =>
        indent -= 1
        assert(indent >= 0)
    }


    sb.toString
  }
}
