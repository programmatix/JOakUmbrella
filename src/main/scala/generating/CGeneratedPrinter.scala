package generating

// Takes a stream of Generated output and prints them
object CGeneratedPrinter {
  private val indentJump = 2

  def print(in: Seq[Generated]): String = {
    val sb = new StringBuilder

    var indent = 0
//    var startedNewLine = false

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
//        if (startedNewLine) {
//          sb ++= (" " * indent * indentJump)
//          startedNewLine = false
////          println("new line: '" + sb.toString  + "'")
//        }

        val next = v.v
        sb ++= next
//        if (next.last == '\n') {
//          startedNewLine = true
////          println(s"starting new line on $next")
//        }
//        else {
          sb ++= " "
//        }

      case v: GenStrings =>
        val it = v.v.iterator
        while (it.hasNext) {
//          if (startedNewLine) {
//            sb ++= (" " * indent * indentJump)
//            startedNewLine = false
////            println("new line: '" + sb.toString  + "'")
//          }

          val next = it.next()
          sb ++= next
//          if (next.last == '\n') {
////            println(s"starting new line on $next")
//            startedNewLine = true
//          }
//          else {
            sb ++= " "
//          }
        }
      case _: IndentUp   =>
        indent += 1
//        sb ++= (" " * indent * indentJump)
      case _: IndentDown =>
        indent -= 1
        assert(indent >= 0)
    }


    sb.toString
  }
}
