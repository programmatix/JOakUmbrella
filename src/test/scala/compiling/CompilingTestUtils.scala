package compiling

import parsing._
import pprint.PPrinter

object CompilingTestUtils {
  def createParser() = new CParser
  def createByteCode() = new JVMByteCodeGenerator
  def createInterim() = new JVMByteCodeInterim

  def testCSnippetAgainstJVM(cCode: String, asmCode: String): Unit = {
    testCAgainstJVM(cCode, asmCode, isSnippet = true)
  }

  def testCTopAgainstJVM(cCode: String, asmCode: String): Unit = {
    testCAgainstJVM(cCode, asmCode, isSnippet = false)
  }

  private def testCAgainstJVM(cCode: String, asmCode: String, isSnippet: Boolean): Unit = {
    val p = createParser()
    val asm = createByteCode()
    val interimParser = createInterim()
    val writer = new JVMClassFileWriter()
    val split = asmCode.trim().replace("\r\n", "\n").split('\n')

    val parsed = if (isSnippet) p.parseSnippet(cCode) else p.parse(cCode)
    parsed match {
      case CParseSuccess(x) =>
        PPrinter.Color.log(x)
        val interim = x match {
          case v: Seq[BlockItem]  => asm.generateSeqBlockItem(v)
          case v: TranslationUnit => asm.generateTranslationUnit(v)
        }
        val generated = interimParser.parse(interim)
        val resolved = writer.process(generated)

        if (resolved.length != split.length) {
//          println(s"length mismatch\n${resolved.mkString("\n")} != \n$asmCode")
          PPrinter.Color.log(interim)
          PPrinter.Color.log(generated)
          PPrinter.Color.log(resolved)
          assert(false, s"length mismatch\n${resolved.mkString("\n")} \n!= \n\n$asmCode")
        }
        else {
          implicit val genParams: JVMByteCode.GenParams = JVMByteCode.GenParams()
          for (idx <- resolved.indices) {
            val a = resolved(idx)
            val s = split(idx)
//            val genned = a.gen(genParams)

            assert (a == s, s"${a} != $s")
          }
        }

      case CParseFail(v)=>
        assert(false, s"Parse fail on $cCode:\n$v")
    }

  }
}
