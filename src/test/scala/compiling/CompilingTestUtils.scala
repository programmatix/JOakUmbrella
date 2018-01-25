package compiling

import parsing.{CParseFail, CParseSuccess, CParser}
import pprint.PPrinter

object CompilingTestUtils {
  def createParser() = new CParser
  def createByteCode() = new JVMByteCodeGenerator
  def createInterim() = new JVMByteCodeInterim

  def testCSnippetAgainstJVM(cCode: String, asmCode: String): Unit = {
    val p = createParser()
    val asm = createByteCode()
    val interimParser = createInterim()
    val split = asmCode.trim().replace("\r\n", "\n").split('\n')

    val parsed = p.parseSnippet(cCode)
    parsed match {
      case CParseSuccess(v) =>
        val interim = asm.generateSeqBlockItem(v)
        val generated = interimParser.parse(interim)

        if (generated.length != split.length) {
          PPrinter.Color.log(v)
          PPrinter.Color.log(interim)
          PPrinter.Color.log(generated)
          assert(false, s"length mismatch\n${generated.mkString("\n")} != \n$asmCode")
        }
        else {
          implicit val genParams: JVMByteCode.GenParams = JVMByteCode.GenParams()
          for (idx <- generated.indices) {
            val a = generated(idx)
            val s = split(idx)
            val genned = a.gen(genParams)

            assert (genned == s, s"${genned} != $s")
          }
        }

      case CParseFail(v)=>
        assert(false, s"Parse fail on $cCode")
    }

  }
}
