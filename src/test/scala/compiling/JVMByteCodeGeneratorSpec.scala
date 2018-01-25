package compiling

import org.scalatest.FunSuite

class JVMByteCodeGeneratorSpec extends FunSuite {
  ignore("return;") {
    CompilingTestUtils.testCSnippetAgainstJVM("return;",
      """return
      """.stripMargin)
  }

  test("return 2;") {
    CompilingTestUtils.testCSnippetAgainstJVM("return 2;",
      """sipush 2
        |ireturn
      """.stripMargin)
  }

  test("2+3*4;") {
    CompilingTestUtils.testCSnippetAgainstJVM("2+3*4;",
      """bipush 2
        |bipush 3
        |bipush 4
        |imul
        |iadd
      """.stripMargin)
  }


  test("int result = 2+3*4;") {
    CompilingTestUtils.testCSnippetAgainstJVM("int result = 2+3*4;",
      """bipush 2
        |bipush 3
        |bipush 4
        |imul
        |iadd
        |istore_0
      """.stripMargin)
  }


    test("func returns 2;") {
    CompilingTestUtils.testCTopAgainstJVM("""int main() {
                                                       |    return 2;
                                                       |}""".stripMargin,
      """public static int main();
        |Code:
        |sipush 2
        |ireturn
      """.stripMargin)
  }
}
