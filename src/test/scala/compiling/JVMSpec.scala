package compiling

import compiling.JVMByteCode._
import org.scalatest.{BeforeAndAfter, FunSuite}

class JVMSpec extends FunSuite with BeforeAndAfter {

  test("Sample10Plus33") {
    val jvm = CompilingTestUtils.compileAndExecuteJavaFile("Sample10Plus33.java", "test").jvm
    assert (jvm.stack.last.stack.toArray sameElements Array(JVMVarInt(43)))
  }

  test("Sample10fPlus33f") {
    val jvm = CompilingTestUtils.compileAndExecuteJavaFile("Sample10fPlus33f.java").jvm
    assert (jvm.stack.last.stack.length == 1)
    assert (jvm.stack.last.stack.head == JVMVarFloat(43f))
  }

//  var cf: JVMClassFileBuilderForReading
//  var jvm: JVM
//
//  before {
//    var cr
//    jvm = new JVM()
//  }

//  test("2+3;") {
//    val opcodes = CompilingTestUtils.compileCToJVMOpcode("2+3;", isSnippet = true)
//    assert (opcodes.size == 3)
//    assert (opcodes(0).toString() == "bipush 2")
//    assert (opcodes(1).toString() == "bipush 3")
//    assert (opcodes(2).toString() == "iadd")
//    val jvm = createJVM
//    jvm.execute(opcodes)
//
//    assert(jvm.stack.last.stack.size == 1)
//    assert(jvm.stack.last.stack.head == 5)
//  }
//
//  test("2.0+3.0;") {
//    val opcodes = CompilingTestUtils.compileCToJVMOpcode("2+3;", isSnippet = true)
//    assert (opcodes.size == 3)
//    assert (opcodes(0).toString() == "bipush 2")
//    assert (opcodes(1).toString() == "bipush 3")
//    assert (opcodes(2).toString() == "iadd")
//    val jvm = createJVM
//    jvm.execute(opcodes)
//
//    assert(jvm.stack.last.stack.size == 1)
//    assert(jvm.stack.last.stack.head == 5)
//  }

}
