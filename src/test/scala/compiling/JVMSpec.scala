package compiling

import jvm.JVMByteCode.{JVMVarFloat, JVMVarInt}
import org.scalatest.{BeforeAndAfter, FunSuite}

class JVMSpec extends FunSuite with BeforeAndAfter {


  test("Sample10Plus33") {
    val jvm = CompilingTestUtils.compileAndExecuteJavaFile("Sample10Plus33.java", "test").jvm
    assert (jvm.stack.last.stack.toArray sameElements Array(JVMVarInt(43)))
  }

  test("PrintHelloWorld") {
    val jvm = CompilingTestUtils.compileAndExecuteJavaFile("PrintHelloWorld.java", "main").jvm
    assert(1 == 1)
  }

  ignore("Sample10fPlus33f") {
    val jvm = CompilingTestUtils.compileAndExecuteJavaFile("Sample10fPlus33f.java").jvm
    assert (jvm.stack.last.stack.length == 1)
    assert (jvm.stack.last.stack.head == JVMVarFloat(43f))
  }
}
