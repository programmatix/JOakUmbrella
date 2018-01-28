package compiling

import jvm.JVMByteCode.{JVMVarFloat, JVMVarInt}
import org.scalatest.FunSuite

class JVMSpec extends FunSuite {


  test("Sample10Plus33") {
    val jvm = CompilingTestUtils.compileAndExecuteJavaFile("Sample10Plus33.java", "test").jvm
    assert (jvm.stack.last.stack.toArray sameElements Array(JVMVarInt(43)))
  }

  test("PrintHelloWorld") {
    val jvm = CompilingTestUtils.compileAndExecuteJavaFile("PrintHelloWorld.java").jvm
    assert (jvm.stack.last.stack.isEmpty)
  }

  test("CallFunc") {
    val jvm = CompilingTestUtils.compileAndExecuteJavaFile("CallFunc.java").jvm
    assert (jvm.stack.last.stack.isEmpty)
  }

  test("ForLoop100") {
    val jvm = CompilingTestUtils.compileAndExecuteJavaFile("ForLoop100.java").jvm
    assert(CompilingTestUtils.containsVar(jvm.stack.head, JVMVarInt(100)))
  }

  test("SimpleIf") {
    val jvm = CompilingTestUtils.compileAndExecuteJavaFile("SimpleIf.java").jvm
    assert (jvm.stack.last.locals.values.last == JVMVarInt(40))
  }

  ignore("Sample10fPlus33f") {
    val jvm = CompilingTestUtils.compileAndExecuteJavaFile("Sample10fPlus33f.java").jvm
    assert (jvm.stack.last.stack.length == 1)
    assert (jvm.stack.last.stack.head == JVMVarFloat(43f))
  }
}
