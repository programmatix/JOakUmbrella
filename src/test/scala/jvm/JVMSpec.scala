package jvm

import jvm.JVMByteCode.{JVMVarInt, JVMVarObjectRefUnmanaged, JVMVarString}
import org.scalatest.FunSuite

class JVMSpec extends FunSuite {


  test("Sample10Plus33") {
    val jvm = CompilingTestUtils.compileAndExecuteJavaFileX("Sample10Plus33.java", "Sample10Plus33", "test", (sf) => {
      assert (sf.stack.toArray sameElements Array(JVMVarInt(43)))
    }).jvm
  }

  test("PrintHelloWorld") {
    val jvm = CompilingTestUtils.compileAndExecuteJavaFile("PrintHelloWorld.java").jvm
  }

  test("CallFunc") {
    val jvm = CompilingTestUtils.compileAndExecuteJavaFile("CallFunc.java").jvm
  }

  test("ForLoop100") {
    val jvm = CompilingTestUtils.compileAndExecuteJavaFile("ForLoop100.java", (sf) => {
      assert(CompilingTestUtils.containsVar(sf, JVMVarInt(100)))
    }).jvm

  }

  test("SimpleIf") {
    val jvm = CompilingTestUtils.compileAndExecuteJavaFile("SimpleIf.java", (sf) => {
      assert (sf.locals.values.last == JVMVarInt(40))
    }).jvm

  }

  test("ReturnValFromFunc") {
    var retIdx = 0
    val jvm = CompilingTestUtils.compileAndExecuteJavaFile("ReturnValFromFunc.java", (sf) => {
      if (retIdx == 1) {
        assert(CompilingTestUtils.containsVar(sf, JVMVarInt(8)))
      }
      retIdx += 1
    }).jvm

  }

  test("CreateString") {
    val jvm = CompilingTestUtils.compileAndExecuteJavaFile("CreateString.java", (sf) => {
      assert (sf.locals.values.last.asInstanceOf[JVMVarObjectRefUnmanaged].o.asInstanceOf[String] == "hello")
    }).jvm
  }

  test("CreateOwnClass") {
    val jvm = CompilingTestUtils.compileAndExecuteJavaFile("CreateOwnClass.java", (sf) => {
      assert(CompilingTestUtils.getKlassInstanceLocal(sf).getField("str") == JVMVarString("hello"))
    }).jvm
  }

}
