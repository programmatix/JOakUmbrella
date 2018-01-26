package compiling

import compiling.JVMClassFileTypes.{CONSTANT, CONSTANT_Utf8_info, method_info}

import scala.collection.mutable.ArrayBuffer


trait JVMClassFileBuilder {
  def addConstant(constant: CONSTANT): Int = {
    constantPool += constant
    // constant indexing is 1-based
    constantPool.length
  }

  def getConstant(idx: Int): CONSTANT = {
    // constant indexing is 1-based
    constantPool(idx - 1)
  }

  protected val constantPool = ArrayBuffer.empty[CONSTANT]
  protected val methods = ArrayBuffer.empty[method_info]

  def addUTF8(value: String): Int = {
    constantPool += CONSTANT_Utf8_info(value)
    constantPool.length
  }

}

class JVMClassFileBuilderForReading (
                           /*
                              Java 1.2 uses major version 46
                              Java 1.3 uses major version 47
                              Java 1.4 uses major version 48
                              Java 5 uses major version 49
                              Java 6 uses major version 50
                              Java 7 uses major version 51
                              Java 8 uses major version 52
                              Java 9 uses major version 53
                            */
                           val jvmMajorVersion: Int,
                           val jvmMinorVersion: Int,
                           val packageName: Option[String],
                           val className: String

                         ) extends JVMClassFileBuilder  {
}
