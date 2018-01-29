package jvm

import jvm.JVMClassFileTypes._

import scala.collection.mutable.ArrayBuffer


trait JVMClassFileBuilder {
  def addConstant(constant: Constant): Int = {
    constants += constant
    // constant indexing is 1-based
    constants.length
  }

  def addMethod(method: MethodInfo): Unit = {
    methods += method
  }

  def getConstant(idx: Int): Constant = {
    // constant indexing is 1-based
    constants(idx - 1)
  }

  val constants = ArrayBuffer.empty[Constant]
  val methods = ArrayBuffer.empty[MethodInfo]

  def addUTF8(value: String): Int = {
    constants += ConstantUtf8(value)
    constants.length
  }

  def getString(idx: Int): String = {
    getConstant(idx).asInstanceOf[ConstantUtf8].value
  }

  def getMethod(name: String): Option[MethodInfo] = {
    methods.find(v => getString(v.nameIndex) == name)
  }

  def getMainMethod(): Option[MethodInfo] = {
    methods.find(v => getString(v.nameIndex) == "main")
  }
}

class JVMClassFileBuilderForReading(
                                   // The .class file
                                     val fullPath: String,
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

                                   ) extends JVMClassFileBuilder {
  def makeImmutable(): JVMClassFile = {
    JVMClassFile(fullPath,
      jvmMajorVersion,
      jvmMinorVersion,
      packageName,
      className,
      constants.toVector,
      methods.toVector)
  }
}


