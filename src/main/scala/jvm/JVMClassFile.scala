package jvm

import jvm.JVMClassFileTypes._

case class JVMClassFile(fullPath: String,
                        jvmMajorVersion: Int,
                        jvmMinorVersion: Int,
                        packageName: Option[String],
                        className: String,
                        constants: Seq[Constant],
                        methods: Seq[MethodInfo],
                        fields: Seq[FieldInfo],
                        interfaces: Seq[ConstantClass]) {

  def getConstant(idx: Int): Constant = {
    // constant indexing is 1-based
    constants(idx - 1)
  }

  def getString(idx: Int): String = {
    getConstant(idx).asInstanceOf[ConstantUtf8].value
  }

  def getMethod(name: String): Option[MethodInfo] = {
    methods.find(v => getString(v.nameIndex) == name)
  }

  def getField(idx: Int): FieldInfo = {
    fields(idx)
  }

  def getMainMethod(): Option[MethodInfo] = {
    methods.find(v => getString(v.nameIndex) == "main")
  }
}
