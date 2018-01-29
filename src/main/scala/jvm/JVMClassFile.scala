package jvm

import jvm.JVMClassFileTypes.{Constant, ConstantUtf8, FieldInfo, MethodInfo}

case class JVMClassFile(fullPath: String,
                        jvmMajorVersion: Int,
                        jvmMinorVersion: Int,
                        packageName: Option[String],
                        className: String,
                        constants: Seq[Constant],
                        methods: Seq[MethodInfo],
                        fields: Seq[FieldInfo]) {

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

  def getMainMethod(): Option[MethodInfo] = {
    methods.find(v => getString(v.nameIndex) == "main")
  }
}
