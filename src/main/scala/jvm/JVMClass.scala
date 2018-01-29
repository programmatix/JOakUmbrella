package jvm

import jvm.JVMByteCode.{JVMType, JVMVar}

import scala.collection.mutable

// An instance of 'JVMClassFile'
class JVMClassInstance(cf: JVMClassFile) {
  private val fields = mutable.Map.empty[String, JVMVar]

  def getField(name: String): JVMVar = fields(name)

  def putField(name: String, typ: JVMType, value: JVMByteCode.JVMVar) = {
    fields(name) = value
  }
}
