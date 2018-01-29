package jvm

import jvm.JVMByteCode._

import scala.collection.mutable.ArrayBuffer

// All methods in here can act on a single stackframe with no knowledge of the surrounded JVM
object JVMStackFrame {
  private[jvm] def getMethodArgsAsObjects(sf: StackFrame, methodTypes: JVMMethodDescriptors.MethodDescriptor): (Seq[Object], Seq[Class[_]]) = {
    val args = ArrayBuffer.empty[Object]
    val argTypes = ArrayBuffer.empty[Class[_]]

    methodTypes.args.foreach(arg => {
      val next = sf.pop()
      arg match {
        case v: JVMTypeObjectStr =>
          if (v.clsRaw == "java/lang/String") {
            args += next.asInstanceOf[JVMVarString].v.asInstanceOf[Object]
            argTypes += classOf[java.lang.String]
          }
          else {
            JVM.err(s"Cannot handle $v yet")
            null
          }
        case v: JVMTypeVoid      =>
          JVM.err(s"not expecting void here")
          null
        case v: JVMTypeBoolean   =>
          args += next.asInstanceOf[JVMVarBoolean].v.asInstanceOf[Object]
          argTypes += Boolean.getClass
        case v: JVMTypeInt    =>
          args += next.asInstanceOf[JVMVarInt].v.asInstanceOf[Object]
          argTypes += Int.getClass
        case v: JVMTypeShort  =>
          args += next.asInstanceOf[JVMVarShort].v.asInstanceOf[Object]
          argTypes += Short.getClass
        case v: JVMTypeByte   =>
          args += next.asInstanceOf[JVMVarByte].v.asInstanceOf[Object]
          argTypes += Byte.getClass
        case v: JVMTypeChar   =>
          args += next.asInstanceOf[JVMVarChar].v.asInstanceOf[Object]
          argTypes += Char.getClass
        case v: JVMTypeFloat  =>
          args += next.asInstanceOf[JVMVarFloat].v.asInstanceOf[Object]
          argTypes += Float.getClass
        case v: JVMTypeDouble =>
          args += next.asInstanceOf[JVMVarDouble].v.asInstanceOf[Object]
          argTypes += Double.getClass
        case v: JVMTypeLong   =>
          args += next.asInstanceOf[JVMVarLong].v.asInstanceOf[Object]
          argTypes += Long.getClass
      }
    })

    (args.toVector, argTypes.toVector)
  }

  private[jvm] def getMethodArgs(sf: StackFrame, methodTypes: JVMMethodDescriptors.MethodDescriptor): Seq[JVMVar] = {
    val args = ArrayBuffer.empty[JVMVar]

    methodTypes.args.foreach(arg => {
      val next = sf.pop()
      args += next
    })

    args.toVector
  }


}
