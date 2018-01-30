package jvm

import jvm.JVMByteCode._

import scala.collection.mutable.ArrayBuffer

// All methods in here can act on a single stackframe with no knowledge of the surrounded JVM
// Prefer putting code here rather than the overcrowded JVM class
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
            val obj = next.asInstanceOf[JVMVarObject].o
            args += obj
            argTypes += obj.getClass
          }
        case v: JVMTypeVoid      =>
          JVM.err(s"not expecting void here")
          null
        case v: JVMTypeBoolean   =>
          args += next.asInstanceOf[JVMVarBoolean].v.asInstanceOf[Object]
          argTypes += Boolean.getClass
        case v: JVMTypeInt       =>
          args += next.asInstanceOf[JVMVarInt].v.asInstanceOf[Object]
          argTypes += Int.getClass
        case v: JVMTypeShort     =>
          args += next.asInstanceOf[JVMVarShort].v.asInstanceOf[Object]
          argTypes += Short.getClass
        case v: JVMTypeByte      =>
          args += next.asInstanceOf[JVMVarByte].v.asInstanceOf[Object]
          argTypes += Byte.getClass
        case v: JVMTypeChar      =>
          args += next.asInstanceOf[JVMVarChar].v.asInstanceOf[Object]
          argTypes += Char.getClass
        case v: JVMTypeFloat     =>
          args += next.asInstanceOf[JVMVarFloat].v.asInstanceOf[Object]
          argTypes += Float.getClass
        case v: JVMTypeDouble    =>
          args += next.asInstanceOf[JVMVarDouble].v.asInstanceOf[Object]
          argTypes += Double.getClass
        case v: JVMTypeLong      =>
          args += next.asInstanceOf[JVMVarLong].v.asInstanceOf[Object]
          argTypes += Long.getClass
        case v: JVMTypeArray     =>
          val array = next.asInstanceOf[JVMVarObjectRefUnmanaged].o
          v.typ match {
            case _: JVMTypeVoid    =>
              args += array.asInstanceOf[Array[Void]]
              argTypes += classOf[Array[Void]]
            case _: JVMTypeBoolean =>
              args += array.asInstanceOf[Array[Boolean]]
              argTypes += classOf[Array[Boolean]]
            case _: JVMTypeInt     =>
              args += array.asInstanceOf[Array[Int]]
              argTypes += classOf[Array[Int]]
            case _: JVMTypeShort   =>
              args += array.asInstanceOf[Array[Short]]
              argTypes += classOf[Array[Short]]
            case _: JVMTypeChar    =>
              args += array.asInstanceOf[Array[Char]]
              argTypes += classOf[Array[Char]]
            case _: JVMTypeByte    =>
              args += array.asInstanceOf[Array[Byte]]
              argTypes += classOf[Array[Byte]]
            case _: JVMTypeFloat   =>
              args += array.asInstanceOf[Array[Float]]
              argTypes += classOf[Array[Float]]
            case _: JVMTypeDouble  =>
              args += array.asInstanceOf[Array[Double]]
              argTypes += classOf[Array[Double]]
            case _: JVMTypeLong    =>
              args += array.asInstanceOf[Array[Long]]
              argTypes += classOf[Array[Long]]
            case _: JVMTypeString  =>
              args += array.asInstanceOf[Array[String]]
              argTypes += classOf[Array[String]]
          }
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
