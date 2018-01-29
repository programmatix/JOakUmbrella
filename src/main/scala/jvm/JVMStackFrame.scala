package jvm

import jvm.JVMByteCode._
import jvm.JVMClassFileTypes.{ConstantClass, ConstantFieldref, ConstantNameAndType}

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
            JVM.err(s"Cannot handle $v yet")
            null
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

  // Returns JVMClassInstance if it's a managed class, else None and the (NEWINST1) procedure kicks in
  private[jvm] def createNewInstance(sf: StackFrame, cls: ConstantClass, classLoader: JVMClassLoader, systemClassLoader: ClassLoader, params: ExecuteParams): Option[JVMVar] = {
    val cf = sf.cf
    //  java/io/PrintStream
    val className = cf.getString(cls.nameIndex)

    val resolvedClassName = className.replace("/", ".")

    // See JVMClassLoader for a description of what's going on here
    classLoader.loadClass(resolvedClassName) match {

      case Some(clsRef) =>
        val klass = new JVMClassInstance(clsRef)
        Some(JVMVarObjectRefManaged(klass))

      case _ =>
//        val clsRef = systemClassLoader.loadClass(resolvedClassName)
        // Class.getDeclaredConstructor(String.class).newInstance("HERESMYARG");
//        val newInstance = clsRef.newInstance().asInstanceOf[Object]
//        JVMVarObject(newInstance)
        None
    }

  }

  private[jvm] def putField(sf: StackFrame, index: Int, objectRef: JVMObjectRef, params: ExecuteParams): Unit = {
    val cf = sf.cf
    //  java/io/PrintStream
    val fieldRef = cf.getConstant(index).asInstanceOf[ConstantFieldref]
    val nameAndType = cf.getConstant(fieldRef.nameAndTypeIndex).asInstanceOf[ConstantNameAndType]

    val name = cf.getString(nameAndType.nameIndex)
    // Ljava/lang/String;
    val descriptor = cf.getString(nameAndType.descriptorIndex)

    val fieldType = JVMMethodDescriptors.fieldDescriptorToTypes(descriptor)

    val value = sf.stack.pop()

    objectRef match {
      case v: JVMVarObjectRefManaged =>
        v.klass.putField(name, fieldType, value)

      case v: JVMVarObjectRefUnmanaged =>
        JVM.err(sf, "cannot putfield on non-klas yet")
    }
  }
}
