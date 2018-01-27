package jvm

import java.io.{ByteArrayInputStream, File, FileInputStream}

import jvm.JVMByteCode._
import jvm.JVMClassFileReader.ReadParams
import jvm.JVMClassFileTypes._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class ProgramCounterRegister {

}

class Thread {
  val pc = new ProgramCounterRegister()
}

class StackFrame {
  val locals = mutable.Map[Int, JVMVar]()

  def addLocal(idx: Int, value: JVMVar) = {
    locals(idx) = value
  }

  def getLocal(idx: Int): JVMVar = locals(idx)

  // Only bytes, shorts and ints can be pushed directly onto the stack.  Other types get stored as locals.
  // Nope, fconst can push a float
  val stack = mutable.Stack[JVMVar]()

  def push(value: JVMVar) = stack.push(value)

  def pop(): JVMVar = stack.pop()

  val constants = mutable.Stack[Constant]()
}

case class ExecuteParams(
                          // For testing: stops execution just before the last return
                          stopBeforeFinalReturn: Boolean = false

                        )

// A Java Virtual Machine implementation, just for learning purposes
class JVM(cf: JVMClassFileBuilderForReading, classLoader: ClassLoader = ClassLoader.getSystemClassLoader) {
  val stack = mutable.Stack[StackFrame]()


  def getMethodStuff(methodTypes: JVMMethodDescriptors.MethodDescriptor): (Seq[Object], Seq[Class[_]]) = {
    val args = ArrayBuffer.empty[Object]
    val argTypes = ArrayBuffer.empty[Class[_]]

    methodTypes.args.foreach(arg => {
      val next = stack.head.pop()
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
        //              case v @ (_: JVMTypeInt | _: JVMTypeShort | _: JVMTypeByte) => next.asInstanceOf[JVMVarInteger].asInt.asInstanceOf[Object]
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

  private def resolveMethodRef(index: Int) = {
    val fieldRef = cf.getConstant(index).asInstanceOf[ConstantMethodref]
    val cls = cf.getConstant(fieldRef.classIndex).asInstanceOf[ConstantClass]
    //  java/io/PrintStream
    val className = cf.getString(cls.nameIndex)
    val nameAndType = cf.getConstant(fieldRef.nameAndTypeIndex).asInstanceOf[ConstantNameAndType]
    // println
    val methodName = cf.getString(nameAndType.nameIndex)
    // (Ljava/lang/String;)V
    val methodDescriptor = cf.getString(nameAndType.descriptorIndex)

    val methodTypes = JVMMethodDescriptors.methodDescriptorToTypes(methodDescriptor)

    val (args, argTypes) = getMethodStuff(methodTypes)


    val clsRef = classLoader.loadClass(className.replace("/", "."))
    val methodRef = clsRef.getMethod(methodName, argTypes: _*)

    (methodRef, args)
  }

  def callFunction(code: Seq[JVMOpCodeWithArgs], parms: ExecuteParams = ExecuteParams()): Unit = {
    val sf = new StackFrame
    stack.push(sf)

    def store(index: Int): Unit = {
      val v1 = sf.stack.pop()
      sf.addLocal(index, v1)
    }

    def load(index: Int): Unit = {
      val stored = sf.getLocal(index)
      sf.push(stored)
    }

    for (op <- code) {
      //      val op = code(idx)
      op.oc.hexcode match {
        case 0x32 => // aaload
          JVM.err("Cannot handle opcode aaload yet")
        case 0x53 => // aastore
          JVM.err("Cannot handle opcode aastore yet")
        case 0x01 => // aconst_null
          JVM.err("Cannot handle opcode aconst_null yet")
        case 0x19 => // aload
          JVM.err("Cannot handle opcode aload yet")
        case 0x2a => // aload_0
          JVM.err("Cannot handle opcode aload_0 yet")
        case 0x2b => // aload_1
          JVM.err("Cannot handle opcode aload_1 yet")
        case 0x2c => // aload_2
          JVM.err("Cannot handle opcode aload_2 yet")
        case 0x2d => // aload_3
          JVM.err("Cannot handle opcode aload_3 yet")
        case 0xbd => // anewarray
          JVM.err("Cannot handle opcode anewarray yet")
        case 0xb0 => // areturn
          JVM.err("Cannot handle opcode areturn yet")
        case 0xbe => // arraylength
          JVM.err("Cannot handle opcode arraylength yet")
        case 0x3a => // astore
          JVM.err("Cannot handle opcode astore yet")
        case 0x4b => // astore_0
          JVM.err("Cannot handle opcode astore_0 yet")
        case 0x4c => // astore_1
          JVM.err("Cannot handle opcode astore_1 yet")
        case 0x4d => // astore_2
          JVM.err("Cannot handle opcode astore_2 yet")
        case 0x4e => // astore_3
          JVM.err("Cannot handle opcode astore_3 yet")
        case 0xbf => // athrow
          JVM.err("Cannot handle opcode athrow yet")
        case 0x33 => // baload
          JVM.err("Cannot handle opcode baload yet")
        case 0x54 => // bastore
          JVM.err("Cannot handle opcode bastore yet")
        case 0x10 => // bipush
          sf.stack.push(op.args.head)

        case 0xca => // breakpoint
          JVM.err("Cannot handle opcode breakpoint yet")
        case 0x34 => // caload
          JVM.err("Cannot handle opcode caload yet")
        case 0x55 => // castore
          JVM.err("Cannot handle opcode castore yet")
        case 0xc0 => // checkcast
          JVM.err("Cannot handle opcode checkcast yet")
        case 0x90 => // d2f
          JVM.err("Cannot handle opcode d2f yet")
        case 0x8e => // d2i
          JVM.err("Cannot handle opcode d2i yet")
        case 0x8f => // d2l
          JVM.err("Cannot handle opcode d2l yet")
        case 0x63 => // dadd
          JVM.err("Cannot handle opcode dadd yet")
        case 0x31 => // daload
          JVM.err("Cannot handle opcode daload yet")
        case 0x52 => // dastore
          JVM.err("Cannot handle opcode dastore yet")
        case 0x98 => // dcmpg
          JVM.err("Cannot handle opcode dcmpg yet")
        case 0x97 => // dcmpl
          JVM.err("Cannot handle opcode dcmpl yet")
        case 0x0e => // dconst_0
          JVM.err("Cannot handle opcode dconst_0 yet")
        case 0x0f => // dconst_1
          JVM.err("Cannot handle opcode dconst_1 yet")
        case 0x6f => // ddiv
          JVM.err("Cannot handle opcode ddiv yet")
        case 0x18 => // dload
          JVM.err("Cannot handle opcode dload yet")
        case 0x26 => // dload_0
          JVM.err("Cannot handle opcode dload_0 yet")
        case 0x27 => // dload_1
          JVM.err("Cannot handle opcode dload_1 yet")
        case 0x28 => // dload_2
          JVM.err("Cannot handle opcode dload_2 yet")
        case 0x29 => // dload_3
          JVM.err("Cannot handle opcode dload_3 yet")
        case 0x6b => // dmul
          JVM.err("Cannot handle opcode dmul yet")
        case 0x77 => // dneg
          JVM.err("Cannot handle opcode dneg yet")
        case 0x73 => // drem
          JVM.err("Cannot handle opcode drem yet")
        case 0xaf => // dreturn
          JVM.err("Cannot handle opcode dreturn yet")
        case 0x39 => // dstore
          JVM.err("Cannot handle opcode dstore yet")
        case 0x47 => // dstore_0
          JVM.err("Cannot handle opcode dstore_0 yet")
        case 0x48 => // dstore_1
          JVM.err("Cannot handle opcode dstore_1 yet")
        case 0x49 => // dstore_2
          JVM.err("Cannot handle opcode dstore_2 yet")
        case 0x4a => // dstore_3
          JVM.err("Cannot handle opcode dstore_3 yet")
        case 0x67 => // dsub
          JVM.err("Cannot handle opcode dsub yet")
        case 0x59 => // dup
          JVM.err("Cannot handle opcode dup yet")
        case 0x5a => // dup_x1
          JVM.err("Cannot handle opcode dup_x1 yet")
        case 0x5b => // dup_x2
          JVM.err("Cannot handle opcode dup_x2 yet")
        case 0x5c => // dup2
          JVM.err("Cannot handle opcode dup2 yet")
        case 0x5d => // dup2_x1
          JVM.err("Cannot handle opcode dup2_x1 yet")
        case 0x5e => // dup2_x2
          JVM.err("Cannot handle opcode dup2_x2 yet")
        case 0x8d => // f2d
          val v1 = sf.pop().asInstanceOf[JVMVarFloat]
          sf.push(JVMVarDouble(v1.v))

        case 0x8b => // f2i
          JVM.err("Cannot handle opcode f2i yet")
        case 0x8c => // f2l
          JVM.err("Cannot handle opcode f2l yet")
        case 0x62 => // fadd
          val v1 = sf.stack.pop().asInstanceOf[JVMVarFloat]
          val v2 = sf.stack.pop().asInstanceOf[JVMVarFloat]
          val v3 = JVMVarFloat(v1.v + v2.v)
          sf.stack.push(v3)

        case 0x30 => // faload
          JVM.err("Cannot handle opcode faload yet")
        case 0x51 => // fastore
          JVM.err("Cannot handle opcode fastore yet")
        case 0x96 => // fcmpg
          JVM.err("Cannot handle opcode fcmpg yet")
        case 0x95 => // fcmpl
          JVM.err("Cannot handle opcode fcmpl yet")

        case 0x0b => // fconst_0
          sf.stack.push(JVMVarFloat(0))

        case 0x0c => // fconst_1
          sf.stack.push(JVMVarFloat(1))

        case 0x0d => // fconst_2
          sf.stack.push(JVMVarFloat(2))

        case 0x6e => // fdiv
          JVM.err("Cannot handle opcode fdiv yet")

        case 0x17 => // fload
          val index = op.args.head.asInstanceOf[JVMVarByte].asInt
          val stored = sf.getLocal(index)
          sf.push(stored)

        case 0x22 => // fload_0
          val stored = sf.getLocal(0)
          sf.push(stored)

        case 0x23 => // fload_1
          val stored = sf.getLocal(1)
          sf.push(stored)

        case 0x24 => // fload_2
          val stored = sf.getLocal(2)
          sf.push(stored)

        case 0x25 => // fload_3
          val stored = sf.getLocal(3)
          sf.push(stored)

        case 0x6a => // fmul
          JVM.err("Cannot handle opcode fmul yet")
        case 0x76 => // fneg
          JVM.err("Cannot handle opcode fneg yet")
        case 0x72 => // frem
          JVM.err("Cannot handle opcode frem yet")
        case 0xae => // freturn
          JVM.err("Cannot handle opcode freturn yet")
        case 0x38 => // fstore
          val v1 = sf.stack.pop().asInstanceOf[JVMVarFloat]
          val index = op.args.head.asInstanceOf[JVMVarByte].asInt
          sf.addLocal(index, v1)

        case 0x43 => // fstore_0
          val v1 = sf.stack.pop()
          sf.addLocal(0, v1)

        case 0x44 => // fstore_1
          val v1 = sf.stack.pop().asInstanceOf[JVMVarFloat]
          sf.addLocal(1, v1)

        case 0x45 => // fstore_2
          val v1 = sf.stack.pop().asInstanceOf[JVMVarFloat]
          sf.addLocal(2, v1)

        case 0x46 => // fstore_3
          val v1 = sf.stack.pop().asInstanceOf[JVMVarFloat]
          sf.addLocal(3, v1)

        case 0x66 => // fsub
          JVM.err("Cannot handle opcode fsub yet")
        case 0xb4 => // getfield
          JVM.err("Cannot handle opcode getfield yet")
        case 0xb2 => // getstatic
          // getstatic pops objectref (a reference to an object) from the stack, retrieves the value of the static field
          // (also known as a class field) identified by <field-spec> from objectref, and pushes the one-word or two-word
          // value onto the operand stack.
          val index = op.args.head.asInstanceOf[JVMVarInteger].asInt
          val fieldRef = cf.getConstant(index).asInstanceOf[ConstantFieldref]
          val cls = cf.getConstant(fieldRef.classIndex).asInstanceOf[ConstantClass]
          //  java/lang/System
          val className = cf.getString(cls.nameIndex)
          val nameAndType = cf.getConstant(fieldRef.nameAndTypeIndex).asInstanceOf[ConstantNameAndType]
          // out
          val name = cf.getString(nameAndType.nameIndex)
          // Ljava/io/PrintStream;
          val typ = cf.getString(nameAndType.descriptorIndex)
          //          val field = ClassLoader.getSystemClassLoader.loadClass("java.lang.System").getField("out")
          //          ClassLoader.getSystemClassLoader.loadClass("java.lang.System").getField("out").get(classOf[java.io.PrintStream])

          val clsRef = ClassLoader.getSystemClassLoader.loadClass("java.lang.System")
          val fieldRef2 = clsRef.getField(name)
          val fieldType = fieldRef2.getType
          val fieldInstance = fieldRef2.get(fieldType)
          sf.stack.push(JVMVarObject(fieldInstance))
        case 0xa7 => // goto
          JVM.err("Cannot handle opcode goto yet")
        case 0xc8 => // goto_w
          JVM.err("Cannot handle opcode goto_w yet")
        case 0x91 => // i2b
          JVM.err("Cannot handle opcode i2b yet")
        case 0x92 => // i2c
          JVM.err("Cannot handle opcode i2c yet")
        case 0x87 => // i2d
          JVM.err("Cannot handle opcode i2d yet")
        case 0x86 => // i2f
          JVM.err("Cannot handle opcode i2f yet")
        case 0x85 => // i2l
          JVM.err("Cannot handle opcode i2l yet")
        case 0x93 => // i2s
          JVM.err("Cannot handle opcode i2s yet")
        case 0x60 => // iadd
          val v1 = sf.stack.pop().asInstanceOf[JVMVarInteger]
          val v2 = sf.stack.pop().asInstanceOf[JVMVarInteger]
          val v3 = JVMVarInt(v1.asInt + v2.asInt)
          sf.stack.push(v3)

        case 0x2e => // iaload
          JVM.err("Cannot handle opcode iaload yet")
        case 0x7e => // iand
          JVM.err("Cannot handle opcode iand yet")
        case 0x4f => // iastore
          JVM.err("Cannot handle opcode iastore yet")
        case 0x02 => // iconst_m1
          JVM.err("Cannot handle opcode iconst_m1 yet")
        case 0x03 => // iconst_0
          sf.stack.push(JVMVarInt(0))

        case 0x04 => // iconst_1
          sf.stack.push(JVMVarInt(1))

        case 0x05 => // iconst_2
          sf.stack.push(JVMVarInt(2))

        case 0x06 => // iconst_3
          sf.stack.push(JVMVarInt(3))

        case 0x07 => // iconst_4
          sf.stack.push(JVMVarInt(4))

        case 0x08 => // iconst_5
          sf.stack.push(JVMVarInt(5))

        case 0x6c => // idiv
          JVM.err("Cannot handle opcode idiv yet")
        case 0xa5 => // if_acmpeq
          JVM.err("Cannot handle opcode if_acmpeq yet")
        case 0xa6 => // if_acmpne
          JVM.err("Cannot handle opcode if_acmpne yet")
        case 0x9f => // if_icmpeq
          JVM.err("Cannot handle opcode if_icmpeq yet")
        case 0xa2 => // if_icmpge
          JVM.err("Cannot handle opcode if_icmpge yet")
        case 0xa3 => // if_icmpgt
          JVM.err("Cannot handle opcode if_icmpgt yet")
        case 0xa4 => // if_icmple
          JVM.err("Cannot handle opcode if_icmple yet")
        case 0xa1 => // if_icmplt
          JVM.err("Cannot handle opcode if_icmplt yet")
        case 0xa0 => // if_icmpne
          JVM.err("Cannot handle opcode if_icmpne yet")
        case 0x99 => // ifeq
          JVM.err("Cannot handle opcode ifeq yet")
        case 0x9c => // ifge
          JVM.err("Cannot handle opcode ifge yet")
        case 0x9d => // ifgt
          JVM.err("Cannot handle opcode ifgt yet")
        case 0x9e => // ifle
          JVM.err("Cannot handle opcode ifle yet")
        case 0x9b => // iflt
          JVM.err("Cannot handle opcode iflt yet")
        case 0x9a => // ifne
          JVM.err("Cannot handle opcode ifne yet")
        case 0xc7 => // ifnonnull
          JVM.err("Cannot handle opcode ifnonnull yet")
        case 0xc6 => // ifnull
          JVM.err("Cannot handle opcode ifnull yet")
        case 0x84 => // iinc
          JVM.err("Cannot handle opcode iinc yet")
        case 0x15 => // iload
          val index = op.args.head.asInstanceOf[JVMVarByte].asInt
          load(index)

        case 0x1a => // iload_0
          load(0)

        case 0x1b => // iload_1
          load(1)

        case 0x1c => // iload_2
          load(2)

        case 0x1d => // iload_3
          load(3)

        case 0xfe => // impdep1
          JVM.err("Cannot handle opcode impdep1 yet")
        case 0xff => // impdep2
          JVM.err("Cannot handle opcode impdep2 yet")
        case 0x68 => // imul
          JVM.err("Cannot handle opcode imul yet")
        case 0x74 => // ineg
          JVM.err("Cannot handle opcode ineg yet")
        case 0xc1 => // instanceof
          JVM.err("Cannot handle opcode instanceof yet")
        case 0xba => // invokedynamic
          JVM.err("Cannot handle opcode invokedynamic yet")
        case 0xb9 => // invokeinterface
          JVM.err("Cannot handle opcode invokeinterface yet")
        case 0xb7 => // invokespecial
          JVM.err("Cannot handle opcode invokespecial yet")
        case 0xb8 => // invokestatic
          val index = op.args.head.asInstanceOf[JVMVarInteger].asInt

          val (methodRef, args) = resolveMethodRef(index)

          if (args.length == 0) {
            methodRef.invoke(null)
          }
          else {
            methodRef.invoke(null, args)
          }

        case 0xb6 => // invokevirtual
          // invoke virtual method on object objectref and puts the result on the stack (might be void); the method is
          // identified by method reference index in constant pool (indexbyte1 << 8 + indexbyte2)
          val index = op.args.head.asInstanceOf[JVMVarInteger].asInt

          val (methodRef, args) = resolveMethodRef(index)
          val objectRef = stack.head.pop().asInstanceOf[JVMVarObject].o

          methodRef.invoke(objectRef, args: _*) // :_* is the hint to expand the Seq to varargs

        case 0x80 => // ior
          JVM.err("Cannot handle opcode ior yet")
        case 0x70 => // irem
          JVM.err("Cannot handle opcode irem yet")
        case 0xac => // ireturn
          if (!parms.stopBeforeFinalReturn) {
            stack.pop()
          }

        case 0x78 => // ishl
          JVM.err("Cannot handle opcode ishl yet")
        case 0x7a => // ishr
          JVM.err("Cannot handle opcode ishr yet")
        case 0x36 => // istore
          val v1 = sf.stack.pop()
          val index = op.args.head.asInstanceOf[JVMVarInteger].asInt
          sf.addLocal(index, v1)

        case 0x3b => // istore_0
          store(0)

        case 0x3c => // istore_1
          store(1)

        case 0x3d => // istore_2
          store(2)

        case 0x3e => // istore_3
          store(3)

        case 0x64 => // isub
          JVM.err("Cannot handle opcode isub yet")
        case 0x7c => // iushr
          JVM.err("Cannot handle opcode iushr yet")
        case 0x82 => // ixor
          JVM.err("Cannot handle opcode ixor yet")
        case 0xa8 => // jsr
          JVM.err("Cannot handle opcode jsr yet")
        case 0xc9 => // jsr_w
          JVM.err("Cannot handle opcode jsr_w yet")
        case 0x8a => // l2d
          JVM.err("Cannot handle opcode l2d yet")
        case 0x89 => // l2f
          JVM.err("Cannot handle opcode l2f yet")
        case 0x88 => // l2i
          JVM.err("Cannot handle opcode l2i yet")
        case 0x61 => // ladd
          JVM.err("Cannot handle opcode ladd yet")
        case 0x2f => // laload
          JVM.err("Cannot handle opcode laload yet")
        case 0x7f => // land
          JVM.err("Cannot handle opcode land yet")
        case 0x50 => // lastore
          JVM.err("Cannot handle opcode lastore yet")
        case 0x94 => // lcmp
          JVM.err("Cannot handle opcode lcmp yet")
        case 0x09 => // lconst_0
          JVM.err("Cannot handle opcode lconst_0 yet")
        case 0x0a => // lconst_1
          JVM.err("Cannot handle opcode lconst_1 yet")
        case 0x12 => // ldc
          val index = op.args.head.asInstanceOf[JVMVarInteger].asInt
          val c = cf.getConstant(index)
          c match {
            case v: ConstantFloat   => sf.push(JVMVarFloat(v.value))
            case v: ConstantInteger => sf.push(JVMVarInt(v.value))
            case v: ConstantString  =>
              val str = cf.getString(v.stringIndex)
              sf.push(JVMVarString(str))
            case _                  => JVM.err(s"Can't handle constant ${c} in ldc yet")
          }

        case 0x13 => // ldc_w
          JVM.err("Cannot handle opcode ldc_w yet")
        case 0x14 => // ldc2_w
          JVM.err("Cannot handle opcode ldc2_w yet")
        case 0x6d => // ldiv
          JVM.err("Cannot handle opcode ldiv yet")
        case 0x16 => // lload
          JVM.err("Cannot handle opcode lload yet")
        case 0x1e => // lload_0
          JVM.err("Cannot handle opcode lload_0 yet")
        case 0x1f => // lload_1
          JVM.err("Cannot handle opcode lload_1 yet")
        case 0x20 => // lload_2
          JVM.err("Cannot handle opcode lload_2 yet")
        case 0x21 => // lload_3
          JVM.err("Cannot handle opcode lload_3 yet")
        case 0x69 => // lmul
          JVM.err("Cannot handle opcode lmul yet")
        case 0x75 => // lneg
          JVM.err("Cannot handle opcode lneg yet")
        case 0xab => // lookupswitch
          JVM.err("Cannot handle opcode lookupswitch yet")
        case 0x81 => // lor
          JVM.err("Cannot handle opcode lor yet")
        case 0x71 => // lrem
          JVM.err("Cannot handle opcode lrem yet")
        case 0xad => // lreturn
          JVM.err("Cannot handle opcode lreturn yet")
        case 0x79 => // lshl
          JVM.err("Cannot handle opcode lshl yet")
        case 0x7b => // lshr
          JVM.err("Cannot handle opcode lshr yet")
        case 0x37 => // lstore
          JVM.err("Cannot handle opcode lstore yet")
        case 0x3f => // lstore_0
          JVM.err("Cannot handle opcode lstore_0 yet")
        case 0x40 => // lstore_1
          JVM.err("Cannot handle opcode lstore_1 yet")
        case 0x41 => // lstore_2
          JVM.err("Cannot handle opcode lstore_2 yet")
        case 0x42 => // lstore_3
          JVM.err("Cannot handle opcode lstore_3 yet")
        case 0x65 => // lsub
          JVM.err("Cannot handle opcode lsub yet")
        case 0x7d => // lushr
          JVM.err("Cannot handle opcode lushr yet")
        case 0x83 => // lxor
          JVM.err("Cannot handle opcode lxor yet")
        case 0xc2 => // monitorenter
          JVM.err("Cannot handle opcode monitorenter yet")
        case 0xc3 => // monitorexit
          JVM.err("Cannot handle opcode monitorexit yet")
        case 0xc5 => // multianewarray
          JVM.err("Cannot handle opcode multianewarray yet")
        case 0xbb => // new
          JVM.err("Cannot handle opcode new yet")
        case 0xbc => // newarray
          JVM.err("Cannot handle opcode newarray yet")
        case 0x00 => // nop
          JVM.err("Cannot handle opcode nop yet")
        case 0x57 => // pop
          JVM.err("Cannot handle opcode pop yet")
        case 0x58 => // pop2
          JVM.err("Cannot handle opcode pop2 yet")
        case 0xb5 => // putfield
          JVM.err("Cannot handle opcode putfield yet")
        case 0xb3 => // putstatic
          JVM.err("Cannot handle opcode putstatic yet")
        case 0xa9 => // ret
          JVM.err("Cannot handle opcode ret yet")
        case 0xb1 => // return
          if (!parms.stopBeforeFinalReturn) {
            stack.pop()
          }
        case 0x35 => // saload
          JVM.err("Cannot handle opcode saload yet")
        case 0x56 => // sastore
          JVM.err("Cannot handle opcode sastore yet")
        case 0x11 => // sipush
          JVM.err("Cannot handle opcode sipush yet")
        case 0x5f => // swap
          JVM.err("Cannot handle opcode swap yet")
        case 0xaa => // tableswitch
          JVM.err("Cannot handle opcode tableswitch yet")

        case _ =>
          JVM.err(s"Cannot yet handle opcode ${op}")
      }
    }
  }

  def execute(code: Seq[JVMOpCodeWithArgs], parms: ExecuteParams = ExecuteParams()): Unit = {
    callFunction(code, parms)
  }
}

object JVM {
  def err(message: String): Unit = {
    println("Error: " + message)
    assert(false)
  }


  def main(args: Array[String]): Unit = {
    if (args.length != 1) {
      println("usage: program <.class file>")
    }
    else {
      val file = new File(args(0) + ".class")
      val fileContent = new Array[Byte](file.length.asInstanceOf[Int])
      new FileInputStream(file).read(fileContent)
      val lines = new ByteArrayInputStream(fileContent)

      JVMClassFileReader.read(lines, ReadParams()) match {
        case Some(classFile) =>
          classFile.getMainMethod() match {
            case Some(main) =>
              val classLoader = ClassLoader.getSystemClassLoader

              val jvm = new JVM(classFile, classLoader)

              val mainCode = main.getCode().codeOrig
              jvm.execute(mainCode)

            case _ =>
              err("No main method found")
          }
        case _               => err("Could not read input class file")
      }
    }
  }
}
