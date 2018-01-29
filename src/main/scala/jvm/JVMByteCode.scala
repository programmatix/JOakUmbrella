package jvm

import java.io.ByteArrayOutputStream
import java.nio.charset.Charset

import compiling.JVMOpCode
import jvm.JVMClassFileTypes.JVMClassFileBuilderUtils

object JVMByteCode {

  case class GenParams()

  // Output by CGenerator, these represent commands
  sealed trait Generated {
    // Returns bytes written
    def write(out: ByteArrayOutputStream, charset: Charset, genParams: GenParams): Int = {
      assert(false)
      0
    }

    def gen(implicit genParams: GenParams): String = toString
  }

  sealed trait ByteCode extends Generated

  sealed trait Command extends Generated

  @Deprecated
  case class GenStrings(v: Seq[String]) extends Generated {
    override def toString = v.mkString(" ")

    override def write(out: ByteArrayOutputStream, charset: Charset, genParams: GenParams): Int = 0
  }

  @Deprecated
  case class GenString(v: String) extends Generated {
    override def toString = v

    override def write(out: ByteArrayOutputStream, charset: Charset, genParams: GenParams): Int = 0
  }


  def make(oc: JVMOpCode) = JVMOpCodeWithArgs(oc, Array())


  def makeFloat(oc: JVMOpCode, args: Float*) = JVMOpCodeWithArgs(oc, args.map(v => JVMVarFloat(v)).toArray)

  def makeInt(oc: JVMOpCode, args: Int*) = JVMOpCodeWithArgs(oc, args.map(v => JVMVarInt(v)).toArray)

  def makeDouble(oc: JVMOpCode, args: Double*) = JVMOpCodeWithArgs(oc, args.map(v => JVMVarDouble(v)).toArray)



  case class JVMOpCodeWithArgs(oc: JVMOpCode, args: Array[JVMVar] = Array()) extends ByteCode {
    assert(oc.args.length == args.length, s"For opcode ${oc.name} expected args ${oc.args.mkString(",")} but got ${args.mkString(",")}")

    override def toString(): String = gen(GenParams()).trim()

    override def gen(implicit params: GenParams): String = {
      oc.name + " " + (
        args.map(arg => {
          //          val argDef = oc.args(idx)
          //          val argActual = args(idx)
          arg.toString
        }).mkString(" ")
        )
    }

    override def write(out: ByteArrayOutputStream, charset: Charset, genParams: GenParams): Int = {
      JVMClassFileBuilderUtils.writeByte(out, oc.hexcode)
      for (idx <- args.indices) {
        val argDef = oc.args(idx)
        val argActual = args(idx)
        val v = argActual.asInstanceOf[JVMVarInteger].asInt
        argDef.lengthBytes match {
          case 1 => JVMClassFileBuilderUtils.writeByte(out, v)
          case 2 => JVMClassFileBuilderUtils.writeShort(out, v)
          case 4 => JVMClassFileBuilderUtils.writeInt(out, v)
        }
      }
      oc.lengthInBytes
    }
  }

  def unsupported(err: String) = JVMGenUnsupportedCurrently(err)

  sealed trait JVMType
  sealed trait JVMTypePrimitive extends JVMType
  sealed trait JVMTypeInternal extends JVMType

  case class JVMTypeVoid() extends JVMTypePrimitive

  case class JVMTypeBoolean() extends JVMTypePrimitive

  // 32-bit signed two's-complement integers. -2147483648 to 2147483647
  case class JVMTypeInt() extends JVMTypePrimitive

  // 16-bit signed two's-complement integers. -32768 to 32767
  case class JVMTypeShort() extends JVMTypePrimitive

  // 16-bit unsigned integers representing Unicode characters. 0 to 65535
  case class JVMTypeChar() extends JVMTypePrimitive

  // 8-bit signed two's-complement integers. -128 to 127
  case class JVMTypeByte() extends JVMTypePrimitive

  case class JVMTypeFloat() extends JVMTypePrimitive

  case class JVMTypeDouble() extends JVMTypePrimitive

  // 64-bit signed two's-complement integers. -9223372036854775808 to 9223372036854775807
  case class JVMTypeLong() extends JVMTypePrimitive

  // String is not a primitive type but needed to create a valid main
  // Why don't we use char* -> String?  Because it makes the assumption the c code won't do fancy array/pointer stuff with it
  case class JVMTypeString() extends JVMType

  case class JVMTypeArray(typ: JVMType) extends JVMType

  // Only used in the JVM
  // longs and doubles are meant to take up two variables on the stack
  case class JVMTypeDummy() extends JVMTypeInternal
  // java/lang/String
  case class JVMTypeObjectStr(clsRaw: String) extends JVMTypeInternal
  case class JVMTypeObjectRef(obj: Object) extends JVMTypeInternal

//  case class JVMTypeReturnAddress() extends JVMTypeInternal
//  case class JVMTypeReference() extends JVMTypeInternal



  sealed trait JVMVar
  sealed trait JVMVarInteger {
    def asInt: Int
  }

  case class JVMVarBoolean(v: Boolean) extends JVMVar

  case class JVMVarInt(v: Int) extends JVMVar with JVMVarInteger {
    def asInt: Int = v
  }

  @Deprecated // currently using JVMVarInt to avoid weirdness
  case class JVMVarShort(v: Short) extends JVMVar with JVMVarInteger {
    def asInt: Int = v
  }

  case class JVMVarChar(v: Char) extends JVMVar

  @Deprecated // currently using JVMVarInt to avoid weirdness
  case class JVMVarByte(v: Byte) extends JVMVar with JVMVarInteger {
    def asInt: Int = v
  }

  case class JVMVarFloat(v: Float) extends JVMVar

  case class JVMVarDouble(v: Double) extends JVMVar

  case class JVMVarLong(v: Long) extends JVMVar

  case class JVMVarString(v: String) extends JVMVar

  // Only used in the JVM itself
//  case class JVMVarField(field: Field) extends JVMVar
  case class JVMVarObject(o: Object) extends JVMVar

  sealed trait JVMObjectRef extends JVMVar

  case class JVMVarObjectRefUnmanaged(o: Object) extends JVMObjectRef
  case class JVMVarObjectRefManaged(klass: JVMClassInstance) extends JVMObjectRef

  //  case class DefineFunction(name: Identifier, types: Types, passedVariables: Seq[DeclareVariable]) extends Command
//  case class DeclareVariable(name: Identifier, typ: JVMType) extends Command

  case class DeclareVariable(name: String, typ: JVMType)

  //  case class StoreExpressionInCurrentVar() extends Command

  case class JVMGenUnsupportedCurrently(err: String) extends RuntimeException(s"Operation is not currently supported: ${err}")

  case class JVMInterimBadState(err: String) extends RuntimeException(err)

}
