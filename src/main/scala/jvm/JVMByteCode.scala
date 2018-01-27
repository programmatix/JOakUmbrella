package jvm

import java.io.ByteArrayOutputStream
import java.nio.charset.Charset

import compiling.JVMOpCode
import jvm.JVMClassFileTypes.JVMClassFileBuilderUtils
import parsing._

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

  // push a short onto the stack as an integer value
  //  case class sipush(v: Short) extends ByteCode {
  //    override def gen(implicit params: GenParams): String = s"sipush $v"
  //    override def write(out: ByteArrayOutputStream, charset: Charset, genParams: GenParams): Int = {
  //      JVMClassFileBuilderUtils.writeByte(out, 0x11)
  //      JVMClassFileBuilderUtils.writeByte(out, v)
  //      2
  //    }
  //  }

  // push a byte onto the stack as an integer value
  //  case class bipush(v: Short) extends ByteCode {
  //    assert (v < 256)
  //
  //    override def gen(implicit params: GenParams): String = s"bipush $v"
  //    override def write(out: ByteArrayOutputStream, charset: Charset, genParams: GenParams): Int = {
  //      JVMClassFileBuilderUtils.writeByte(out, 0x10)
  //      JVMClassFileBuilderUtils.writeByte(out, v)
  //      2
  //    }
  //  }
  //
  //  // return an integer from a method
  //  case class ireturn() extends ByteCode {
  //    override def gen(implicit params: GenParams): String = "ireturn"
  //    override def write(out: ByteArrayOutputStream, charset: Charset, genParams: GenParams): Int = {
  //      JVMClassFileBuilderUtils.writeByte(out, 0xac)
  //      1
  //    }
  //  }

  // store int value into variable #index
  //  case class istore(variable: Int) extends ByteCode {
  //    override def gen(implicit params: GenParams): String = {
  //      variable match {
  //        case 0|1|2|3 => s"istore_$variable"
  //        case _ => s"istore $variable"
  //      }
  //    }
  //    override def write(out: ByteArrayOutputStream, charset: Charset, genParams: GenParams): Int = {
  //      variable match {
  //        case 0 =>
  //          JVMClassFileBuilderUtils.writeByte(out, 0x3b)
  //          1
  //        case 1 =>
  //          JVMClassFileBuilderUtils.writeByte(out, 0x3c)
  //          1
  //        case 2 =>
  //          JVMClassFileBuilderUtils.writeByte(out, 0x3d)
  //          1
  //        case 3 =>
  //          JVMClassFileBuilderUtils.writeByte(out, 0x3e)
  //          1
  //        case _ =>
  //          JVMClassFileBuilderUtils.writeByte(out, 0x36)
  //          JVMClassFileBuilderUtils.writeByte(out, variable)
  //          2
  //      }
  //
  //    }
  //  }

  // multiply two integers
  //  case class imul() extends ByteCode {
  //    override def gen(implicit params: GenParams): String = "imul"
  //    override def write(out: ByteArrayOutputStream, charset: Charset, genParams: GenParams): Int = {
  //      JVMClassFileBuilderUtils.writeByte(out, 0x68)
  //      1
  //    }
  //  }
  //
  //  // add two integers
  //  case class iadd() extends ByteCode {
  //    override def gen(implicit params: GenParams): String = "iadd"
  //    override def write(out: ByteArrayOutputStream, charset: Charset, genParams: GenParams): Int = {
  //      JVMClassFileBuilderUtils.writeByte(out, 0x60)
  //      1
  //    }
  //  }
  //
  //  // return void from method
  //  case class ret() extends ByteCode {
  //    override def gen(implicit params: GenParams): String = "return"
  //    override def write(out: ByteArrayOutputStream, charset: Charset, genParams: GenParams): Int = {
  //      JVMClassFileBuilderUtils.writeByte(out, 0xa9)
  //      1
  //    }
  //  }

  def make(oc: JVMOpCode) = JVMOpCodeWithArgs(oc, Array())


  def makeFloat(oc: JVMOpCode, args: Float*) = JVMOpCodeWithArgs(oc, args.map(v => JVMVarFloat(v)).toArray)


  // JVM bytes and shorts introduce signed oddness so keep stuff as ints
  //  def makeByte(oc: JVMOpCode, args: Byte*) = JVMOpCodeWithArgs(oc, args.map(v => JVMVarByte(v)).toArray)
//  def makeShort(oc: JVMOpCode, args: Short*) = JVMOpCodeWithArgs(oc, args.map(v => JVMVarShort(v)).toArray)

  def makeInt(oc: JVMOpCode, args: Int*) = JVMOpCodeWithArgs(oc, args.map(v => JVMVarInt(v)).toArray)

  def makeDouble(oc: JVMOpCode, args: Double*) = JVMOpCodeWithArgs(oc, args.map(v => JVMVarDouble(v)).toArray)



  case class JVMOpCodeWithArgs(oc: JVMOpCode, args: Array[JVMVar] = Array()) extends ByteCode {
    assert(oc.args.length == args.length)

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

  //  case class label(symbol: String) extends ByteCode {
  //    override def gen(implicit params: GenParams): String = symbol + ":"
  //    override def write(out: ByteArrayOutputStream, charset: Charset, genParams: GenParams): Int = {
  //      assert(false) // ???
  //      0
  //    }
  //  }

  //  case class movl(constant: String, register: String) extends ByteCode {
  //    override def gen(implicit params: GenParams): String = s"movl $constant,%$register"
  //  }

  // 3 -> -3
  //  case class neg(register: String) extends ByteCode {
  //    override def gen(implicit params: GenParams): String = s"neg %$register"
  //    override def write(out: ByteArrayOutputStream, charset: Charset, genParams: GenParams): Int = {
  //      JVMClassFileBuilderUtils.writeByte(out, 0xac)
  //    }
  //  }

  def unsupported(err: String) = JVMGenUnsupportedCurrently(err)

  private def makeTypes(in: Types): String = {
    val mapped = in.types.map {
      case v: TypeSpecifierVoid     => "void"
      case v: TypeSpecifierChar     => "char"
      case v: TypeSpecifierShort    => "short"
      case v: TypeSpecifierInt      => "int"
      case v: TypeSpecifierLong     => "long"
      case v: TypeSpecifierFloat    => "float"
      case v: TypeSpecifierDouble   => "double"
      case v: TypeSpecifierSigned   => "signed"
      case v: TypeSpecifierUnsigned => "unsigned"
      case v: TypeSpecifierBool     => "boolean"
      case v: TypeSpecifierComplex  => throw unsupported("_Complex type")
      case _                        => throw unsupported("unhandled type")
    }
    mapped.mkString(" ")
  }

  //  private def makeIdentifier(in: Identifier): String = in.v
  //
  //  private def makePassedVariables(in: Seq[DeclareVariable]): String = {
  //    ""
  //  }

  //  sealed trait ByteCodeComplex
  //
  //  // public static void main(java.lang.String[]);
  //  // Code:
  //  case class Function(in: DefineFunction) extends ByteCodeComplex {
  //    override def gen(implicit params: GenParams): String = {
  //      s"public static ${makeTypes(in.types)} ${makeIdentifier(in.name)}(${makePassedVariables(in.passedVariables)});\nCode:"
  //    }
  //
  //  }


  // Most of the time this will just be one type like "int"
  case class Types(types: Seq[TypeSpecifier])

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

  case class JVMVarShort(v: Short) extends JVMVar with JVMVarInteger {
    def asInt: Int = v
  }

  case class JVMVarChar(v: Char) extends JVMVar

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

  //  case class DefineFunction(name: Identifier, types: Types, passedVariables: Seq[DeclareVariable]) extends Command
  case class DeclareVariable(name: Identifier, typ: JVMType) extends Command

  //  case class StoreExpressionInCurrentVar() extends Command

  case class JVMGenUnsupportedCurrently(err: String) extends RuntimeException(s"Operation is not currently supported: ${err}")

  case class JVMInterimBadState(err: String) extends RuntimeException(err)

}
