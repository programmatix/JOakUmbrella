package compiling

import java.io.ByteArrayOutputStream
import java.nio.charset.Charset

import compiling.JVMClassFileTypes.JVMClassFileBuilderUtils
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

  def make(oc: JVMOpCode) = generic(oc, Array())
  def make(oc: JVMOpCode, args: Int*) = generic(oc, args.toArray)

  case class generic(oc: JVMOpCode, args: Array[Int] = Array()) extends ByteCode {
    assert (oc.args.length == args.length)

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
        argDef.lengthBytes match {
          case 1 => JVMClassFileBuilderUtils.writeByte(out, argActual)
          case 2 => JVMClassFileBuilderUtils.writeShort(out, argActual)
          case 4 => JVMClassFileBuilderUtils.writeInt(out, argActual)
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
      case _ => throw unsupported("unhandled type")
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

  case class JVMTypeVoid() extends JVMType
  case class JVMTypeBoolean() extends JVMType
  case class JVMTypeInt() extends JVMType
  case class JVMTypeShort() extends JVMType
  case class JVMTypeChar() extends JVMType
  case class JVMTypeByte() extends JVMType
  case class JVMTypeFloat() extends JVMType
  case class JVMTypeDouble() extends JVMType
  case class JVMTypeLong() extends JVMType

  // String is not a primitive type but needed to create a valid main
  // Why don't we use char* -> String?  Because it makes the assumption the c code won't do fancy array/pointer stuff with it
  case class JVMTypeString() extends JVMType

  case class JVMTypeArray(typ: JVMType) extends JVMType

//  case class JVMTypeShortArray() extends JVMType
//  case class JVMTypeBooleanArray() extends JVMType
//  case class JVMTypeIntArray() extends JVMType
//  case class JVMTypeCharArray() extends JVMType
//  case class JVMTypeByteArray() extends JVMType
//  case class JVMTypeFloatArray() extends JVMType
//  case class JVMTypeDoubleArray() extends JVMType
//  case class JVMTypeLongArray() extends JVMType

  //  case class DefineFunction(name: Identifier, types: Types, passedVariables: Seq[DeclareVariable]) extends Command
  case class DeclareVariable(name: Identifier, typ: JVMType) extends Command
//  case class StoreExpressionInCurrentVar() extends Command

  case class JVMGenUnsupportedCurrently(err: String) extends RuntimeException(s"Operation is not currently supported: ${err}")
  case class JVMInterimBadState(err: String) extends RuntimeException(err)
}
