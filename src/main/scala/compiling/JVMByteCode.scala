package compiling

import parsing._

object JVMByteCode {
  case class GenParams()

  // Output by CGenerator, these represent commands
  sealed trait Generated {
    def gen(implicit genParams: GenParams): String = toString
  }
  sealed trait ByteCode extends Generated
  sealed trait Command extends Generated

  @Deprecated
  case class GenStrings(v: Seq[String]) extends Generated {
    override def toString = v.mkString(" ")
  }
  @Deprecated
  case class GenString(v: String) extends Generated {
    override def toString = v
  }

  // Increase the indent, but don't print anything
//  @Deprecated
//  case class IndentUp() extends Generated

//  @Deprecated
//  case class IndentDown() extends Generated

  // Print a newline only
//  @Deprecated
//  case class NewlineOnly() extends Generated

  // Print a newline, then the current indent
//  @Deprecated
//  case class NewlineAndIndent() extends Generated

  // Print a newline, then increase indent, then print the current indent
//  @Deprecated
//  case class NewlineAndIndentUp() extends Generated

  // Print a newline, then decrease indent, then print the current indent
//  @Deprecated
//  case class NewlineAndIndentDown() extends Generated

  // push a short onto the stack as an integer value
  case class sipush(v: Short) extends ByteCode {
    override def gen(implicit params: GenParams): String = s"sipush $v"
  }

  // push a byte onto the stack as an integer value
  case class bipush(v: Byte) extends ByteCode {
    override def gen(implicit params: GenParams): String = s"bipush $v"
  }

  // return an integer from a method
  case class ireturn() extends ByteCode {
    override def gen(implicit params: GenParams): String = "ireturn"
  }

  // store int value into variable #index
  case class istore(variable: Int) extends ByteCode {
    override def gen(implicit params: GenParams): String = {
      variable match {
        case 0|1|2|3 => s"istore_$variable"
        case _ => s"istore $variable"
      }
    }
  }

  // multiply two integers
  case class imul() extends ByteCode {
    override def gen(implicit params: GenParams): String = "imul"
  }

  // add two integers
  case class iadd() extends ByteCode {
    override def gen(implicit params: GenParams): String = "iadd"
  }

  // return void from method
  case class ret() extends ByteCode {
    override def gen(implicit params: GenParams): String = "return"
  }

  case class label(symbol: String) extends ByteCode {
    override def gen(implicit params: GenParams): String = symbol + ":"
  }

  case class movl(constant: String, register: String) extends ByteCode {
    override def gen(implicit params: GenParams): String = s"movl $constant,%$register"
  }

  // 3 -> -3
  case class neg(register: String) extends ByteCode {
    override def gen(implicit params: GenParams): String = s"neg %$register"
  }

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

  private def makeIdentifier(in: Identifier): String = in.v

  private def makePassedVariables(in: Seq[DeclareVariable]): String = {
    ""
  }

  // public static void main(java.lang.String[]);
  // Code:
  case class Function(in: DefineFunction) extends ByteCode {
    override def gen(implicit params: GenParams): String = {
      s"public static ${makeTypes(in.types)} ${makeIdentifier(in.name)}(${makePassedVariables(in.passedVariables)});\nCode:"
    }

  }


  // Most of the time this will just be one type like "int"
  case class Types(types: Seq[TypeSpecifier])
  case class DefineFunction(name: Identifier, types: Types, passedVariables: Seq[DeclareVariable]) extends Command
  case class DeclareVariable(name: Identifier, types: Types) extends Command
  case class StoreExpressionInCurrentVar() extends Command

  case class JVMGenUnsupportedCurrently(err: String) extends RuntimeException(s"Operation is not currently supported: ${err}")
  case class JVMInterimBadState(err: String) extends RuntimeException(err)
}
