package generating

import parsing._
import parsing.{BlockItem, TranslationUnit}

import scala.collection.mutable.ArrayBuffer



class CGenerator  {
  private val nll = "\n"
  private val nl = Seq[String](nll)
  private val el = Seq(";", nll)
  //  private val wsl = " "
  //  private val ws = Seq[String](wsl)
  //  private val indentl = " "
  //  private val indent = Seq[String](indentl)

  def generate(in: BlockItem): Seq[String] = {
    in match {
      case v: Statement   => generate(v)
      case v: Declaration => generate(v) ++ el
    }
  }

  def generate(in: Seq[BlockItem]): Seq[String] = in.flatMap(v => generate(v))

  def generate(in: TranslationUnit): Seq[String] = in.v.flatMap(v => generate(v))

  def generate(in: DeclarationSpecifiers): Seq[String] = in.v.flatMap(v => generate(v))

  def generate(in: DeclarationList): Seq[String] = in.v.flatMap(v => generate(v))

  //  def generate(in: Seq[InitDeclarator]): Seq[String] = in.flatMap(v => generate(v))

  def generate(in: ParameterDeclaration): Seq[String] = {
    in match {
      case v: ParameterDeclarationDeclarator => generate(v.v) ++ generate(v.v2)
    }
  }

  def generate(in: ParameterTypeList): Seq[String] = {
    val out = ArrayBuffer.empty[String]
    out ++= in.v.flatMap(v => generate(v))
    if (in.ellipses) out += ", ..."
    out
  }

  def generate(in: DDBracketed): Seq[String] = {
    generate(in.declarator)
  }

  def generate(in: DirectDeclarator): Seq[String] = {
    in match {
      case v: DDBracketed          => generate(v)
      case v: DirectDeclaratorOnly => generate(v.v)
      case v: FunctionDeclaration  => ((generate(v.name) :+ "(") ++ generate(v.params) :+ ")") ++ nl
    }
  }

  def generate(in: Statement): Seq[String] = {
    in match {
      case v: Break               => Seq("break;")
      case v: Continue            => Seq("continue;")
      case v: Goto                => ("goto " +: generate(v.v)) ++ Seq(";", nll)
      case v: IterationWhile      => Seq("while", "(") ++ generate(v.v1) ++ Seq(")", "{", nll) ++ generate(v.v2) ++ Seq("}", nll)
      case v: LabelledLabel       => (generate(v.v1) :+ ": ") ++ generate(v.v2)
      case v: LabelledCase        => ("case " +: generate(v.v1) :+ ": ") ++ generate(v.v2)
      case v: CompoundStatement   => v.v.flatMap(generate)
      case v: ExpressionStatement => generate(v.expression) ++ el
      case v: Return              =>
        v.v match {
          case Some(exp) => Seq("return ") ++ generate(exp) ++ el
          case _         => Seq("return;")
        }
      case v: LabelledDefault     => Seq("default: ", nll) ++ generate(v.v2) ++ nl
    }
  }

  def generate(dec: Declarator): Seq[String] = {
    val out = ArrayBuffer.empty[String]
    dec.pointer.map(out += _)
    out ++= generate(dec.v)
    out
  }

  def generate(in: Initializer): Seq[String] = {
    in match {
      case v: InitializerSimple => generate(v.exp)
      case v: InitializerList   => generate(v.exp)
    }
  }

  def generate(in: InitDeclarator): Seq[String] = {
    in match {
      case v: DeclaratorEmpty    => generate(v.declarator)
      case v: DeclaratorWithInit => generate(v.declarator) ++ Seq("=") ++ generate(v.init)
    }
  }

  def generate(in: Declaration): Seq[String] = {
    val out = ArrayBuffer.empty[String]
    in match {
      case v: SimpleDeclaration       =>
        out ++= generate(v.spec)
        v.init.map(x => out ++= x.flatMap(generate))
      case v: StaticAssertDeclaration =>
        out ++= Seq("_Static_assert", "(") ++ generate(v.v1) ++ Seq(",") ++ generate(v.v2) ++ Seq(")") ++ el
    }
    out
  }

  def generate(in: Identifier): Seq[String] = {
    Seq(in.v)
  }

  def generate(in: Option[DeclarationList]): Seq[String] = {
    val out = ArrayBuffer.empty[String]
    in.map(v => out ++= generate(v))
    out
  }

  def generate(in: ExternalDeclaration): Seq[String] = {
    in match {
      case v: FunctionDefinition =>
        generate(v.spec) ++ generate(v.dec) ++ generate(v.decs) ++ Seq("{", nll) ++ generate(v.v) ++ Seq("}", nll)
    }
  }

  def generate(in: DeclarationSpecifier): Seq[String] = {
    in match {
      case v: StorageClassSpecifier => Seq(v.v)
      case v: TypeSpecifier         => Seq(v.v)
      case v: TypeQualifier         => Seq(v.v)
      case v: FunctionSpecifier     => Seq(v.v)
      case v: AlignmentSpecifier    => Seq(v.v)
    }
  }


  def generate(exp: Expression): Seq[String] = {
    val out = ArrayBuffer.empty[String]
    exp match {
      case v: ExpressionMultiply           => out ++= generate(v.v1) ++ Seq("*") ++ generate(v.v2)
      case v: ExpressionDivision           => out ++= generate(v.v1) ++ Seq("/") ++ generate(v.v2)
      case v: ExpressionMod                => out ++= generate(v.v1) ++ Seq("%") ++ generate(v.v2)
      case v: ExpressionAdd                => out ++= generate(v.v1) ++ Seq("+") ++ generate(v.v2)
      case v: ExpressionMinus              => out ++= generate(v.v1) ++ Seq("-") ++ generate(v.v2)
      case v: ExpressionLeftShift          => out ++= generate(v.v1) ++ Seq("<<") ++ generate(v.v2)
      case v: ExpressionRightShift         => out ++= generate(v.v1) ++ Seq(">>") ++ generate(v.v2)
      case v: ExpressionLessThan           => out ++= generate(v.v1) ++ Seq("<") ++ generate(v.v2)
      case v: ExpressionGreaterThan        => out ++= generate(v.v1) ++ Seq(">") ++ generate(v.v2)
      case v: ExpressionLessThanOrEqual    => out ++= generate(v.v1) ++ Seq("<=") ++ generate(v.v2)
      case v: ExpressionGreaterThanOrEqual => out ++= generate(v.v1) ++ Seq(">=") ++ generate(v.v2)
      case v: ExpressionEquals             => out ++= generate(v.v1) ++ Seq("==") ++ generate(v.v2)
      case v: ExpressionNotEquals          => out ++= generate(v.v1) ++ Seq("!=") ++ generate(v.v2)
      case v: ExpressionAnd                => out ++= generate(v.v1) ++ Seq("&") ++ generate(v.v2)
      case v: ExpressionXOr                => out ++= generate(v.v1) ++ Seq("^") ++ generate(v.v2)
      case v: ExpressionInclusiveOr        => out ++= generate(v.v1) ++ Seq("|") ++ generate(v.v2)
      case v: ExpressionLogicalAnd         => out ++= generate(v.v1) ++ Seq("&&") ++ generate(v.v2)
      case v: ExpressionLogicalOr          => out ++= generate(v.v1) ++ Seq("||") ++ generate(v.v2)
      case v: ExpressionConditional        => out ++= generate(v.v1) ++ Seq("?") ++ generate(v.v2) ++ Seq(":") ++ generate(v.v3)
      case v: ExpressionComma              => out ++= generate(v.v1) ++ Seq(",") ++ generate(v.v2)
      case v: ExpressionAssignment         => out ++= generate(v.v1) ++ Seq("=") ++ generate(v.v2)
      case v: IntConstant                  => out += v.v.toString
      case v: FloatConstant                => out += v.v.toString
      case v: StringLiteral                => out ++= Seq("\"" + v.v + "\"")
      case v: Identifier                   => out += v.v
      case v: PostfixExpressionIndex       =>
        out ++= generate(v.v1)
        out += "["
        out ++= generate(v.v2)
        out += "]"
      case v: PostfixExpressionMinusMinus  =>
        out ++= generate(v.v1)
        out += "--"
      case v: PostfixExpressionPlusPlus    =>
        out ++= generate(v.v1)
        out += "++"
    }
    out
  }
}
