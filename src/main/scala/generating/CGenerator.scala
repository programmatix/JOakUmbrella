package generating

import parsing._
import parsing.{BlockItem, TranslationUnit}

import scala.collection.mutable.ArrayBuffer


class CGenerator {
  private val nll = "\n"
  private val nl = Seq[String](nll)
  private val el = Seq(";", nll)

  def generateBlockItem(in: BlockItem): Seq[String] = {
    in match {
      case v: Statement   => generateStatement(v)
      case v: Declaration => generateDeclaration(v)
    }
  }

  def generateDeclaration(in: Declaration): Seq[String] = {
    in match {
      case v: SimpleDeclaration       => generateDeclarationSpecifiers(v.spec) ++ v.init.map(generateInitDeclatorList).getOrElse(Seq()) ++ el
      case v: StaticAssertDeclaration => Seq("_Static_assert", "(") ++ generateExpression(v.v1) ++ Seq(",") ++ generateExpression(v.v2) ++ Seq(")") ++ el
    }
  }

  def generateSeqBlockItem(in: Seq[BlockItem]): Seq[String] = in.flatMap(v => generateBlockItem(v))

  def generateTranslationUnit(in: TranslationUnit): Seq[String] = in.v.flatMap(v => generateTop(v))

  //  def generate(in: CFile): Seq[String] = in.v.flatMap(v => generate(v))
  //  def generate(in: CFile): Seq[String] = generate(in.v)

  def generateDeclarationSpecifiers(in: DeclarationSpecifiers): Seq[String] = in.v.flatMap(v => generateDeclarationSpecifier(v))

  def generateDeclarationList(in: DeclarationList): Seq[String] = in.v.flatMap(v => generateBlockItem(v))

  //  def generate(in: Seq[InitDeclarator]): Seq[String] = in.flatMap(v => generate(v))

  def generateParameterDeclaration(in: ParameterDeclaration): Seq[String] = {
    in match {
      case v: ParameterDeclarationDeclarator => generateDeclarationSpecifiers(v.v) ++ generateDeclarator(v.v2)
    }
  }

  def generateParameterTypeList(in: ParameterTypeList): Seq[String] = {
    val it = in.v.iterator
    if (it.hasNext) {
      val out = ArrayBuffer.empty[String]
      out ++= generateParameterDeclaration(it.next())
      while(it.hasNext) {
        out += ","
        out ++= generateParameterDeclaration(it.next())
      }
      if (in.ellipses) {
        out ++= Seq(",", "...")
      }
      out
    }
    else Seq()
  }

  //  def generateDDBracketed(in: DDBracketed): Seq[String] = {
  //    generate(in.declarator)
  //  }

  def generateDirectDeclarator(in: DirectDeclarator): Seq[String] = {
    in match {
      case v: DDBracketed          => generateDeclarator(v.declarator)
      case v: DirectDeclaratorOnly => generateIdentifier(v.v)
      case v: FunctionDeclaration  => ((generateIdentifier(v.name) :+ "(") ++ generateParameterTypeList(v.params) :+ ")") ++ nl
    }
  }

  def generateDeclarator(in: Declarator): Seq[String] = {
    in.pointer.map(v => Seq(v)).getOrElse(Seq()) ++ generateDirectDeclarator(in.v)
  }

  def generateStatement(in: Statement): Seq[String] = {
    in match {
      case v: Break               => Seq("break;") ++ el
      case v: Continue            => Seq("continue;") ++ el
      case v: Goto                => ("goto " +: generateIdentifier(v.v)) ++ Seq(";", nll)
      case v: IterationWhile      => Seq("while", "(") ++ generateExpression(v.v1) ++ Seq(")", nll) ++ generateStatement(v.v2) ++ Seq(nll)
      case v: IterationDoWhile      => Seq("do") ++ Seq(nll) ++ generateStatement(v.v2) ++ Seq("while", "(") ++ generateExpression(v.v1) ++ Seq(")", ";", nll)
      case v: IterationFor1      => Seq("for") ++ Seq("(") ++
        v.v1.map(x => generateExpression(x)).getOrElse(Seq()) ++ Seq(";") ++
        v.v2.map(x => generateExpression(x)).getOrElse(Seq()) ++ Seq(";") ++
        v.v3.map(x => generateExpression(x)).getOrElse(Seq[String]()) ++ Seq(")", nll) ++
        generateStatement(v.v4) ++
        Seq(nll)
      case v: IterationFor2      => Seq("for") ++ Seq("(") ++
        generateDeclaration(v.v) ++
        v.v1.map(x => generateExpression(x)).getOrElse(Seq()) ++ Seq(";") ++
        v.v2.map(x => generateExpression(x)).getOrElse(Seq[String]()) ++ Seq(")", nll) ++
        generateStatement(v.v3) ++ nl
      case v: LabelledLabel       => (generateIdentifier(v.v1) :+ ": ") ++ generateStatement(v.v2)
      case v: LabelledCase        => ("case " +: generateExpression(v.v1) :+ ": ") ++ generateStatement(v.v2)
      case v: CompoundStatement   => Seq("{") ++ v.v.flatMap(generateBlockItem) ++ Seq("}")
      case v: ExpressionStatement => generateExpression(v.expression) ++ el
      case v: SelectionIf => Seq("if", "(") ++ generateExpression(v.v1) ++ Seq(")") ++ generateStatement(v.v2)
      case v: SelectionIfElse => Seq("if", "(") ++ generateExpression(v.v1) ++ Seq(")") ++ generateStatement(v.v2) ++ Seq("else") ++ generateStatement(v.v3)
      case v: SelectionSwitch => Seq("switch", "(") ++ generateExpression(v.v1) ++ Seq(")") ++ generateStatement(v.v2)
      case v: ExpressionEmptyStatement => nl
//      case v: StatementDeclaration =>
      case v: Return              =>
        v.v match {
          case Some(exp) => Seq("return ") ++ generateExpression(exp) ++ el
          case _         => Seq("return") ++ el
        }
      case v: LabelledDefault     => Seq("default: ", nll) ++ generateStatement(v.v2) ++ nl
    }
  }

  //  def generate(dec: Declarator): Seq[String] = {
  //    val out = ArrayBuffer.empty[String]
  //    dec.pointer.map(out += _)
  //    out ++= generateDirectDeclarator(dec.v)
  //    out
  //  }

  def generateInitializer(in: Initializer): Seq[String] = {
    in match {
      case v: InitializerSimple => generateExpression(v.exp)
      case v: InitializerList   => generateExpression(v.exp)
    }
  }

  def generateInitDeclarator(in: InitDeclarator): Seq[String] = {
    in match {
      case v: DeclaratorEmpty    => generateDeclarator(v.declarator)
      case v: DeclaratorWithInit => generateDeclarator(v.declarator) ++ Seq("=") ++ generateInitializer(v.init)
    }
  }

  def generateStructDeclarator(in: StructDeclaractor): Seq[String] = {
    in match {
      case v: StructDeclaractor1    => generateDeclarator(v.v)
      case v: StructDeclaractor2    => v.v.map(generateDeclarator).getOrElse(Seq()) ++ generateExpression(v.exp)
    }
  }

  def generateInitDeclatorList(v: Seq[InitDeclarator]): Seq[String] = {
    val it = v.iterator
    generateInitDeclarator(it.next()) ++ it.flatMap(x => Seq(",") ++ generateInitDeclarator(x))
  }
  def generateStructDeclarationList(v: Seq[StructDeclaration]): Seq[String] = v.flatMap(generateStructDeclaration)
  def generateStructDeclaratorList(v: StructDeclaratorList): Seq[String] = {
    val it = v.v.iterator
    generateStructDeclarator(it.next()) ++ it.flatMap(x => Seq(",") ++ generateStructDeclarator(x))
  }
  def generateSpecifierQualifierList(v: Seq[DeclarationSpecifier]): Seq[String] = v.flatMap(generateDeclarationSpecifier)
  def generateStructDeclaration(in: StructDeclaration): Seq[String] = {
      generateSpecifierQualifierList(in.v) ++ in.v2.map(x => generateStructDeclaratorList(x)).getOrElse(Seq()) ++ Seq(";", nll)
  }

  def generateIdentifier(in: Identifier): Seq[String] = {
    Seq(in.v)
  }

  def generateOptDeclarationList(in: Option[DeclarationList]): Seq[String] = {
    val out = ArrayBuffer.empty[String]
    in.map(v => out ++= generateDeclarationList(v))
    out
  }

  def generateExternalDeclaration(in: ExternalDeclaration): Seq[String] = {
    in match {
      case v: FunctionDefinition =>
        generateDeclarationSpecifiers(v.spec) ++ generateDeclarator(v.dec) ++ generateOptDeclarationList(v.decs) ++ Seq(nll) ++ generateStatement(v.v) ++ Seq(nll)
      case v: Declaration        => generateDeclaration(v)
    }
  }

  def generateIdentifierList(in: Seq[Identifier]): Seq[String] = {
    val it = in.iterator
    generateIdentifier(it.next()) ++ it.flatMap(x => Seq(",") ++ generateIdentifier(x))
  }

  def generateDeclarationSpecifier(in: DeclarationSpecifier): Seq[String] = {
    in match {
      case v: StorageClassSpecifier => Seq(v.v)
      case v: StructImpl => Seq(if (v.isStruct) "struct" else "union") ++ v.id.map(generateIdentifier).getOrElse(Seq()) ++ Seq("{") ++ generateStructDeclarationList(v.v2) ++ Seq("}")
      case v: StructUse => Seq(if (v.isStruct) "struct" else "union") ++ generateIdentifier(v.id)
      case v: TypeSpecifierSimple         => Seq(v.v)
      case v: TypeQualifier         => Seq(v.v)
      case v: FunctionSpecifier     => Seq(v.v)
      case v: AlignmentSpecifier    => Seq(v.v)
    }
  }


  def generateExpression(exp: Expression): Seq[String] = {
    exp match {
      case v: ExpressionMultiply           => generateExpression(v.v1) ++ Seq("*") ++ generateExpression(v.v2)
      case v: ExpressionDivision           => generateExpression(v.v1) ++ Seq("/") ++ generateExpression(v.v2)
      case v: ExpressionMod                => generateExpression(v.v1) ++ Seq("%") ++ generateExpression(v.v2)
      case v: ExpressionAdd                => generateExpression(v.v1) ++ Seq("+") ++ generateExpression(v.v2)
      case v: ExpressionMinus              => generateExpression(v.v1) ++ Seq("-") ++ generateExpression(v.v2)
      case v: ExpressionLeftShift          => generateExpression(v.v1) ++ Seq("<<") ++ generateExpression(v.v2)
      case v: ExpressionRightShift         => generateExpression(v.v1) ++ Seq(">>") ++ generateExpression(v.v2)
      case v: ExpressionLessThan           => generateExpression(v.v1) ++ Seq("<") ++ generateExpression(v.v2)
      case v: ExpressionGreaterThan        => generateExpression(v.v1) ++ Seq(">") ++ generateExpression(v.v2)
      case v: ExpressionLessThanOrEqual    => generateExpression(v.v1) ++ Seq("<=") ++ generateExpression(v.v2)
      case v: ExpressionGreaterThanOrEqual => generateExpression(v.v1) ++ Seq(">=") ++ generateExpression(v.v2)
      case v: ExpressionEquals             => generateExpression(v.v1) ++ Seq("==") ++ generateExpression(v.v2)
      case v: ExpressionNotEquals          => generateExpression(v.v1) ++ Seq("!=") ++ generateExpression(v.v2)
      case v: ExpressionAnd                => generateExpression(v.v1) ++ Seq("&") ++ generateExpression(v.v2)
      case v: ExpressionXOr                => generateExpression(v.v1) ++ Seq("^") ++ generateExpression(v.v2)
      case v: ExpressionInclusiveOr        => generateExpression(v.v1) ++ Seq("|") ++ generateExpression(v.v2)
      case v: ExpressionLogicalAnd         => generateExpression(v.v1) ++ Seq("&&") ++ generateExpression(v.v2)
      case v: ExpressionLogicalOr          => generateExpression(v.v1) ++ Seq("||") ++ generateExpression(v.v2)
      case v: ExpressionConditional        => generateExpression(v.v1) ++ Seq("?") ++ generateExpression(v.v2) ++ Seq(":") ++ generateExpression(v.v3)
      case v: ExpressionComma              => generateExpression(v.v1) ++ Seq(",") ++ generateExpression(v.v2)
      case v: ExpressionAssignment         => generateExpression(v.v1) ++ Seq("=") ++ generateExpression(v.v2)
      case v: EnumerationConstant                  => Seq(v.v)
      case v: CharacterConstant                  => Seq(v.v)
      case v: IntConstant                  => Seq(v.v.toString)
      case v: FloatConstant                => Seq(v.v.toString)
      case v: StringLiteral                => Seq("\"" + v.v + "\"")
      case v: Identifier                   => Seq(v.v)
      case v: PostfixExpressionIndex       => generateExpression(v.v1) ++ Seq("[") ++ generateExpression(v.v2) ++ Seq("]")
      case v: PostfixExpressionMinusMinus  => generateExpression(v.v1) ++ Seq("--")
      case v: PostfixExpressionPlusPlus    => generateExpression(v.v1) ++ Seq("++")
      case v: PostfixExpressionDot         => generateExpression(v.v1) ++ Seq("->") ++ generateExpression(v.v2)
      case v: PostfixExpressionArrow       => generateExpression(v.v1) ++ Seq("->") ++ generateExpression(v.v2)
      case v: PostfixExpressionArgs        => generateExpression(v.v1) ++ Seq("(") ++ v.v2.map(generateExpression).getOrElse(Seq()) ++ Seq(")")
      case v: PostfixExpressionSimple      => generateExpression(v.v1)
      case v: CastExpression               => Seq("(") ++ generateTypeName(v.v) ++ Seq(")") ++ generateExpression(v.v2)
      case v: UnaryExpressionPlusPlus => Seq("++") ++ generateExpression(v.v)
      case v: UnaryExpressionMinusMinus => Seq("--") ++ generateExpression(v.v)
      case v: UnaryExpressionCast => Seq(v.v.toString) ++ generateExpression(v.v2)
      case v: UnaryExpressionSizeOf => Seq("sizeof") ++ generateExpression(v.v)
      case v: UnaryExpressionSizeOfType => Seq("sizeof", "(") ++ generateTypeName(v.v) ++ Seq(")")
      case v: UnaryExpressionAlignOf => Seq("_Alignof", "(") ++ generateTypeName(v.v) ++ Seq(")")
      case v: ArgumentExpressionList => if (v.v.nonEmpty) {
        val it = v.v.iterator
        it.next()
        generateExpression(v.v.head) ++ it.flatMap(x => Seq(",") ++ generateExpression(x))
      }
      else {
        Seq()
      }
    }
  }

  def generateTypeName(name: TypeName): Seq[String] = {
    Seq(name.v)
  }

  def generateGroup(v: Group): Seq[String] = v.v.flatMap(generateGroupPart(_))

  def generateGroupPart(in: GroupPart): Seq[String] = {
    in match {
      case v: IfSection        => generateInGroup(v.ifGroup) ++ v.elif.map(y => y.flatMap(x => generateElifGroup(x))).getOrElse(Seq()) ++ v.elseGroup.map(generateElseGroup(_)).getOrElse(Seq()) ++ generateEndifLine(v.endif)
      case v: Include          => Seq("#include") ++ generatePPTokens(v.v)
      case v: Define           => Seq("#define") ++ generateIdentifier(v.ident) ++ generateReplacementList(v.v)
      case v: Define2          => Seq("#define") ++ generateIdentifier(v.ident) ++ Seq("(") ++ v.v.map(x => generateIdentifierList(x)).getOrElse(Seq()) ++ Seq(")") ++ generateReplacementList(v.v2)
      case v: Define3          => Seq("#define") ++ generateIdentifier(v.ident) ++ Seq("(", "...", ")") ++ generateReplacementList(v.v)
      case v: Define4          => Seq("#define") ++ generateIdentifier(v.ident) ++ Seq("(") ++ generateIdentifierList(v.v) ++ Seq(",", "...", ")") ++ generateReplacementList(v.v2)
      case v: Undef            => Seq("#undef") ++ generateIdentifier(v.ident)
      case v: Line             => Seq("#line") ++ generatePPTokens(v.v)
      case v: Error            => Seq("#error") ++ v.v.map(_.flatMap(generatePPToken)).getOrElse(Seq())
      case v: Pragma           => Seq("#pragma") ++ v.v.map(_.flatMap(generatePPToken)).getOrElse(Seq())
      case v: ControlLineEmpty => Seq("#")
      case v: NonDirective     => Seq("#") ++ generatePPTokens(v.pp)
      case v: TextLine         =>
        assert(false)
        Seq()
    }
  }

  def generateInGroup(in: IfGroup): Seq[String] = {
    in match {
      case v: If     => Seq("#if") ++ generateExpression(v.exp) ++ v.group.map(generateGroup(_)).getOrElse(Seq())
      case v: IfDef  => Seq("#ifdef") ++ generateIdentifier(v.ident) ++ v.group.map(generateGroup(_)).getOrElse(Seq())
      case v: IfNDef => Seq("#ifndef") ++ generateIdentifier(v.ident) ++ v.group.map(generateGroup(_)).getOrElse(Seq())
    }
  }

  def generateElifGroup(in: ElifGroup): Seq[String] = {
    Seq("#elif") ++ generateExpression(in.exp) ++ in.group.map(generateGroup(_)).getOrElse(Seq())
  }

  def generateElseGroup(in: ElseGroup): Seq[String] = {
    Seq("#else") ++ in.group.map(generateGroup(_)).getOrElse(Seq())
  }

  def generateEndifLine(in: EndifLine): Seq[String] = {
    Seq("#endif")
  }

  //  def generate(in: ControlLine): Seq[String] = {
  //    in match {
  //    }
  //  }

  def generateNonDirective(in: NonDirective): Seq[String] = in.pp.flatMap(generatePPToken(_))

  def generateReplacementList(in: ReplacementList): Seq[String] = in.v.map(_.flatMap(generatePPToken(_))).getOrElse(Seq())

  def generatePPTokens(in: Seq[PPToken]): Seq[String] = in.flatMap(generatePPToken(_))

  def generatePPToken(in: PPToken): Seq[String] = {
    in match {
      case v: HeaderName => generateHeaderName(v)
      case v: Identifier => generateIdentifier(v)
      case v: Constant   => generateExpression(v)
      //      case v: CharacterConstant => generate(v)
      case v: StringLiteral => generateExpression(v)
      case v: Punctuator    => Seq("P:", v.v)
    }
  }

  def generateHeaderName(v: HeaderName): Seq[String] = {
    Seq(if (v.angularBrackets) "<" else "\"", v.v, if (v.angularBrackets) ">" else "\"")
  }

  def generateTop(x: Top): Seq[String] = {
    x match {
      case v: ExternalDeclaration => generateExternalDeclaration(v)
      case v: PreprocessingFile   => generateGroup(v.v)
    }
  }


}
