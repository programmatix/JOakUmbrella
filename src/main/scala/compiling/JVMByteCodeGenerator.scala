package compiling

import compiling.JVMByteCode._
import parsing._
import parsing.{BlockItem, TranslationUnit}

import scala.collection.mutable.ArrayBuffer


// Converts a C AST into JVM bytecode.  It returns a Seq[JVMByteCode.Generated]
class JVMByteCodeGenerator {
  @Deprecated
  private val newlineIndent: Seq[Generated] = Seq()
  @Deprecated
  private val endlineIndent: Seq[Generated] = Seq() //Seq(GenString(";"), NewlineAndIndent())

  // Helpers to turn Seq[String] into Generated
  import scala.language.implicitConversions

  private implicit def stringsToGenerated(x: Seq[String]): Seq[Generated] = Seq(GenStrings(x))

  private def GS(v: String) = Seq(GenString(v))

  private def GS(v: String*) = Seq(GenStrings(v))

  def generateBlockItem(in: BlockItem): Seq[Generated] = {
    in match {
      case v: Statement   => generateStatement(v)
      case v: Declaration => generateDeclaration(v)
    }
  }

  def generateDeclaration(in: Declaration): Seq[Generated] = {
    in match {
      case v: SimpleDeclaration       => generateSimpleDeclaration(v)
      case v: StaticAssertDeclaration => GS("_Static_assert", "(") ++ generateExpression(v.v1) ++ GS(",") ++ generateExpression(v.v2) ++ GS(")") ++ endlineIndent
    }
  }

  def resolveDeclaratorToIdentifer(in: Declarator): Identifier = {
    if (in.pointer.isDefined) {
      throw JVMGenUnsupportedCurrently("handle pointers while trying to create identifier")
    }
    in.v match {
      case v: DDBracketed          => throw JVMGenUnsupportedCurrently("handle DDBracketed while trying to create identifier")
      case v: DirectDeclaratorOnly => v.v
      case v: FunctionDeclaration  => throw JVMGenUnsupportedCurrently("handle FunctionDeclaration while trying to create identifier")
    }

  }

  // >>int hello = "world"<<;
  def generateSimpleDeclaration(in: SimpleDeclaration): Seq[Generated] = {

    val typ = resolveDeclarationSpecifiersToTypeString(in.spec)

    in.init match {
      case Some(initDeclarators) =>
        // int >>hello<<;
        // int >>hello=3<<;
        initDeclarators.flatMap(initDeclarator => initDeclarator match {
          // int >>hello<<;
          case v: DeclaratorEmpty =>
            val varName = resolveDeclaratorToIdentifer(v.declarator).v
            val out: Seq[Generated] = Seq(DeclareVariable(varName, typ))
            out

          // int >>hello=3<<;
          case v: DeclaratorWithInit =>
            val varName = resolveDeclaratorToIdentifer(v.declarator).v
            val out: Seq[Generated] = Seq(DeclareVariable(varName, typ)) ++ generateInitializer(v.init)
            out
        })
      case _                     =>
        throw JVMGenUnsupportedCurrently(s"cannot handle SimpleDeclaration ${in} as no declaration")
    }
  }

  def generateSeqBlockItem(in: Seq[BlockItem]): Seq[Generated] = in.flatMap(v => generateBlockItem(v))

  def generateTranslationUnit(in: TranslationUnit): Seq[Generated] = in.v.flatMap(v => generateTop(v))

  def resolveDeclarationSpecifiersToTypeString(in: DeclarationSpecifiers): String = {
    var out: Option[String] = None
    in.v.foreach {
      case v: StorageClassSpecifier => throw JVMGenUnsupportedCurrently("StorageClassSpecifier while trying to resolve declaration specifier to type")
      case v: StructImpl            => throw JVMGenUnsupportedCurrently("StructImpl while trying to resolve declaration specifier to type")
      case v: StructUse             => throw JVMGenUnsupportedCurrently("StructUse while trying to resolve declaration specifier to type")
      case v: TypeQualifier         => throw JVMGenUnsupportedCurrently("TypeQualifier while trying to resolve declaration specifier to type")
      case v: TypeSpecifierSimple   => out = Some(v.v)
      case v: FunctionSpecifier     => throw JVMGenUnsupportedCurrently("FunctionSpecifier while trying to resolve declaration specifier to type")
      case v: AlignmentSpecifier    => throw JVMGenUnsupportedCurrently("AlignmentSpecifier while trying to resolve declaration specifier to type")
    }

    if (out.isEmpty) throw JVMGenUnsupportedCurrently(s"unable to find useful type in ${in}")
    out.get
  }


  def generateDeclarationSpecifiers(in: DeclarationSpecifiers): Seq[Generated] = in.v.flatMap(v => generateDeclarationSpecifier(v))

  def generateDeclarationList(in: DeclarationList): Seq[Generated] = in.v.flatMap(v => generateBlockItem(v))

  def generateParameterDeclaration(in: ParameterDeclaration): Seq[Generated] = {
    in match {
      case v: ParameterDeclarationDeclarator => generateDeclarationSpecifiers(v.v) ++ generateDeclarator(v.v2)
    }
  }

  def generateParameterTypeList(in: ParameterTypeList): Seq[Generated] = {
    val it = in.v.iterator
    if (it.hasNext) {
      val out = ArrayBuffer.empty[Generated]
      out ++= generateParameterDeclaration(it.next())
      while (it.hasNext) {
        out += GenStrings(Seq(","))
        out ++= generateParameterDeclaration(it.next())
      }
      if (in.ellipses) {
        out += GenStrings(Seq(",", "..."))
      }
      out
    }
    else Seq()
  }

  def generateDirectDeclarator(in: DirectDeclarator): Seq[Generated] = {
    in match {
      case v: DDBracketed          => generateDeclarator(v.declarator)
      case v: DirectDeclaratorOnly => generateIdentifier(v.v)
      case v: FunctionDeclaration  => ((generateIdentifier(v.name) ++ GS("(")) ++ generateParameterTypeList(v.params) ++ GS(")")) ++ newlineIndent
    }
  }

  def generateDeclarator(in: Declarator): Seq[Generated] = {
    in.pointer.map(v => GS(v)).getOrElse(Seq[Generated]()) ++ generateDirectDeclarator(in.v)
  }

  def generateStatement(in: Statement): Seq[Generated] = {
    in match {
      case v: Break                    => GS("break") ++ endlineIndent
      case v: Continue                 => GS("continue") ++ endlineIndent
      case v: Goto                     => GS("goto") ++ generateIdentifier(v.v) ++ endlineIndent
      case v: IterationWhile           => GS("while", "(") ++ generateExpression(v.v1) ++ GS(")") ++ newlineIndent ++ generateStatement(v.v2)
      case v: IterationDoWhile         => GS("do") ++ newlineIndent ++ generateStatement(v.v2) ++ GS("while", "(") ++ generateExpression(v.v1) ++ GS(")") ++ endlineIndent
      case v: IterationFor1            => GS("for") ++ GS("(") ++
        v.v1.map(x => generateExpression(x)).getOrElse(Seq()) ++ GS(";") ++
        v.v2.map(x => generateExpression(x)).getOrElse(Seq()) ++ GS(";") ++
        v.v3.map(x => generateExpression(x)).getOrElse(Seq[Generated]()) ++ GS(")") ++ newlineIndent ++
        generateStatement(v.v4) ++
        newlineIndent
      case v: IterationFor2            => GS("for") ++ GS("(") ++
        generateDeclaration(v.v) ++
        v.v1.map(x => generateExpression(x)).getOrElse(Seq()) ++ GS(";") ++
        v.v2.map(x => generateExpression(x)).getOrElse(Seq[Generated]()) ++ GS(")") ++ newlineIndent ++
        generateStatement(v.v3) ++ newlineIndent
      case v: LabelledLabel            => generateIdentifier(v.v1) ++ GS(":") ++ generateStatement(v.v2)
      case v: LabelledCase             => GS("case") ++ generateExpression(v.v1) ++ GS(":") ++ Seq(NewlineAndIndentUp()) ++ generateStatement(v.v2) ++ Seq(IndentDown())
      case cs: CompoundStatement       => {
        val blockItems: Seq[Generated] = {
          val it = cs.v.iterator
          val out = ArrayBuffer.empty[Generated]
          while (it.hasNext) {
            out ++= generateBlockItem(it.next())
            if (it.hasNext) out ++= Seq(GenString(";"), NewlineAndIndent())
            else out ++= Seq(GenString(";"))
          }
          //          val out = it.flatMap(x => generateBlockItem(x)) ++ (if (it.hasNext) Seq(GenString(";"), NewlineAndIndent()) else Seq())
          out.toList
        }
        GS("{") ++ Seq(NewlineAndIndentUp()) ++ blockItems ++ Seq(NewlineAndIndentDown()) ++ GS("}") ++ newlineIndent
      }
      case v: ExpressionStatement      => generateExpression(v.expression) ++ endlineIndent
      case v: SelectionIf              => GS("if", "(") ++ generateExpression(v.v1) ++ GS(")") ++ generateStatement(v.v2)
      case v: SelectionIfElse          => GS("if", "(") ++ generateExpression(v.v1) ++ GS(")") ++ generateStatement(v.v2) ++ GS("else") ++ generateStatement(v.v3)
      case v: SelectionSwitch          => GS("switch", "(") ++ generateExpression(v.v1) ++ GS(")") ++ generateStatement(v.v2)
      case v: ExpressionEmptyStatement => newlineIndent ++ Seq(GenString(";"), NewlineAndIndent())
      //      case v: StatementDeclaration =>
      case v: Return          =>
        v.v match {
          case Some(exp) =>
            val simple = convertExpressionToSimple(exp).toShort
            Seq(sipush(simple), ireturn())
          case _         => Seq(ret())
        }
      case v: LabelledDefault => GS("default:") ++ Seq(NewlineAndIndentUp()) ++ generateStatement(v.v2) ++ Seq(IndentDown())
    }
  }

  def generateInitializer(in: Initializer): Seq[Generated] = {
    in match {
      case v: InitializerSimple =>
        generateExpression(v.exp) ++ Seq(StoreExpressionInCurrentVar())
      case v: InitializerList   => generateExpression(v.exp)
    }
  }

  @Deprecated
  def generateInitDeclarator(in: InitDeclarator): Seq[Generated] = {
    in match {
      case v: DeclaratorEmpty    => generateDeclarator(v.declarator)
      case v: DeclaratorWithInit => generateDeclarator(v.declarator) ++ GS("=") ++ generateInitializer(v.init)
    }
  }

  def generateStructDeclarator(in: StructDeclaractor): Seq[Generated] = {
    in match {
      case v: StructDeclaractor1 => generateDeclarator(v.v)
      case v: StructDeclaractor2 => v.v.map(generateDeclarator).getOrElse(Seq()) ++ generateExpression(v.exp)
    }
  }

  @Deprecated
  def generateInitDeclatorList(v: Seq[InitDeclarator]): Seq[Generated] = {
    val it = v.iterator
    generateInitDeclarator(it.next()) ++ it.flatMap(x => GS(",") ++ generateInitDeclarator(x))
  }

  def generateStructDeclarationList(v: Seq[StructDeclaration]): Seq[Generated] = v.flatMap(generateStructDeclaration)

  def generateStructDeclaratorList(v: StructDeclaratorList): Seq[Generated] = {
    val it = v.v.iterator
    generateStructDeclarator(it.next()) ++ it.flatMap(x => GS(",") ++ generateStructDeclarator(x))
  }

  def generateSpecifierQualifierList(v: Seq[DeclarationSpecifier]): Seq[Generated] = v.flatMap(generateDeclarationSpecifier)

  def generateStructDeclaration(in: StructDeclaration): Seq[Generated] = {
    generateSpecifierQualifierList(in.v) ++ in.v2.map(x => generateStructDeclaratorList(x)).getOrElse(Seq()) ++ Seq(GenString(";"), NewlineAndIndent())
  }

  def generateIdentifier(in: Identifier): Seq[Generated] = {
    GS(in.v)
  }

  def generateOptDeclarationList(in: Option[DeclarationList]): Seq[Generated] = {
    val out = ArrayBuffer.empty[Generated]
    in.map(v => out ++= generateDeclarationList(v))
    out
  }

  def generateExternalDeclaration(in: ExternalDeclaration): Seq[Generated] = {
    in match {
      case v: FunctionDefinition => generateDeclarationSpecifiers(v.spec) ++ generateDeclarator(v.dec) ++ generateOptDeclarationList(v.decs) ++ generateStatement(v.v) ++ Seq(NewlineOnly(), NewlineAndIndent())
      case v: Declaration        => generateDeclaration(v) ++ Seq(GenString(";"), NewlineAndIndent())
    }
  }

  def generateIdentifierList(in: Seq[Identifier]): Seq[Generated] = {
    val it = in.iterator
    generateIdentifier(it.next()) ++ it.flatMap(x => GS(",") ++ generateIdentifier(x))
  }

  def generateDeclarationSpecifier(in: DeclarationSpecifier): Seq[Generated] = {
    in match {
      case v: StorageClassSpecifier => GS(v.v)
      case v: StructImpl            => GS(if (v.isStruct) "struct" else "union") ++ v.id.map(generateIdentifier).getOrElse(Seq()) ++ GS("{") ++ Seq(NewlineAndIndentUp()) ++ generateStructDeclarationList(v.v2) ++ Seq(NewlineAndIndentDown()) ++ GS("}")
      case v: StructUse             => GS(if (v.isStruct) "struct" else "union") ++ generateIdentifier(v.id)
      case v: TypeSpecifierSimple   => GS(v.v)
      case v: TypeQualifier         => GS(v.v)
      case v: FunctionSpecifier     => GS(v.v)
      case v: AlignmentSpecifier    => GS(v.v)
    }
  }

  private def convertExpressionToSimple(exp: Expression): String

  = {
    exp match {
      //      case v: ExpressionMultiply           => generateExpression(v.v1) ++ GS("*") ++ generateExpression(v.v2)
      //      case v: ExpressionDivision           => generateExpression(v.v1) ++ GS("/") ++ generateExpression(v.v2)
      //      case v: ExpressionMod                => generateExpression(v.v1) ++ GS("%") ++ generateExpression(v.v2)
      //      case v: ExpressionAdd                => generateExpression(v.v1) ++ GS("+") ++ generateExpression(v.v2)
      //      case v: ExpressionMinus              => generateExpression(v.v1) ++ GS("-") ++ generateExpression(v.v2)
      //      case v: ExpressionLeftShift          => generateExpression(v.v1) ++ GS("<<") ++ generateExpression(v.v2)
      //      case v: ExpressionRightShift         => generateExpression(v.v1) ++ GS(">>") ++ generateExpression(v.v2)
      //      case v: ExpressionLessThan           => generateExpression(v.v1) ++ GS("<") ++ generateExpression(v.v2)
      //      case v: ExpressionGreaterThan        => generateExpression(v.v1) ++ GS(">") ++ generateExpression(v.v2)
      //      case v: ExpressionLessThanOrEqual    => generateExpression(v.v1) ++ GS("<=") ++ generateExpression(v.v2)
      //      case v: ExpressionGreaterThanOrEqual => generateExpression(v.v1) ++ GS(">=") ++ generateExpression(v.v2)
      //      case v: ExpressionEquals             => generateExpression(v.v1) ++ GS("==") ++ generateExpression(v.v2)
      //      case v: ExpressionNotEquals          => generateExpression(v.v1) ++ GS("!=") ++ generateExpression(v.v2)
      //      case v: ExpressionAnd                => generateExpression(v.v1) ++ GS("&") ++ generateExpression(v.v2)
      //      case v: ExpressionXOr                => generateExpression(v.v1) ++ GS("^") ++ generateExpression(v.v2)
      //      case v: ExpressionInclusiveOr        => generateExpression(v.v1) ++ GS("|") ++ generateExpression(v.v2)
      //      case v: ExpressionLogicalAnd         => generateExpression(v.v1) ++ GS("&&") ++ generateExpression(v.v2)
      //      case v: ExpressionLogicalOr          => generateExpression(v.v1) ++ GS("||") ++ generateExpression(v.v2)
      //      case v: ExpressionConditional        => generateExpression(v.v1) ++ GS("?") ++ generateExpression(v.v2) ++ GS(":") ++ generateExpression(v.v3)
      //      case v: ExpressionComma              => generateExpression(v.v1) ++ GS(",") ++ generateExpression(v.v2)
      //      case v: ExpressionAssignment         => generateExpression(v.v1) ++ GS("=") ++ generateExpression(v.v2)
      //      case v: EnumerationConstant          => GS(v.v)
      //      case v: CharacterConstant            => GS(v.v)
      case v: IntConstant => v.v.toString
      //      case v: FloatConstant                => GS(v.v.toString)
      //      case v: StringLiteral                => GS("\"" + v.v + "\"")
      //      case v: Identifier                   => GS(v.v)
      //      case v: PostfixExpressionIndex       => generateExpression(v.v1) ++ GS("[") ++ generateExpression(v.v2) ++ GS("]")
      //      case v: PostfixExpressionMinusMinus  => generateExpression(v.v1) ++ GS("--")
      //      case v: PostfixExpressionPlusPlus    => generateExpression(v.v1) ++ GS("++")
      //      case v: PostfixExpressionDot         => generateExpression(v.v1) ++ GS("->") ++ generateExpression(v.v2)
      //      case v: PostfixExpressionArrow       => generateExpression(v.v1) ++ GS("->") ++ generateExpression(v.v2)
      //      case v: PostfixExpressionArgs        => generateExpression(v.v1) ++ GS("(") ++ v.v2.map(generateExpression).getOrElse(Seq()) ++ GS(")")
      //      case v: PostfixExpressionSimple      => generateExpression(v.v1)
      //      case v: CastExpression               => GS("(") ++ generateTypeName(v.v) ++ GS(")") ++ generateExpression(v.v2)
      //      case v: UnaryExpressionPlusPlus      => GS("++") ++ generateExpression(v.v)
      //      case v: UnaryExpressionMinusMinus    => GS("--") ++ generateExpression(v.v)
      //      case v: UnaryExpressionCast          => GS(v.v.toString) ++ generateExpression(v.v2)
      //      case v: UnaryExpressionSizeOf        => GS("sizeof") ++ generateExpression(v.v)
      //      case v: UnaryExpressionSizeOfType    => GS("sizeof", "(") ++ generateTypeName(v.v) ++ GS(")")
      //      case v: UnaryExpressionAlignOf       => GS("_Alignof", "(") ++ generateTypeName(v.v) ++ GS(")")
      //      case v: ArgumentExpressionList       => if (v.v.nonEmpty) {
      //        val it = v.v.iterator
      //        it.next()
      //        generateExpression(v.v.head) ++ it.flatMap(x => GS(",") ++ generateExpression(x))
      //      }
      //      else {
      //        Seq()
      //      }
    }
  }

  def generateExpression(exp: Expression): Seq[Generated] = {
    exp match {
      case v: ExpressionMultiply           => generateExpression(v.v1) ++ generateExpression(v.v2) ++ Seq(imul())
      case v: ExpressionDivision           => generateExpression(v.v1) ++ GS("/") ++ generateExpression(v.v2)
      case v: ExpressionMod                => generateExpression(v.v1) ++ GS("%") ++ generateExpression(v.v2)
      case v: ExpressionAdd                => generateExpression(v.v1) ++ generateExpression(v.v2) ++ Seq(iadd())
      case v: ExpressionMinus              => generateExpression(v.v1) ++ GS("-") ++ generateExpression(v.v2)
      case v: ExpressionLeftShift          => generateExpression(v.v1) ++ GS("<<") ++ generateExpression(v.v2)
      case v: ExpressionRightShift         => generateExpression(v.v1) ++ GS(">>") ++ generateExpression(v.v2)
      case v: ExpressionLessThan           => generateExpression(v.v1) ++ GS("<") ++ generateExpression(v.v2)
      case v: ExpressionGreaterThan        => generateExpression(v.v1) ++ GS(">") ++ generateExpression(v.v2)
      case v: ExpressionLessThanOrEqual    => generateExpression(v.v1) ++ GS("<=") ++ generateExpression(v.v2)
      case v: ExpressionGreaterThanOrEqual => generateExpression(v.v1) ++ GS(">=") ++ generateExpression(v.v2)
      case v: ExpressionEquals             => generateExpression(v.v1) ++ GS("==") ++ generateExpression(v.v2)
      case v: ExpressionNotEquals          => generateExpression(v.v1) ++ GS("!=") ++ generateExpression(v.v2)
      case v: ExpressionAnd                => generateExpression(v.v1) ++ GS("&") ++ generateExpression(v.v2)
      case v: ExpressionXOr                => generateExpression(v.v1) ++ GS("^") ++ generateExpression(v.v2)
      case v: ExpressionInclusiveOr        => generateExpression(v.v1) ++ GS("|") ++ generateExpression(v.v2)
      case v: ExpressionLogicalAnd         => generateExpression(v.v1) ++ GS("&&") ++ generateExpression(v.v2)
      case v: ExpressionLogicalOr          => generateExpression(v.v1) ++ GS("||") ++ generateExpression(v.v2)
      case v: ExpressionConditional        => generateExpression(v.v1) ++ GS("?") ++ generateExpression(v.v2) ++ GS(":") ++ generateExpression(v.v3)
      case v: ExpressionComma              => generateExpression(v.v1) ++ GS(",") ++ generateExpression(v.v2)
      case v: ExpressionAssignment         =>
        generateExpression(v.v1) ++ generateExpression(v.v2) ++ Seq(StoreExpressionInCurrentVar())
      case v: EnumerationConstant          => GS(v.v)
      case v: CharacterConstant            => GS(v.v)
      case v: IntConstant                  =>
        if (v.v <= Byte.MaxValue) Seq(bipush(v.v.toByte))
        else if (v.v <= Short.MaxValue) Seq(sipush(v.v.toShort))
        else {
          assert(false)
          Seq()
        }
      case v: FloatConstant                => GS(v.v.toString)
      case v: StringLiteral                => GS("\"" + v.v + "\"")
      case v: Identifier                   => GS(v.v)
      case v: PostfixExpressionIndex       => generateExpression(v.v1) ++ GS("[") ++ generateExpression(v.v2) ++ GS("]")
      case v: PostfixExpressionMinusMinus  => generateExpression(v.v1) ++ GS("--")
      case v: PostfixExpressionPlusPlus    => generateExpression(v.v1) ++ GS("++")
      case v: PostfixExpressionDot         => generateExpression(v.v1) ++ GS("->") ++ generateExpression(v.v2)
      case v: PostfixExpressionArrow       => generateExpression(v.v1) ++ GS("->") ++ generateExpression(v.v2)
      case v: PostfixExpressionArgs        => generateExpression(v.v1) ++ GS("(") ++ v.v2.map(generateExpression).getOrElse(Seq()) ++ GS(")")
      case v: PostfixExpressionSimple      => generateExpression(v.v1)
      case v: CastExpression               => GS("(") ++ generateTypeName(v.v) ++ GS(")") ++ generateExpression(v.v2)
      case v: UnaryExpressionPlusPlus      => GS("++") ++ generateExpression(v.v)
      case v: UnaryExpressionMinusMinus    => GS("--") ++ generateExpression(v.v)
      case v: UnaryExpressionCast          => GS(v.v.toString) ++ generateExpression(v.v2)
      case v: UnaryExpressionSizeOf        => GS("sizeof") ++ generateExpression(v.v)
      case v: UnaryExpressionSizeOfType    => GS("sizeof", "(") ++ generateTypeName(v.v) ++ GS(")")
      case v: UnaryExpressionAlignOf       => GS("_Alignof", "(") ++ generateTypeName(v.v) ++ GS(")")
      case v: ArgumentExpressionList       => if (v.v.nonEmpty) {
        val it = v.v.iterator
        it.next()
        generateExpression(v.v.head) ++ it.flatMap(x => GS(",") ++ generateExpression(x))
      }
      else {
        Seq()
      }
    }
  }

  def generateTypeName(name: TypeName): Seq[Generated] = {
    GS(name.v)
  }

  def generateGroup(v: Group): Seq[Generated] = v.v.flatMap(x => generateGroupPart(x) ++ Seq(NewlineAndIndent()))

  def generateGroupPart(in: GroupPart): Seq[Generated] = {
    in match {
      case v: IfSection        => generateInGroup(v.ifGroup) ++ v.elif.map(y => y.flatMap(x => generateElifGroup(x))).getOrElse(Seq()) ++ v.elseGroup.map(generateElseGroup).getOrElse(Seq()) ++ generateEndifLine(v.endif)
      case v: Include          => GS("#include") ++ generatePPTokens(v.v)
      case v: Define           => GS("#define") ++ generateIdentifier(v.ident) ++ generateReplacementList(v.v)
      case v: Define2          => GS("#define") ++ generateIdentifier(v.ident) ++ GS("(") ++ v.v.map(x => generateIdentifierList(x)).getOrElse(Seq()) ++ GS(")") ++ generateReplacementList(v.v2)
      case v: Define3          => GS("#define") ++ generateIdentifier(v.ident) ++ GS("(", "...", ")") ++ generateReplacementList(v.v)
      case v: Define4          => GS("#define") ++ generateIdentifier(v.ident) ++ GS("(") ++ generateIdentifierList(v.v) ++ GS(",", "...", ")") ++ generateReplacementList(v.v2)
      case v: Undef            => GS("#undef") ++ generateIdentifier(v.ident)
      case v: Line             => GS("#line") ++ generatePPTokens(v.v)
      case v: Error            => GS("#error") ++ v.v.map(_.flatMap(generatePPToken)).getOrElse(Seq())
      case v: Pragma           => GS("#pragma") ++ v.v.map(_.flatMap(generatePPToken)).getOrElse(Seq())
      case v: ControlLineEmpty => GS("#")
      case v: NonDirective     => GS("#") ++ generatePPTokens(v.pp)
      case v: TextLine         =>
        assert(false)
        Seq()
    }
  }

  def generateInGroup(in: IfGroup): Seq[Generated] = {
    in match {
      case v: If     => GS("#if") ++ generateExpression(v.exp) ++ v.group.map(generateGroup).getOrElse(Seq())
      case v: IfDef  => GS("#ifdef") ++ generateIdentifier(v.ident) ++ v.group.map(generateGroup).getOrElse(Seq())
      case v: IfNDef => GS("#ifndef") ++ generateIdentifier(v.ident) ++ v.group.map(generateGroup).getOrElse(Seq())
    }
  }

  def generateElifGroup(in: ElifGroup): Seq[Generated] = {
    GS("#elif") ++ generateExpression(in.exp) ++ in.group.map(generateGroup).getOrElse(Seq())
  }

  def generateElseGroup(in: ElseGroup): Seq[Generated] = {
    GS("#else") ++ in.group.map(generateGroup).getOrElse(Seq())
  }

  def generateEndifLine(in: EndifLine): Seq[Generated] = {
    GS("#endif")
  }

  def generateNonDirective(in: NonDirective): Seq[Generated] = in.pp.flatMap(generatePPToken)

  def generateReplacementList(in: ReplacementList): Seq[Generated] = in.v.map(_.flatMap(generatePPToken)).getOrElse(Seq())

  def generatePPTokens(in: Seq[PPToken]): Seq[Generated] = in.flatMap(generatePPToken)

  def generatePPToken(in: PPToken): Seq[Generated] = {
    in match {
      case v: HeaderName    => generateHeaderName(v)
      case v: Identifier    => generateIdentifier(v)
      case v: Constant      => generateExpression(v)
      case v: StringLiteral => generateExpression(v)
      case v: Punctuator    => GS("P:", v.v)
    }
  }

  def generateHeaderName(v: HeaderName): Seq[Generated] = {
    GS(if (v.angularBrackets) "<" else "\"", v.v, if (v.angularBrackets) ">" else "\"")
  }

  def generateTop(x: Top): Seq[Generated] = {
    x match {
      case v: ExternalDeclaration => generateExternalDeclaration(v)
      case v: PreprocessingFile   => generateGroup(v.v)
    }
  }
}