package compiling

import compiling.JVMOpCodes._
import jvm.JVMByteCode._
import jvm.{JVMByteCode, JVMClassFileBuilderForWriting}
import parsing.{BlockItem, TranslationUnit, _}

import scala.collection.mutable.ArrayBuffer

// Converts a C AST into JVM bytecode.  It returns a Seq[JVMByteCode.Generated]
class JVMByteCodeGenerator(cf: JVMClassFileBuilderForWriting) {
  @Deprecated
  private val newlineIndent: Seq[Generated] = Seq()
  @Deprecated
  private val endlineIndent: Seq[Generated] = Seq() //Seq(GenString(";"), NewlineAndIndent())

  // Helpers to turn Seq[String] into Generated
  import scala.language.implicitConversions

  private implicit def stringsToGenerated(x: Seq[String]): Seq[Generated] = {
    throw unsupported(s"doing something with ${x}")
    Seq(GenStrings(x))
  }

  private def GS(v: String) = {
    throw unsupported(s"doing something with ${v}")
    Seq(GenString(v))
  }

  private def GS(v: String*) = {
    throw unsupported(s"doing something with ${v}")
    Seq(GenStrings(v))
  }

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

  def resolveDeclaratorToIdentifier(in: Declarator): Identifier = {
    if (in.pointer.isDefined) {
      throw JVMGenUnsupportedCurrently("handle pointers while trying to create identifier")
    }
    in.v match {
      case v: DDBracketed          =>
        throw JVMGenUnsupportedCurrently("handle DDBracketed while trying to create identifier")
      case v: DirectDeclaratorOnly => v.v
      case v: FunctionDeclaration  =>
        throw JVMGenUnsupportedCurrently("handle FunctionDeclaration while trying to create identifier")
    }

  }

  def resolveExpressionToIdentifier(in: Expression): Identifier = {
    in match {
      case v: Identifier => v
      case _ =>
        throw JVMGenUnsupportedCurrently(s"could not get identifier from $in")
    }
  }

  // >>int hello = "world"<<;
  def generateSimpleDeclaration(in: SimpleDeclaration): Seq[Generated] = {

    val types = resolveDeclarationSpecifiersToType(in.spec)

    in.init match {
      case Some(initDeclarators) =>
        // int >>hello<<;
        // int >>hello=3<<;
        initDeclarators.flatMap(initDeclarator => initDeclarator match {
          // int >>hello<<;
          case v: DeclaratorEmpty =>
            val varName = resolveDeclaratorToIdentifier(v.declarator)
            val out: Seq[Generated] = Seq(DeclareVariable(varName, types))
            out

          // int >>hello=3<<;
          case v: DeclaratorWithInit =>
            val varName = resolveDeclaratorToIdentifier(v.declarator)
            val out: Seq[Generated] = Seq(DeclareVariable(varName, types)) ++ generateInitializer(v.init)
            out
        })
      case _                     =>
        throw JVMGenUnsupportedCurrently(s"cannot handle SimpleDeclaration ${in} as no declaration")
    }
  }

  def resolveSimpleDeclarationToVariable(in: SimpleDeclaration): DeclareVariable = {

    val types = resolveDeclarationSpecifiersToType(in.spec)

    in.init match {
      case Some(initDeclarators) =>
        if (initDeclarators.size > 1) {
          throw JVMGenUnsupportedCurrently("got multiple DeclaratorWithInit")
        }
        // int >>hello<<;
        // int >>hello=3<<;
        initDeclarators.head match {
          // int >>hello<<;
          case v: DeclaratorEmpty =>
            val varName = resolveDeclaratorToIdentifier(v.declarator)
            DeclareVariable(varName, types)

          // int >>hello=3<<;
          case v: DeclaratorWithInit =>
            throw JVMGenUnsupportedCurrently("got DeclaratorWithInit in SimpleDeclaration")
        }
      case _                     =>
        throw JVMGenUnsupportedCurrently(s"cannot handle SimpleDeclaration ${in} as no declaration")
    }
  }

  def generateSeqBlockItem(in: Seq[BlockItem]): Seq[Generated] = {
    val out = in.flatMap(v => {
      generateBlockItem(v)
    })

    cf.addFunction(Identifier("fake"), Seq(), JVMTypeVoid(), out)

    out
  }

  def generateTranslationUnit(in: TranslationUnit): Unit = in.v.foreach(v => generateTop(v))

  def resolveDeclarationSpecifiersToType(in: DeclarationSpecifiers): JVMType = {
    var out: Option[JVMType] = None

    in.v.foreach {
      case v: StorageClassSpecifier =>
        throw JVMGenUnsupportedCurrently("StorageClassSpecifier while trying to resolve declaration specifier to type")
      case v: StructImpl            =>
        throw JVMGenUnsupportedCurrently("StructImpl while trying to resolve declaration specifier to type")
      case v: StructUse             =>
        throw JVMGenUnsupportedCurrently("StructUse while trying to resolve declaration specifier to type")
      case v: TypeQualifier         => // We can discard these, only important one is const
      case v: TypeSpecifierVoid     => out = Some(JVMTypeVoid())
      case v: TypeSpecifierChar     => out = Some(JVMTypeChar())
      case v: TypeSpecifierShort    => out = Some(JVMTypeShort())
      case v: TypeSpecifierInt      => out = Some(JVMTypeInt())
      case v: TypeSpecifierLong     => out = Some(JVMTypeLong())
      case v: TypeSpecifierFloat    => out = Some(JVMTypeFloat())
      case v: TypeSpecifierDouble   => out = Some(JVMTypeDouble())
      case v: TypeSpecifierSigned   => // Discard, JVM types are signed
      case v: TypeSpecifierUnsigned =>
        // TODO upgrade to a larger type?
        throw JVMGenUnsupportedCurrently("unsigned types on JVM")
      case v: TypeSpecifierBool     => out = Some(JVMTypeBoolean())
      case v: TypeSpecifierComplex  =>
        throw JVMGenUnsupportedCurrently("complex type")
      case v: TypeSpecifierSimple   =>
        throw JVMGenUnsupportedCurrently("TypeSpecifierSimple while trying to resolve declaration specifier to type")
      case v: FunctionSpecifier     =>
        throw JVMGenUnsupportedCurrently("FunctionSpecifier while trying to resolve declaration specifier to type")
      case v: AlignmentSpecifier    =>
        throw JVMGenUnsupportedCurrently("AlignmentSpecifier while trying to resolve declaration specifier to type")
    }

    if (out.isEmpty) {
      throw JVMGenUnsupportedCurrently(s"could not get type from ${in}")
    }

    out.get
  }


  def generateDeclarationSpecifiers(in: DeclarationSpecifiers): Seq[Generated] = in.v.flatMap(v => generateDeclarationSpecifier(v))

  def resolveDeclarationListToVariables(in: DeclarationList): Seq[DeclareVariable] = {
    in.v.map {
      case x: SimpleDeclaration       => resolveSimpleDeclarationToVariable(x)
      case x: StaticAssertDeclaration => throw JVMGenUnsupportedCurrently("got StaticAssertDeclaration in DeclarationList")
    }
  }

  def unsupported(err: String) = JVMByteCode.unsupported(err)

  def resolveParameterTypeListToVariables(in: ParameterTypeList): Seq[DeclareVariable] = {
    if (in.ellipses) {
      throw unsupported("ellipses in parameter type list")
    }
    in.v.map {
      case x: ParameterDeclarationDeclarator => resolveToVariable(x.v2, x.v)
    }
  }

  def resolveToVariable(declarator: Declarator, specifiers: DeclarationSpecifiers): DeclareVariable = {
    val types = resolveDeclarationSpecifiersToType(specifiers)
    val varName = resolveDeclaratorToIdentifier(declarator)
    JVMByteCode.DeclareVariable(varName, types)
  }

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
      case v: LabelledCase             => GS("case") ++ generateExpression(v.v1) ++ GS(":") ++ generateStatement(v.v2)
      case cs: CompoundStatement       => {
        val blockItems: Seq[Generated] = {
          val it = cs.v.iterator
          val out = ArrayBuffer.empty[Generated]
          while (it.hasNext) {
            out ++= generateBlockItem(it.next())
            if (it.hasNext) {
            }
            else out ++= Seq(GenString(";"))
          }
          //          val out = it.flatMap(x => generateBlockItem(x)) ++ (if (it.hasNext) Seq(GenString(";"), NewlineAndIndent()) else Seq())
          out.toList
        }
        blockItems
      }
      case v: ExpressionStatement      => generateExpression(v.expression) ++ endlineIndent
      case v: SelectionIf              => GS("if", "(") ++ generateExpression(v.v1) ++ GS(")") ++ generateStatement(v.v2)
      case v: SelectionIfElse          => GS("if", "(") ++ generateExpression(v.v1) ++ GS(")") ++ generateStatement(v.v2) ++ GS("else") ++ generateStatement(v.v3)
      case v: SelectionSwitch          => GS("switch", "(") ++ generateExpression(v.v1) ++ GS(")") ++ generateStatement(v.v2)
      case v: ExpressionEmptyStatement => Seq()
      //      case v: StatementDeclaration =>
      case v: Return          =>
        v.v match {
          case Some(exp) =>
            val simple = convertExpressionToSimple(exp).toShort
            if (simple < 256) {
              Seq(makeInt(bipush, simple), make(ireturn))
            }
            else {
              Seq(makeInt(sipush, simple), make(ireturn))
            }

          case _ => Seq(make(ret))
        }
      case v: LabelledDefault => GS("default:") ++ generateStatement(v.v2)
    }
  }

  def generateInitializer(in: Initializer): Seq[Generated] = {
    in match {
      case v: InitializerSimple =>
        generateExpression(v.exp) ++ Seq(make(istore_0))
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
    generateSpecifierQualifierList(in.v) ++ in.v2.map(x => generateStructDeclaratorList(x)).getOrElse(Seq())
  }

  def generateIdentifier(in: Identifier): Seq[Generated] = {
    GS(in.v)
  }

  def generateExternalDeclaration(in: ExternalDeclaration): Unit = {
    in match {
      case v: FunctionDefinition => generateFunctionDefinition(v)
      case v: Declaration        =>
        throw unsupported("Declaration at ExternalDeclaration")
        generateDeclaration(v)
    }
  }

  def generateFunctionDefinition(in: FunctionDefinition): Unit = {
    val typ = resolveDeclarationSpecifiersToType(in.spec)
    //    val varName = resolveDeclaratorToIdentifier(in.dec).v

    if (in.dec.pointer.isDefined) {
      throw JVMGenUnsupportedCurrently("handle pointers while trying to declare function")
    }
    in.dec.v match {
      case v: DDBracketed          => throw JVMGenUnsupportedCurrently("handle DDBracketed while trying to declare function")
      case v: DirectDeclaratorOnly => throw JVMGenUnsupportedCurrently("handle DDBracketed while trying to declare function")
      case v: FunctionDeclaration  =>
        //        val variables: Seq[DeclareVariable] = in.decs match {
        //          case Some(declarationList) => resolveDeclarationListToVariables(declarationList)
        //          case _                     => Seq()
        //        }

        val ret = resolveDeclarationSpecifiersToType(in.spec)
        val name = v.name
        val variables = resolveParameterTypeListToVariables(v.params)
        val definition = generateStatement(in.v)

        if (name.v == "main") {
          val methodRefIdx = cf.addFunction(Identifier("__" + name.v + "Original"), variables, ret, definition)

          cf.addFunction(Identifier("main"),
            //            Seq(DeclareVariable(Identifier("args"), JVMTypeInt())),
            Seq(DeclareVariable(Identifier("args"), JVMTypeArray(JVMTypeString()))),
            JVMTypeVoid(),
            Seq(
              makeInt(invokestatic, methodRefIdx),
              JVMOpCodeWithArgs(return_)
            )
          )
        }
        else {
          cf.addFunction(name, variables, ret, definition)
        }

    }
  }

  def generateIdentifierList(in: Seq[Identifier]): Seq[Generated] = {
    val it = in.iterator
    generateIdentifier(it.next()) ++ it.flatMap(x => GS(",") ++ generateIdentifier(x))
  }

  def generateDeclarationSpecifier(in: DeclarationSpecifier): Seq[Generated] = {
    in match {
      case v: StorageClassSpecifier => GS(v.v)
      case v: StructImpl            => GS(if (v.isStruct) "struct" else "union") ++ v.id.map(generateIdentifier).getOrElse(Seq()) ++ generateStructDeclarationList(v.v2)
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
      case v: ExpressionMultiply           => generateExpression(v.v1) ++ generateExpression(v.v2) ++ Seq(make(imul))
      case v: ExpressionDivision           => generateExpression(v.v1) ++ GS("/") ++ generateExpression(v.v2)
      case v: ExpressionMod                => generateExpression(v.v1) ++ GS("%") ++ generateExpression(v.v2)
      case v: ExpressionAdd                => generateExpression(v.v1) ++ generateExpression(v.v2) ++ Seq(make(iadd))
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
        generateExpression(v.v1) ++ generateExpression(v.v2) ++ Seq(make(istore_0))
      case v: EnumerationConstant          => GS(v.v)
      case v: CharacterConstant            => GS(v.v)
      case v: IntConstant                  =>
        if (v.v <= Byte.MaxValue && v.v >= Byte.MinValue) Seq(makeInt(bipush, v.v))
        else if (v.v <= Short.MaxValue && v.v >= Short.MinValue) Seq(makeInt(sipush, v.v))
        else {
          // TODO longer ints
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
      case v: PostfixExpressionArgs        =>
        val funcName = resolveExpressionToIdentifier(v.v1)
        if (funcName == "printf") {

        }
//        make(invokestatic, )
        generateExpression(v.v1) ++ GS("(") ++ v.v2.map(generateExpression).getOrElse(Seq()) ++ GS(")")
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

  def generateGroup(v: Group): Seq[Generated] = v.v.flatMap(x => generateGroupPart(x))

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

  def generateTop(x: Top): Unit = {
    x match {
      case v: ExternalDeclaration => generateExternalDeclaration(v)
      case v: PreprocessingFile   => generateGroup(v.v)
    }
  }
}
