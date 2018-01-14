package parser

import fastparse.WhitespaceApi


// https://port70.net/~nsz/c/c11/n1570.html#A
// Removed left recursion
class CParser {

  // Whitespace sensitive parsers go here
  private[parser] val identifier: fastparse.all.Parser[Identifier] = {
    import fastparse.all._
    val digit = P(CharIn('0' to '9')).!.map(v => Digit(v.charAt(0)))
    val hexadecimalDigit =  P(CharIn('0' to '9') | CharIn('a' to 'f') | CharIn('A' to 'F')).!.map(v => HexDigit(v.charAt(0)))
    val hexQuad = P(hexadecimalDigit ~ hexadecimalDigit~ hexadecimalDigit~ hexadecimalDigit).map(v => HexQuad(v._1, v._2, v._3, v._4))
    val universalCharacterName = P(P("\\u") ~ hexQuad).map(v => UniversalCharacterName1(v)) | P("\\U" ~ hexQuad ~ hexQuad).map(v => UniversalCharacterName2(v._1, v._2))
    val nondigit = P(CharIn('a' to 'z') | CharIn('A' to 'Z') | "_").!.map(v => Nondigit(v.charAt(0)))
    val identifierNondigit = nondigit.map(v => IdentifierNondigit1(v)) |
      universalCharacterName.map(v => IdentifierNondigit2(v))
    P(nondigit ~ !" " ~ (nondigit | universalCharacterName | digit).rep(0)).!.opaque("identifier").map(Identifier(_))
  }

  private[parser] val decimalConstant = {
    import fastparse.all._
    P(CharIn('1' to '9') ~ CharIn('0' to '9').rep(0)).!.map(v =>
      IntConstant(Integer.parseInt(v, 10)))
  }
  private[parser] val octalConstant = {
    import fastparse.all._
    P("0" ~ CharIn('0' to '7').rep(0)).!.map(v =>
      IntConstant(Integer.parseInt(v, 8)))
  }

  // Ignore whitespace
  private val White = WhitespaceApi.Wrapper{
    import fastparse.all._
    NoTrace(CharIn(" \t\n").rep | P("\r\n").rep)
  }
  import White._
  import fastparse.noApi._


  // Whitespace in-sensitive parsers go here

  // http://c0x.coding-guidelines.com/6.4.2.1.html
  private[parser]  lazy val keyword = P(P("auto ∗") | P("break") | P("case") | P("char") | P("const") | P("continue") | P("default") | P("do") | P("double") | P("else")|
    P("enum") | P("extern") | P("float") | P("for") | P("goto") | P("if") | P("inline") | P("int") | P("long") | P("register") | P("restrict")|
    P("return") | P("short") | P("signed") | P("sizeof") | P("static") | P("struct") | P("switch") | P("typedef") | P("union") | P("unsigned")|
    P("void") | P("volatile") | P("while") | P("_Alignas") | P("_Alignof") | P("_Atomic") | P("_Bool") | P("_Complex") | P("_Generic")|
    P("_Imaginary") | P("_Noreturn") | P("_Static_assert") | P("_Thread_local")).!.map(v => Keyword(v))


  private[parser]  lazy val constant: Parser[Constant] = P(integerConstant | floatingConstant).opaque("constant")
  private[parser]  lazy val integerConstant: Parser[IntConstant] =
    P(P(decimalConstant ~ integerSuffix.? | octalConstant ~ integerSuffix.? | hexadecimalConstant ~ integerSuffix.?))
      .opaque("integerConstant")
  private[parser] lazy val hexadecimalPrefix = P("0x") | P("0X")
  private[parser]  lazy val hexadecimalConstant =
    P(hexadecimalPrefix ~ CharIn("0123456789abcdef").rep(1)).!.map(v =>
      IntConstant(Integer.parseInt(v, 16)))
  private[parser]  lazy val nonzeroDigit = P(CharIn('1' to '9')).!.map(v => Digit(v.charAt(0)))
  //  private[parser]  lazy val octalDigit = P(CharIn('0' to '7'))
  private[parser]  lazy val integerSuffix = P((CharIn("uU") ~ P(P("ll") | P("LL") | P("l") | P("L"))) |
    ((P("ll") | P("LL") | P("l") | P("L")) ~ P(CharIn("uU")).?))
  //  private[parser]  lazy val unsignedSuffix = P(CharIn("uU"))
  //  private[parser]  lazy val longSuffix = P(CharIn("lL"))
  //  private[parser]  lazy val longLongSuffix = P("ll" | "LL")
  private[parser]  lazy val floatingConstant: Parser[FloatConstant] = P(decimalFloatingConstant | hexadecimalFloatingConstant)
  private[parser]  lazy val decimalFloatingConstant: Parser[FloatConstant] = P(P(fractionalConstant ~ exponentPart.? ~ floatingSuffix.?).map(v => FloatConstant(v._1.v * v._2.map(_.calc()).getOrElse(1.0f))) |
    P(digitSequence.!.map(v => {
      v.toInt
    }) ~ exponentPart ~ floatingSuffix.?).map(v => FloatConstant(v._1 * v._2.calc())))
  private[parser] lazy val hexadecimalFloatingConstant: Parser[FloatConstant] =
    P(hexadecimalPrefix ~ hexadecimalFractionalConstant ~ binaryExponentPart ~ floatingSuffix.?).map(v =>
      FloatConstant(v._1.v * v._2.calc())) |
      P(hexadecimalPrefix ~ hexadecimalDigitSequence ~ binaryExponentPart ~ floatingSuffix.?).map(v =>
        FloatConstant(v._1.v * v._2.calc()))
  private[parser] lazy val fractionalConstant = P(digitSequence.? ~ P(".") ~ digitSequence).!.map(v => FloatConstant(v.toFloat))
  private[parser] lazy val exponentPart: Parser[Exponent] =
    (P("e" | P("E")) ~ CharIn("+-").? ~ digitSequence).!.map(v => {
      Exponent(v.stripPrefix("e").stripPrefix("E").toInt)})
  private[parser] lazy val digitSequence = P(CharIn("0123456789").rep(1))
  private[parser] lazy val hexadecimalFractionalConstant: Parser[FloatConstant] =
    P(hexadecimalDigitSequence.?.! ~ P(".").! ~ hexadecimalDigitSequence.!).map(v =>
      FloatConstant(v._1.toFloat + Math.pow(v._3.toFloat, v._3.size * 1).toFloat))
  private[parser] lazy val binaryExponentPart: Parser[Exponent] =
    (P("p" | P("P")) ~ CharIn("+-").? ~ digitSequence).!.map(v =>
      Exponent(v.stripPrefix("p").stripPrefix("P").toInt))
  private[parser] lazy val hexadecimalDigitSequence: Parser[FloatConstant] =
    P(CharIn("0123456789abcdef").rep(1)).!.map(v => FloatConstant(v.toFloat))
  private[parser] lazy val floatingSuffix = P(CharIn("flFL"))


  private[parser] lazy val enumerationConstant = identifier.map(v => EnumerationConstant(v.v))
  private[parser] lazy val cCharSequence = CharsWhile(v => v != '\'' && v != '\\' && v != '\n')
  private[parser] lazy val characterConstant:Parser[CharacterConstant] =
    P(CharIn("LuU").? ~ P("\'") ~ cCharSequence ~ "\'").!.map(v =>
      CharacterConstant(v.stripPrefix("\"").stripSuffix("\"")))
  //  private[parser] lazy val escapeSequence = simpleEscapeSequence
  //  octalEscapeSequence
  //  hexadecimalEscapeSequence
  //  universalCharacterName
  //  private[parser] lazy val simpleEscapeSequence: one of
  //    \' \" \? \\
  //  \a \b \f \n \r \t \v
  //  private[parser] lazy val octalEscapeSequence = \ octalDigit
  //    \ octalDigit octalDigit
  //  \ octalDigit octalDigit octalDigit
  //  private[parser] lazy val hexadecimalEscapeSequence = \x hexadecimalDigit
  //  hexadecimalEscapeSequence hexadecimalDigit
  //    A.1.6 String literals
  private[parser] lazy val stringLiteral = P(encodingPrefix.? ~ CharIn("\"") ~ CharsWhile(v => v != '"' && v != '\\' && v != '\n') ~ CharIn("\"")).!.map(v =>
    StringLiteral(v.stripPrefix("u8").stripPrefix("u").stripPrefix("U").stripPrefix("L").stripPrefix("\"").stripSuffix("\"")))
  private[parser] lazy val encodingPrefix = P("u8" | CharIn("uUL"))
  private[parser] lazy val punctuator: Parser[Punctuator] =
    P(P("[") | P("|") | P("]") | P("(") | P(")") | P("{") | P("}") | P(".") | P("->") | P("++") | P("--") | P("&") |
      P("*") | P("+") | P("-") | P("~") | P("!") | P("/") | P("%") | P("<<") | P(">>") | P("<") | P(">") | P("<=") |
      P(">=") | P("==") | P("!=") | P("^") | P("|") | P("&&") | P("||") | P("?") | P(":") | P(";") | P("...") | P("=") |
      P("*=") | P("/=") | P("%=") | P("+=") | P("-=") | P("<<=") | P(">>=") | P("&=") | P("^=") | P("|=") | P(",") |
      P("#") | P("##") | P("<:") | P(":>") | P("<%") | P("%>") | P("%:") | P("%:%:")).!.map(v => Punctuator(v))

  private[parser]  lazy val token: Parser[Token] = keyword | identifier | constant | stringLiteral | punctuator

  private[parser] lazy val headerName: Parser[HeaderName] = (P(P("<") ~ CharsWhile(v => v != '\n' && v != '>') ~ P(">")) |
    P(P("\"") ~ CharsWhile(v => v != '\n' && v != '"') ~ P("\""))).!.map(v => {
    HeaderName(v.substring(1, v.size - 1))
  })
  //  private[parser] lazy val ppNumber = digit
  //    . digit
  //  ppNumber digit
  //    ppNumber identifierNondigit
  //    ppNumber e sign
  //  ppNumber E sign
  //  ppNumber p sign
  //  ppNumber P sign
  //  ppNumber .

  //  private[parser] lazy val primaryExpression: Parser[Expression] = P(identifier | constant | stringLiteral | P(P("(") ~ expression ~ P(")")) | genericSelection)
  private[parser] lazy val primaryExpression: Parser[Expression] =
    P(identifier | constant | stringLiteral | P(P("(") ~ expression ~ P(")"))).opaque("primaryExpression")

  // Never seen this
  //  private[parser] lazy val genericSelection: Parser[GenericSelection] = P("_Generic") ~ P("(") ~ assignmentExpression ~ P(",") ~ genericAssocList ~ P(")")
  //  private[parser] lazy val genericAssocList: Parser[Any] = genericAssociation | P(genericAssocList ~ P(",") ~ genericAssociation)
  //  private[parser] lazy val genericAssociation = P(typeName ~ P(":") ~ assignmentExpression) |
  //    P(P("default") ~ P(":") ~ assignmentExpression)

  private[parser] lazy val postfixExpression: Parser[Expression] =
    P(primaryExpression ~ postfixExpressionR).map(v =>
      postfixRecurse(v)).opaque("postfixExpression")

  private def postfixRecurse(v: (Expression, PostfixRight2)): Expression = {
    val exp = postfixMerge(v._1, v._2.op)
    if (v._2.next == null) {
      v._1
    }
    else {
      v._2.next.op match {
        case _: Empty => exp
        case _ => postfixRecurse(exp, v._2.next)
      }
    }
  }

  private[parser] def postfixMerge(left: Expression, v: PostfixRight): Expression = {
    val exp: Expression = v match {
      case x: PostfixRightIndex => PostfixExpressionIndex(left, x.v1)
      case x: PostfixRightDot => PostfixExpressionDot(left, x.v1)
      case x: PostfixRightPlusPlus => PostfixExpressionPlusPlus(left)
      case x: PostfixRightMinusMinus => PostfixExpressionMinusMinus(left)
      case x: PostfixRightArrow => PostfixExpressionArrow(left, x.v1)
      case x: PostfixRightArgs => PostfixExpressionArgs(left, x.v2)
      case x: Empty => x
    }
    exp
  }

  private[parser] lazy val postfixExpressionR: Parser[PostfixRight2] =
    P(P("[") ~ expression ~ P("]") ~ postfixExpressionR).map(v => PostfixRight2(PostfixRightIndex(v._1), v._2)) |
      //    P(P("[") ~ multiplicativeExpression ~ P("]") ~ postfixExpressionR).map(v => PostfixRight2(PostfixRightIndex(v._1), v._2)) |
      P(P("(") ~ argumentExpressionList.? ~ P(")") ~ postfixExpressionR).map(v => PostfixRight2(PostfixRightArgs(v._1), v._2)) |
      P(P(".") ~ identifier ~ postfixExpressionR).map(v => PostfixRight2(PostfixRightDot(v._1), v._2)) |
      P(P("->") ~ identifier ~ postfixExpressionR).map(v => PostfixRight2(PostfixRightArrow(v._1), v._2)) |
      P(P("++") ~ postfixExpressionR).map(v => PostfixRight2(PostfixRightPlusPlus(), v)) |
      P(P("--") ~ postfixExpressionR).map(v => PostfixRight2(PostfixRightMinusMinus(), v)) |
      P("").map(v => PostfixRight2(Empty(), null))

  // Can't figure this one out
  // P(P("(") ~ typeName ~ P(")") ~ P("{") ~ initializerList ~ P(",").? ~ P("}"))
  private[parser] lazy val argumentExpressionList: Parser[ArgumentExpressionList] =
  P(assignmentExpression ~ P(P(",") ~ assignmentExpression).rep(0)).map(v =>
    //  P(unaryExpression ~ P(P(",") ~ unaryExpression).rep(0)).map(v =>
    ArgumentExpressionList((v._1 +: v._2).toList))

  //  private[parser] lazy val unaryExpression: Parser[Expression] = P(postfixExpression ~ unaryExpressionBuild).map(v => unaryRecurse(v))


  private[parser] lazy val unaryExpression: Parser[Expression] =
    P(P(P("++") ~ unaryExpression).map(v => UnaryExpressionPlusPlus(v)) |
      P(P("--") ~ unaryExpression).map(v => UnaryExpressionMinusMinus(v)) |
      P(unaryOperator.! ~ castExpression).map(v => UnaryExpressionCast(v._1.charAt(0), v._2)) |
      P(P("sizeof") ~ unaryExpression).map(v => UnaryExpressionSizeOf(v)) |
      P(P("sizeof") ~ P("(") ~ typeName ~ P(")")).map(v => UnaryExpressionSizeOfType(v)) |
      P(P("_Alignof") ~ P("(") ~ typeName ~ P(")")).map(v => UnaryExpressionAlignOf(v)) |
      postfixExpression).opaque("unaryExpression")

  private[parser] lazy val unaryOperator = CharIn("&*+-~!")

  private[parser] lazy val castExpression: Parser[Expression] =
    P(P(P("(") ~ typeName ~ P(")") ~ castExpression).map(v => CastExpression(v._1, v._2)) |
      unaryExpression).opaque("castExpression")

  private[parser] lazy val multiplicativeExpression: Parser[Expression] =
    P(castExpression ~ multiplicativeExpressionHelper).map(v => binary(v._1,v._2)).opaque("multiplicativeExpression")
  private[parser] lazy val multiplicativeExpressionHelper: Parser[BinaryOpBuildWrap] =
    P(P("*").! ~ multiplicativeExpression).map(v => BinaryOpBuildWrap(v._1, v._2)) |
      P(P("/").! ~ multiplicativeExpression).map(v => BinaryOpBuildWrap(v._1, v._2)) |
      P(P("%").! ~ multiplicativeExpression).map(v => BinaryOpBuildWrap(v._1, v._2)) |
      P("").map(v => BinaryOpBuildWrap(" ", null))

  // These methods help us glue full expressions back together, after they had to be split apart during left recursion removal
  private def binary(left: Expression, right: BinaryOpBuildWrap): Expression = {
    right.op match {
      case "*" => ExpressionMultiply(left, right.next)
      case "/" => ExpressionDivision(left, right.next)
      case "%" => ExpressionMod(left, right.next)
      case "+" => ExpressionAdd(left, right.next)
      case "-" => ExpressionMinus(left, right.next)
      case "<<" => ExpressionLeftShift(left, right.next)
      case ">>" => ExpressionRightShift(left, right.next)
      case "<" => ExpressionLessThan(left, right.next)
      case ">" => ExpressionGreaterThan(left, right.next)
      case "<=" => ExpressionLessThanOrEqual(left, right.next)
      case ">=" => ExpressionGreaterThanOrEqual(left, right.next)
      case "==" => ExpressionEquals(left, right.next)
      case "!=" => ExpressionNotEquals(left, right.next)
      case "&" => ExpressionAnd(left, right.next)
      case "^" => ExpressionXOr(left, right.next)
      case "|" => ExpressionInclusiveOr(left, right.next)
      case "&&" => ExpressionLogicalAnd(left, right.next)
      case "||" => ExpressionLogicalOr(left, right.next)
      case "=" => ExpressionAssignment(left, right.next)
      case "*=" => ExpressionAssignment(left, ExpressionMultiply(left, right.next))
      case "/=" => ExpressionAssignment(left, ExpressionDivision(left, right.next))
      case "%=" => ExpressionAssignment(left, ExpressionMod(left, right.next))
      case "+=" => ExpressionAssignment(left, ExpressionAdd(left, right.next))
      case "-=" => ExpressionAssignment(left, ExpressionMinus(left, right.next))
      case "<<=" => ExpressionAssignment(left, ExpressionLeftShift(left, right.next))
      case ">>=" => ExpressionAssignment(left, ExpressionRightShift(left, right.next))
      case "&=" => ExpressionAssignment(left, ExpressionAnd(left, right.next))
      case "^=" => ExpressionAssignment(left, ExpressionXOr(left, right.next))
      case "|=" => ExpressionAssignment(left, ExpressionInclusiveOr(left, right.next))
      case "," => ExpressionComma(left, right.next)
      case "" | " " => left
      case _ =>
        assert(false)
        left
    }
  }

  private def ternary(left: Expression, right: TernaryOpBuildWrap): Expression = {
    right.op1 match {
      case "?" => ExpressionConditional(left, right.v1, right.v2)
      case _ => left
    }
  }

  //  private def binary2(left: Expression, right: BinaryOpBuildWrap2): Expression = {
  //    val exp = binary2(left, right.next)
  //    right.op match {
  //      case "*" => ExpressionMultiply(left, exp)
  //      case "/" => ExpressionDivision(left, exp)
  //      case "%" => ExpressionMod(left, exp)
  //      case _ => left
  //    }
  //  }

  private[parser] lazy val additiveExpression: Parser[Expression] =
    (multiplicativeExpression ~ additiveExpressionHelper).map(v => binary(v._1, v._2)).opaque("additiveExpression")
  private[parser] lazy val additiveExpressionHelper: Parser[BinaryOpBuildWrap] =
    P(P("+").! ~ additiveExpression).map(v => BinaryOpBuildWrap(v._1, v._2)) |
      P(P("-").! ~ additiveExpression).map(v => BinaryOpBuildWrap(v._1, v._2)) |
      P("").map(v => BinaryOpBuildWrap(" ", null))

  private[parser] lazy val shiftExpression: Parser[Expression] =
    (additiveExpression ~ shiftExpressionHelper).map(v => binary(v._1, v._2)).opaque("shiftExpression")
  private[parser] lazy val shiftExpressionHelper: Parser[BinaryOpBuildWrap] =
    P(P("<<").! ~ shiftExpression).map(v => BinaryOpBuildWrap(v._1, v._2)) |
      P(P(">>").! ~ shiftExpression).map(v => BinaryOpBuildWrap(v._1, v._2)) |
      P("").map(v => BinaryOpBuildWrap(" ", null))

  private[parser] lazy val relationalExpression: Parser[Expression] =
    (shiftExpression ~ relationalExpressionHelper).map(v => binary(v._1, v._2)).opaque("relationalExpression")
  private[parser] lazy val relationalExpressionHelper: Parser[BinaryOpBuildWrap] =
    P(P("<").! ~ relationalExpression).map(v => BinaryOpBuildWrap(v._1, v._2)) |
      P(P(">").! ~ relationalExpression).map(v => BinaryOpBuildWrap(v._1, v._2)) |
      P(P("<=").! ~ relationalExpression).map(v => BinaryOpBuildWrap(v._1, v._2)) |
      P(P(">=").! ~ relationalExpression).map(v => BinaryOpBuildWrap(v._1, v._2)) |
      P("").map(v => BinaryOpBuildWrap(" ", null))

  private[parser] lazy val equalityExpression: Parser[Expression] =
    (relationalExpression ~ equalityExpressionHelper).map(v => binary(v._1, v._2)).opaque("equalityExpression")
  private[parser] lazy val equalityExpressionHelper: Parser[BinaryOpBuildWrap] =
    P(P("==").! ~ equalityExpression).map(v => BinaryOpBuildWrap(v._1, v._2)) |
      P(P("!=").! ~ equalityExpression).map(v => BinaryOpBuildWrap(v._1, v._2)) |
      P("").map(v => BinaryOpBuildWrap(" ", null))

  private[parser] lazy val ANDExpression: Parser[Expression] =
    (equalityExpression ~ ANDExpressionHelper).map(v => binary(v._1, v._2)).opaque("ANDExpression")
  private[parser] lazy val ANDExpressionHelper: Parser[BinaryOpBuildWrap] =
    P(P("&").! ~ ANDExpression).map(v => BinaryOpBuildWrap(v._1, v._2)) |
      P("").map(v => BinaryOpBuildWrap(" ", null))

  private[parser] lazy val exclusiveORExpression: Parser[Expression] =
    (ANDExpression ~ exclusiveORExpressionHelper).map(v => binary(v._1, v._2))
  private[parser] lazy val exclusiveORExpressionHelper: Parser[BinaryOpBuildWrap] =
    P(P("^").! ~ exclusiveORExpression).map(v => BinaryOpBuildWrap(v._1, v._2)) |
      P("").map(v => BinaryOpBuildWrap(" ", null))

  private[parser] lazy val inclusiveORExpression: Parser[Expression] =
    (exclusiveORExpression ~ inclusiveORExpressionHelper).map(v => binary(v._1, v._2))
  private[parser] lazy val inclusiveORExpressionHelper: Parser[BinaryOpBuildWrap] =
    P(P("|").! ~ inclusiveORExpression).map(v => BinaryOpBuildWrap(v._1, v._2)) |
      P("").map(v => BinaryOpBuildWrap(" ", null))

  private[parser] lazy val logicalANDExpression: Parser[Expression] =
    (inclusiveORExpression ~ logicalANDExpressionHelper).map(v => binary(v._1, v._2))
  private[parser] lazy val logicalANDExpressionHelper: Parser[BinaryOpBuildWrap] =
    P(P("&&").! ~ logicalANDExpression).map(v => BinaryOpBuildWrap(v._1, v._2)) |
      P("").map(v => BinaryOpBuildWrap(" ", null))

  private[parser] lazy val logicalORExpression: Parser[Expression] =
    (logicalANDExpression ~ logicalORExpressionHelper).map(v => binary(v._1, v._2)).opaque("logicalORExpression")
  private[parser] lazy val logicalORExpressionHelper: Parser[BinaryOpBuildWrap] =
    P(P("||").! ~ logicalORExpression).map(v => BinaryOpBuildWrap(v._1, v._2)) |
      P("").map(v => BinaryOpBuildWrap(" ", null))

  private[parser] lazy val conditionalExpression: Parser[Expression] =
    (logicalORExpression ~ conditionalExpressionHelper).map(v => ternary(v._1, v._2)).opaque("conditionalExpression")
  private[parser] lazy val conditionalExpressionHelper: Parser[TernaryOpBuildWrap] =
    P(P("?") ~ expression ~ P(":") ~ conditionalExpression).map(v => TernaryOpBuildWrap("?", ":", v._1, v._2)) |
      P("").map(v => TernaryOpBuildWrap(" ", " ", null, null))

  private[parser] lazy val assignmentExpression: Parser[Expression] =
    (conditionalExpression ~ assignmentExpressionHelper).map(v => binary(v._1, v._2)).opaque("assignmentExpression")
  private[parser] lazy val assignmentExpressionHelper: Parser[BinaryOpBuildWrap] =
    P(assignmentOperator.! ~ assignmentExpression).map(v => BinaryOpBuildWrap(v._1, v._2)) |
      P("").map(v => BinaryOpBuildWrap(" ", null))
  private[parser] lazy val assignmentOperator = P("=") | P("*=") | P("/=") | P("%=") | P("+=") | P("-=") | P("<<=") | P(">>=") | P("&=") | P("^=") | P("|=")

  private[parser] lazy val expression: Parser[Expression] =
    (assignmentExpression ~ expressionHelper).map(v => binary(v._1, v._2)).opaque("expression")
  private[parser] lazy val expressionHelper: Parser[BinaryOpBuildWrap] =
    P(P(P(",").! ~ assignmentExpression).map(v => BinaryOpBuildWrap(v._1, v._2)) |
      P("").map(v => BinaryOpBuildWrap(" ", null))).opaque("expressionHelper")

  private[parser] lazy val constantExpression: Parser[Expression] = conditionalExpression

  private[parser] lazy val declaration: Parser[Declaration] = P(P(declarationSpecifiers ~ initDeclaratorList.? ~ P(";") |
    static_assertDeclaration).!.map(v => Declaration(v))).opaque("declaration")
  private[parser] lazy val declarationSpecifier: Parser[DeclarationSpecifier] =
    P(storageClassSpecifier | typeSpecifier | typeQualifier | functionSpecifier | alignmentSpecifier).opaque("declarationSpecifier")
  private[parser] lazy val declarationSpecifiers: Parser[DeclarationSpecifiers] = declarationSpecifier.rep(1).map(v => DeclarationSpecifiers(v.toList)).opaque("declarationSpecifiers")
  private[parser] lazy val initDeclaratorList: Parser[Any] = P(initDeclarator ~ P(P(",") ~ initDeclarator).rep(0)).opaque("initDeclarationList")
  private[parser] lazy val initDeclarator: Parser[Any] = declarator |
    P(declarator ~ P("=") ~ initializer)
  private[parser] lazy val storageClassSpecifier: Parser[StorageClassSpecifier] =
    P(P("typedef") | P("extern") | P("static") | P("_Thread_local") | P("auto") | P("register")).!.map(StorageClassSpecifier)
  //  private[parser] lazy val typeSpecifier: Parser[TypeSpecifier] = (P("void") | P("char") | P("short") | P("int") | P("long") | P("float") | P("double") | P("signed") | P("unsigned") | P("_Bool") | P("_Complex") | atomicTypeSpecifier | structOrUnionSpecifier | enumSpecifier | typedefName).!.map(TypeSpecifier)
  // TODO
  private[parser] lazy val typeSpecifier: Parser[TypeSpecifier] = (P("void") | P("char") | P("short") | P("int") | P("long") | P("float") | P("double") | P("signed") | P("unsigned") | P("_Bool") | P("_Complex") | atomicTypeSpecifier | structOrUnionSpecifier | enumSpecifier).!.map(TypeSpecifier)
  private[parser] lazy val structOrUnionSpecifier = P(structOrUnion ~ identifier.? ~ P("{") ~ structDeclarationList ~ P("}")) |
    P(structOrUnion ~ identifier)
  private[parser] lazy val structOrUnion = P("struct") | P("union")
  private[parser] lazy val structDeclarationList: Parser[Any] = structDeclaration |
    P(structDeclarationList ~ structDeclaration)
  private[parser] lazy val structDeclaration = P(specifierQualifierList ~ structDeclaratorList.? ~ P(";")) |
    static_assertDeclaration
  private[parser] lazy val specifierQualifierList: Parser[Any] = P(typeSpecifier ~ specifierQualifierList.?) |
    P(typeQualifier ~ specifierQualifierList.?)
  private[parser] lazy val structDeclaratorList: Parser[Any] = structDeclarator |
    P(structDeclaratorList ~ P(",") ~ structDeclarator)
  private[parser] lazy val structDeclarator = declarator |
    P(declarator.? ~ P(":") ~ constantExpression)
  private[parser] lazy val enumSpecifier = P(P("enum") ~ identifier.? ~ P("{") ~ enumeratorList ~ P("}") ~ P(",").?) |
    P(P("enum") ~ identifier)
  private[parser] lazy val enumeratorList: Parser[Any] = enumerator |
    P(enumeratorList ~ P(",") ~ enumerator)
  private[parser] lazy val enumerator = enumerationConstant |
    P(enumerationConstant ~ P("=") ~ constantExpression)
  private[parser] lazy val atomicTypeSpecifier = P("_Atomic") ~ P("(") ~ typeName ~ P(")")
  private[parser] lazy val typeQualifier: Parser[TypeQualifier] = (P("const") | P("restrict") | P("volatile") | P("_Atomic")).!.map(TypeQualifier)
  private[parser] lazy val functionSpecifier: Parser[FunctionSpecifier] = (P("inline") | P("_Noreturn")).!.map(FunctionSpecifier)
  private[parser] lazy val alignmentSpecifier: Parser[AlignmentSpecifier] = P("_Alignas").!.map(AlignmentSpecifier) |
    P(P("(") ~ typeName ~ P(")")).!.map(AlignmentSpecifier) |
    P(P("_Alignas") ~ P("(") ~ constantExpression ~ P(")")).!.map(AlignmentSpecifier)

  // "main(int argc)"
  // "someFunc(hello, world)"
  private[parser] lazy val declarator: Parser[Declarator] = (pointer.!.? ~ directDeclarator).opaque("declarator").log().map(v => Declarator(v._1, v._2))
  private[parser] lazy val directDeclarator: Parser[DirectDeclarator] = P(P(P("(") ~ declarator ~ P(")")).map(DDBracketed) |
    (identifier ~ directDeclaratorHelper).map(v => {
      directDeclaratorRecurse(v._1, v._2)
    })).opaque("directDeclarator").log()
  private[parser] lazy val directDeclaratorHelper: Parser[DDBuild2] =
    P(
      P(P("[") ~ typeQualifierList.? ~ assignmentExpression.? ~ P("]") ~ directDeclaratorHelper).map(v => DDBuild2(DDTypeQualifierListAssignment(v._1, v._2), v._3)) |
        P(P("[") ~ P("static") ~ typeQualifierList.? ~ assignmentExpression ~ P("]") ~ directDeclaratorHelper).map(v => DDBuild2(DDTypeQualifierListAssignment(v._1, Some(v._2)), v._3)) |
        P(P("[") ~ typeQualifierList ~ P("static") ~ assignmentExpression ~P("]") ~ directDeclaratorHelper).map(v => DDBuild2(DDTypeQualifierListAssignment(Some(v._1), Some(v._2)), v._3)) |
        P(P("[") ~ typeQualifierList.? ~P("*") ~ P("]") ~ directDeclaratorHelper).log().map(v => DDBuild2(DDTypeQualifierList(v._1), v._2)) |
        P(P("(") ~ parameterTypeList ~ P(")") ~ directDeclaratorHelper).log().map(v => DDBuild2(DDParameterTypeList(v._1), v._2)) |
        P(P("(") ~ identifierList.? ~ P(")") ~ directDeclaratorHelper).log().map(v => DDBuild2(DDIdentifierList(v._1), v._2)) |
        P("").log().map(v => DDBuild2(Empty(), null))
    ).opaque("directDeclaratorHelper").log()

  private def directDeclaratorRecurse(identifier: Identifier, right: DDBuild2): DirectDeclarator = {
    // Just ignore right for now, don't know how to handle it
    directDeclaratorMerge(identifier, right.me)
  }

  private[parser] def directDeclaratorMerge(left: Identifier, right: DDBuild): DirectDeclarator = {
    right match {
      case v: DDParameterTypeList => FunctionDeclaration(left, v.v)
      case v: Empty => DirectDeclaratorOnly(left)
      case _ =>
        assert(false, "Cannot handle yet")
        null
    }
  }


  private[parser] lazy val pointer: Parser[Any] = P("*") ~ typeQualifierList.? |
    P(P("*") ~ typeQualifierList.? ~ pointer)
  private[parser] lazy val typeQualifierList = typeQualifier.rep(1)

  private[parser] lazy val parameterTypeList: Parser[ParameterTypeList] =
    (parameterList ~ P(P(",") ~ P("...")).!.?).opaque("parameterTypeList").map(v =>
      ParameterTypeList(v._1, v._2.isDefined))
  private[parser] lazy val parameterList: Parser[Seq[ParameterDeclaration]] =
    (parameterDeclaration ~ P(P(",") ~ parameterDeclaration).rep(0)).opaque("parameterList").map(v => (v._1 +: v._2).toList)
  private[parser] lazy val parameterDeclaration: Parser[ParameterDeclaration] = P(declarationSpecifiers ~ declarator).map(v => ParameterDeclarationDeclarator(v._1, v._2))
  // TODO need but directAbstractDeclarator will be a pain
  //    P(declarationSpecifiers ~ abstractDeclarator.?).map(v => ParameterDeclarationAbstractDeclarator(v._1, v._2))

  private[parser] lazy val identifierList: Parser[Seq[Identifier]] =
    (identifier ~ (P(",") ~ identifier).rep(0)).opaque("identifierList").map(v => (v._1 +: v._2).toList)

  // TODO This needs to go back
  //  private[parser] lazy val typeName: Parser[TypeName] = P(specifierQualifierList ~ abstractDeclarator.?).!.map(TypeName)
  private[parser] lazy val typeName: Parser[TypeName] = P(specifierQualifierList).!.map(TypeName)
  private[parser] lazy val abstractDeclarator: Parser[Any] = pointer |
    P(pointer.? ~ directAbstractDeclarator)
  private[parser] lazy val directAbstractDeclarator: Parser[Any] = P(P("(") ~ abstractDeclarator ~ P(")")) |
    P(directAbstractDeclarator.? ~ P("[") ~ typeQualifierList.?) ~ P(assignmentExpression.? ~ P("]")) |
    P(directAbstractDeclarator.? ~ P("[") ~ P("static") ~ typeQualifierList.?) ~ P(assignmentExpression ~ P("]")) |
    P(directAbstractDeclarator.? ~ P("[") ~ typeQualifierList ~ P("static")) ~ P(assignmentExpression ~ P("]")) |
    P(directAbstractDeclarator.? ~ P("[") ~ P("*") ~ P("]")) |
    P(directAbstractDeclarator.? ~ P("(") ~ parameterTypeList.? ~ P(")"))
  private[parser] lazy val typedefName = identifier
  private[parser] lazy val initializer = assignmentExpression |
    P(P("{") ~ initializerList ~ P("}")) |
    P(P("{") ~ initializerList ~ P(",") ~ P("}"))
  private[parser] lazy val initializerList: Parser[Any] = designation.? ~ initializer |
    P(initializerList ~ P(",") ~ designation.? ~ initializer)
  private[parser] lazy val designation = designatorList ~ P("=")
  private[parser] lazy val designatorList: Parser[Any] = designator |
    P(designatorList ~ designator)
  private[parser] lazy val designator = P("[") ~ constantExpression ~ P("]") |
    P(P(".") ~ identifier)
  private[parser] lazy val static_assertDeclaration = P("_Static_assert") ~ P("(") ~ constantExpression ~ P(",") ~ stringLiteral ~ P(")") ~ P(";")

  private[parser] val statement: Parser[Statement] = P(labeledStatement) |
    P(compoundStatement) |
    P(expressionStatement) |
    P(selectionStatement) |
    P(iterationStatement) |
    P(jumpStatement)
  private[parser] val labeledStatement: Parser[LabelledStatement] = (identifier ~ P(":") ~ statement).map(v => LabelledLabel(v._1, v._2))
  P(P("case") ~ constantExpression ~ P(":") ~ statement).map(v => LabelledCase(v._1, v._2)) |
    P(P("default") ~ P(":") ~ statement).map(v => LabelledDefault(v))
  private[parser] val compoundStatement: Parser[CompoundStatement] = P("{") ~ blockItemList.?.opaque("compoundStatement").map(v => CompoundStatement(v.getOrElse(List()))) ~ P("}")
  private[parser] lazy val blockItemList: Parser[Seq[BlockItem]] = blockItem.rep(1).opaque("blockItemList").map(v => v.toList)
  private[parser] lazy val blockItem: Parser[BlockItem] = statement
  // TODO
  //  private[parser] lazy val blockItem: Parser[BlockItem] = declaration | statement
  private[parser] val expressionStatement: Parser[Statement] = (expression.? ~ P(";")).map(v => if (v.isDefined) ExpressionStatement(v.get) else ExpressionEmptyStatement())
  private[parser] val selectionStatement: Parser[SelectionStatement] = P(P("if") ~ P("(") ~ expression ~ P(")") ~ statement).map(v => SelectionIf(v._1, v._2)) |
    P(P("if") ~ P("(") ~ expression ~ P(")") ~ statement ~ P("else") ~ statement).map(v => SelectionIfElse(v._1, v._2, v._3)) |
    P(P("switch") ~ P("(") ~ expression ~ P(")") ~ statement).map(v => SelectionSwitch(v._1, v._2))
  private[parser] val iterationStatement: Parser[IterationStatement] = P(P("while") ~ P("(") ~ expression ~ P(")") ~ statement).map(v => IterationWhile(v._1, v._2)) |
    P(P("do") ~ statement ~ P("while") ~ P("(") ~ expression ~ P(")") ~ P(";")).map(v => IterationDoWhile(v._2, v._1)) |
    P(P("for") ~ P("(") ~ expression.? ~ P(";") ~ expression.? ~ P(";") ~ expression.? ~ P(")") ~ statement).map(v => IterationFor1(v._1, v._2, v._3, v._4))
  // TODO when declaration read
  //    P(P("for") ~ P("(") ~ declaration ~ expression.? ~ P(";") ~ expression.? ~ P(")") ~ statement).map(v => IterationFor2(v._1, v._2, v._3))
  private[parser] val jumpStatement: Parser[JumpStatement] = P(P("goto") ~ identifier ~ P(";")).map(v => Goto(v)) |
    P(P("continue") ~ P(";")).map(v => Continue()) |
    P(P("break") ~ P(";")).map(v => Break()) |
    P(P("return") ~ expression.? ~ P(";")).map(v => Return(v))


  def parse(in: String) = {
    top.parse(in)
  }

  def parseSnippet(in: String) = {
    compoundStatement.parse(in)
  }

  //  A.2.4 External definitions
  private[parser] val declarationList: Parser[DeclarationList] = declaration.rep(1).map(v => DeclarationList(v)).opaque("declarationList")
  private[parser] val functionDefinition: Parser[FunctionDefinition] =
    (declarationSpecifiers ~ declarator ~ declarationList.? ~ compoundStatement).opaque("functionDefinition").map(v =>
      FunctionDefinition(v._1, v._2, v._3, v._4))
  private[parser] val externalDeclaration: Parser[ExternalDeclaration] = functionDefinition | declaration
  private[parser] val translationUnit = externalDeclaration.rep(1).map(_.toList)
  private[parser] val top = translationUnit

  //    A.3 Preprocessing directives
  //  private[parser] lazy val preprocessingFile = group.?
  //  private[parser] lazy val group = groupPart
  //  group groupPart
  //  private[parser] lazy val groupPart:
  //    ifSection
  //  controlLine
  //  textLine
  //  # nonDirective
  //  private[parser] lazy val ifSection:
  //    ifGroup elifGroups.?elseGroup.?endifLine
  //  private[parser] lazy val ifGroup:
  //  # if constantExpression newLine group.?
  //  # ifdef identifier newLine group.?
  //  # ifndef identifier newLine group.?
  //  private[parser] lazy val elifGroups = elifGroup
  //  elifGroups elifGroup
  //  private[parser] lazy val elifGroup:
  //  # elif constantExpression newLine group.?
  //  private[parser] lazy val elseGroup:
  //  # else newLine group.?
  //  private[parser] lazy val endifLine:
  //  # endif newLine
  //  private[parser] lazy val controlLine:
  //  # include ppTokens newLine
  //  # define identifier replacementList newLine
  //  # define identifier lparen identifierList.?)
  //  replacementList newLine
  //  # define identifier lparen ... ) replacementList newLine
  //  # define identifier lparen identifierList , ... )
  //  replacementList newLine
  //  # undef identifier newLine
  //  # line ppTokens newLine
  //  # error ppTokens.?newLine
  //  # pragma ppTokens.?newLine
  //  # newLine
  //  private[parser] lazy val textLine = ppTokens.? ~ newLine
  //  private[parser] lazy val nonDirective = ppTokens !newLine
  //  private[parser] lazy val lparen = a ( character not immediately preceded by whiteSpace
  //  private[parser] lazy val replacementList = ppTokens.?
  //  private[parser] lazy val ppTokens = preprocessingToken
  //  ppTokens preprocessingToken
  //  private[parser] lazy val newLine = P("\n")
}
