//package parser
//
//import fastparse.all.Parser
//import fastparse.core
//
//import scala.util.parsing.combinator.RegexParsers
//
//
//
//class CParserFull extends RegexParsers {
//  import fastparse.all._
//
//  private[parser] def digit = P(CharIn('0' to '9')).!.map(v => Digit(v.charAt(0)))
//
//  private[parser] val hexadecimalDigit =  P(CharIn('0' to '9') | CharIn('a' to 'f') | CharIn('A' to 'F')).!.map(v => HexDigit(v.charAt(0)))
//  private[parser] val hexQuad = P(hexadecimalDigit ~ hexadecimalDigit~ hexadecimalDigit~ hexadecimalDigit).map(v => HexQuad(v._1, v._2, v._3, v._4))
//  private[parser] val universalCharacterName = P(P("\\u") ~ hexQuad).map(v => UniversalCharacterName1(v)) | P("\\U" ~ hexQuad ~ hexQuad).map(v => UniversalCharacterName2(v._1, v._2))
//  private[parser] val nondigit = P(CharIn('a' to 'z') | CharIn('A' to 'Z') | "_").!.map(v => Nondigit(v.charAt(0)))
//
//  private[parser] val identifierNondigit = nondigit.map(v => IdentifierNondigit1(v)) |
//    universalCharacterName.map(v => IdentifierNondigit2(v))
//
//  // http://c0x.coding-guidelines.com/6.4.2.1.html
//  // Simplified
//  //  private[parser] val identifier: Parser[Identifier] = P(identifier ~ identifierNondigit).map(v => Identifier2(v._1, v._2)) |
//  //    P(identifier ~ digit).map(v => Identifier3(v._1, v._2)) |
//  //    identifierNondigit.map(v => Identifier1(v))
//  private[parser] val identifier = P(nondigit ~ (nondigit | universalCharacterName | digit).rep(1)).!.map(Identifier(_))
//  private[parser]  lazy val keyword = P(P("auto ∗") | P("break") | P("case") | P("char") | P("const") | P("continue") | P("default") | P("do") | P("double") | P("else")|
//    P("enum") | P("extern") | P("float") | P("for") | P("goto") | P("if") | P("inline") | P("int") | P("long") | P("register") | P("restrict")|
//    P("return") | P("short") | P("signed") | P("sizeof") | P("static") | P("struct") | P("switch") | P("typedef") | P("union") | P("unsigned")|
//    P("void") | P("volatile") | P("while") | P("_Alignas") | P("_Alignof") | P("_Atomic") | P("_Bool") | P("_Complex") | P("_Generic")|
//    P("_Imaginary") | P("_Noreturn") | P("_Static_assert") | P("_Thread_local")).!.map(v => Keyword(v))
//
//
//  private[parser]  lazy val constant: Parser[Constant] = P(integerConstant | floatingConstant)
//  private[parser]  lazy val integerConstant: Parser[IntConstant] =
//    P(decimalConstant ~ integerSuffix.? | octalConstant ~ integerSuffix.? | hexadecimalConstant ~ integerSuffix.?)
//  private[parser]  lazy val decimalConstant =
//    P(CharIn('1' to '9') ~ CharIn('0' to '9').rep(1)).!.map(v =>
//      IntConstant(Integer.parseInt(v, 10)))
//  private[parser]  lazy val octalConstant =
//    P("0" ~ CharIn('0' to '7').rep(1)).!.map(v =>
//      IntConstant(Integer.parseInt(v, 8)))
//  private[parser] lazy val hexadecimalPrefix = P("0x") | P("0X")
//  private[parser]  lazy val hexadecimalConstant =
//    P(hexadecimalPrefix ~ CharIn("0123456789abcdef").rep(1)).!.map(v =>
//      IntConstant(Integer.parseInt(v, 16)))
//  private[parser]  lazy val nonzeroDigit = P(CharIn('1' to '9')).!.map(v => Digit(v.charAt(0)))
//  //  private[parser]  lazy val octalDigit = P(CharIn('0' to '7'))
//  private[parser]  lazy val integerSuffix = P((CharIn("uU") ~ P(P("ll") | P("LL") | P("l") | P("L"))) |
//    ((P("ll") | P("LL") | P("l") | P("L")) ~ P(CharIn("uU")).?))
//  //  private[parser]  lazy val unsignedSuffix = P(CharIn("uU"))
//  //  private[parser]  lazy val longSuffix = P(CharIn("lL"))
//  //  private[parser]  lazy val longLongSuffix = P("ll" | "LL")
//  private[parser]  lazy val floatingConstant: Parser[FloatConstant] = P(decimalFloatingConstant | hexadecimalFloatingConstant)
//  private[parser]  lazy val decimalFloatingConstant: Parser[FloatConstant] = P(P(fractionalConstant ~ exponentPart.? ~ floatingSuffix.?).map(v => FloatConstant(v._1.v * v._2.map(_.calc()).getOrElse(1.0f))) |
//    P(digitSequence.!.map(v => {
//      v.toInt
//    }) ~ exponentPart ~ floatingSuffix.?).map(v => FloatConstant(v._1 * v._2.calc())))
//  private[parser] lazy val hexadecimalFloatingConstant: Parser[FloatConstant] =
//    P(hexadecimalPrefix ~ hexadecimalFractionalConstant ~ binaryExponentPart ~ floatingSuffix.?).map(v =>
//      FloatConstant(v._1.v * v._2.calc())) |
//      P(hexadecimalPrefix ~ hexadecimalDigitSequence ~ binaryExponentPart ~ floatingSuffix.?).map(v =>
//        FloatConstant(v._1.v * v._2.calc()))
//  private[parser] lazy val fractionalConstant = P(digitSequence.? ~ P(".") ~ digitSequence).!.map(v => FloatConstant(v.toFloat))
//  private[parser] lazy val exponentPart: Parser[Exponent] =
//    (P("e" | P("E")) ~ CharIn("+-").? ~ digitSequence).!.map(v => {
//      Exponent(v.stripPrefix("e").stripPrefix("E").toInt)})
//  private[parser] lazy val digitSequence = P(CharIn("0123456789").rep(1))
//  private[parser] lazy val hexadecimalFractionalConstant: Parser[FloatConstant] =
//    P(hexadecimalDigitSequence.?.! ~ P(".").! ~ hexadecimalDigitSequence.!).map(v =>
//      FloatConstant(v._1.toFloat + Math.pow(v._3.toFloat, v._3.size * 1).toFloat))
//  private[parser] lazy val binaryExponentPart: Parser[Exponent] =
//    (P("p" | P("P")) ~ CharIn("+-").? ~ digitSequence).!.map(v =>
//      Exponent(v.stripPrefix("p").stripPrefix("P").toInt))
//  private[parser] lazy val hexadecimalDigitSequence: Parser[FloatConstant] =
//    P(CharIn("0123456789abcdef").rep(1)).!.map(v => FloatConstant(v.toFloat))
//  private[parser] lazy val floatingSuffix = P(CharIn("flFL"))
//
//
//  private[parser] lazy val enumerationConstant = identifier.map(v => EnumerationConstant(v.v))
//  private[parser] lazy val cCharSequence = CharsWhile(v => v != '\'' && v != '\\' && v != '\n')
//  private[parser] lazy val characterConstant:Parser[CharacterConstant] =
//    P(CharIn("LuU").? ~ P("\'") ~ cCharSequence ~ "\'").!.map(v =>
//      CharacterConstant(v.stripPrefix("\"").stripSuffix("\"")))
//  //  private[parser] lazy val escapeSequence = simpleEscapeSequence
//  //  octalEscapeSequence
//  //  hexadecimalEscapeSequence
//  //  universalCharacterName
//  //  private[parser] lazy val simpleEscapeSequence: one of
//  //    \' \" \? \\
//  //  \a \b \f \n \r \t \v
//  //  private[parser] lazy val octalEscapeSequence = \ octalDigit
//  //    \ octalDigit octalDigit
//  //  \ octalDigit octalDigit octalDigit
//  //  private[parser] lazy val hexadecimalEscapeSequence = \x hexadecimalDigit
//  //  hexadecimalEscapeSequence hexadecimalDigit
//  //    A.1.6 String literals
//  private[parser] lazy val stringLiteral = P(encodingPrefix.? ~ CharIn("\"") ~ CharsWhile(v => v != '"' && v != '\\' && v != '\n') ~ CharIn("\"")).!.map(v =>
//    StringLiteral(v.stripPrefix("u8").stripPrefix("u").stripPrefix("U").stripPrefix("L").stripPrefix("\"").stripSuffix("\"")))
//  private[parser] lazy val encodingPrefix = P("u8" | CharIn("uUL"))
//  private[parser] lazy val punctuator: Parser[Punctuator] =
//    P(P("[") | P("|") | P("]") | P("(") | P(")") | P("{") | P("}") | P(".") | P("->") | P("++") | P("--") | P("&") |
//      P("*") | P("+") | P("-") | P("~") | P("!") | P("/") | P("%") | P("<<") | P(">>") | P("<") | P(">") | P("<=") |
//      P(">=") | P("==") | P("!=") | P("^") | P("|") | P("&&") | P("||") | P("?") | P(":") | P(";") | P("...") | P("=") |
//      P("*=") | P("/=") | P("%=") | P("+=") | P("-=") | P("<<=") | P(">>=") | P("&=") | P("^=") | P("|=") | P(",") |
//      P("#") | P("##") | P("<:") | P(":>") | P("<%") | P("%>") | P("%:") | P("%:%:")).!.map(v => Punctuator(v))
//
//  private[parser]  lazy val token: Parser[Token] =
//    P(keyword | identifier | constant | stringLiteral | punctuator)
//
//
//
//
//  A.2 Phrase structure grammar
//    A.2.1 Expressions
//    private[parser] lazy val primaryExpression = identifier
//  constant
//  stringLiteral
//  ( expression )
//  genericSelection
//  private[parser] lazy val genericSelection = _Generic ( assignmentExpression , genericAssocList )
//  private[parser] lazy val genericAssocList = genericAssociation
//  genericAssocList , genericAssociation
//  private[parser] lazy val genericAssociation:
//  typeName : assignmentExpression
//  default : assignmentExpression
//  private[parser] lazy val postfixExpression = primaryExpression
//  postfixExpression [ expression ]
//  postfixExpression ( argumentExpressionList.?)
//  postfixExpression . identifier
//  postfixExpression -> identifier
//  postfixExpression ++
//    postfixExpression --
//    ( typeName ) { initializerList }
//  ( typeName ) { initializerList , }
//  private[parser] lazy val argumentExpressionList = assignmentExpression
//  argumentExpressionList , assignmentExpression
//  private[parser] lazy val unaryExpression = postfixExpression
//  ++ unaryExpression
//  -- unaryExpression
//  unaryOperator castExpression
//  sizeof unaryExpression
//  sizeof ( typeName )
//  _Alignof ( typeName )
//  private[parser] lazy val unaryOperator: one of
//    &*+-~!
//  private[parser] lazy val castExpression = unaryExpression
//  ( typeName ) castExpression
//  private[parser] lazy val multiplicativeExpression = castExpression
//  multiplicativeExpression * castExpression
//  multiplicativeExpression / castExpression
//  multiplicativeExpression % castExpression
//  private[parser] lazy val additiveExpression = multiplicativeExpression
//  additiveExpression + multiplicativeExpression
//  additiveExpression - multiplicativeExpression
//  private[parser] lazy val shiftExpression = additiveExpression
//  shiftExpression << additiveExpression
//  shiftExpression >> additiveExpression
//  private[parser] lazy val relationalExpression = shiftExpression
//  relationalExpression < shiftExpression
//  relationalExpression > shiftExpression
//  relationalExpression <= shiftExpression
//  relationalExpression >= shiftExpression
//  private[parser] lazy val equalityExpression = relationalExpression
//  equalityExpression == relationalExpression
//  equalityExpression != relationalExpression
//  private[parser] lazy val ANDExpression = equalityExpression
//  ANDExpression & equalityExpression
//  private[parser] lazy val exclusiveORExpression = ANDExpression
//  exclusiveORExpression ^ ANDExpression
//  private[parser] lazy val inclusiveORExpression = exclusiveORExpression
//  inclusiveORExpression | exclusiveORExpression
//  private[parser] lazy val logicalANDExpression = inclusiveORExpression
//  logicalANDExpression && inclusiveORExpression
//  private[parser] lazy val logicalORExpression = logicalANDExpression
//  logicalORExpression || logicalANDExpression
//  private[parser] lazy val conditionalExpression = logicalORExpression
//  logicalORExpression ? expression : conditionalExpression
//  private[parser] lazy val assignmentExpression = conditionalExpression
//  unaryExpression assignmentOperator assignmentExpression
//  private[parser] lazy val assignmentOperator: one of
//  = *= /= %= += -= <<= >>= &= ^= |=
//    private[parser] lazy val expression = assignmentExpression
//  expression , assignmentExpression
//  private[parser] lazy val constantExpression = conditionalExpression
//  A.2.2 Declarations
//    private[parser] lazy val declaration = declarationSpecifiers initDeclaratorList.?;
//  static_assertDeclaration
//  private[parser] lazy val declarationSpecifiers = storageClassSpecifier declarationSpecifiers.?
//  typeSpecifier declarationSpecifiers.?
//  typeQualifier declarationSpecifiers.?
//  functionSpecifier declarationSpecifiers.?
//  alignmentSpecifier declarationSpecifiers.?
//  private[parser] lazy val initDeclaratorList = initDeclarator
//  initDeclaratorList , initDeclarator
//  private[parser] lazy val initDeclarator = declarator
//  declarator = initializer
//  private[parser] lazy val storageClassSpecifier = typedef
//  extern
//  static
//  _Thread_local
//  auto
//  register
//  private[parser] lazy val typeSpecifier = void
//  char
//  short
//  int
//  long
//  float
//  double
//  signed
//  unsigned
//  _Bool
//  _Complex
//  atomicTypeSpecifier
//  structOrUnionSpecifier
//  enumSpecifier
//  typedefName
//  private[parser] lazy val structOrUnionSpecifier = structOrUnion identifier.?{ structDeclarationList }
//  structOrUnion identifier
//    private[parser] lazy val structOrUnion = struct
//  union
//  private[parser] lazy val structDeclarationList = structDeclaration
//  structDeclarationList structDeclaration
//  private[parser] lazy val structDeclaration = specifierQualifierList structDeclaratorList.?;
//  static_assertDeclaration
//  private[parser] lazy val specifierQualifierList:
//  typeSpecifier specifierQualifierList.?
//  typeQualifier specifierQualifierList.?
//  private[parser] lazy val structDeclaratorList = structDeclarator
//  structDeclaratorList , structDeclarator
//  private[parser] lazy val structDeclarator = declarator
//  declarator.?: constantExpression
//  private[parser] lazy val enumSpecifier = enum identifier.?{ enumeratorList }
//  enum identifier.?{ enumeratorList , }
//  enum identifier
//    private[parser] lazy val enumeratorList = enumerator
//  enumeratorList , enumerator
//  private[parser] lazy val enumerator = enumerationConstant
//  enumerationConstant = constantExpression
//  private[parser] lazy val atomicTypeSpecifier = _Atomic ( typeName )
//  private[parser] lazy val typeQualifier = const
//  restrict
//  volatile
//  _Atomic
//  private[parser] lazy val functionSpecifier = inline
//  _Noreturn
//  private[parser] lazy val alignmentSpecifier = _Alignas ( typeName )
//  _Alignas ( constantExpression )
//  private[parser] lazy val declarator = pointer.?directDeclarator
//  private[parser] lazy val directDeclarator = identifier
//  ( declarator )
//  directDeclarator [ typeQualifierList.?assignmentExpression.?]
//  directDeclarator [ static typeQualifierList.?assignmentExpression ]
//  directDeclarator [ typeQualifierList static assignmentExpression ]
//  directDeclarator [ typeQualifierList.?* ]
//  directDeclarator ( parameterTypeList )
//  directDeclarator ( identifierList.?)
//  private[parser] lazy val pointer = * typeQualifierList.?
//  * typeQualifierList.?pointer
//    private[parser] lazy val typeQualifierList:
//  typeQualifier
//  typeQualifierList typeQualifier
//  private[parser] lazy val parameterTypeList = parameterList
//  parameterList , ...
//  private[parser] lazy val parameterList = parameterDeclaration
//  parameterList , parameterDeclaration
//  private[parser] lazy val parameterDeclaration = declarationSpecifiers declarator
//      declarationSpecifiers abstractDeclarator.?
//  private[parser] lazy val identifierList = identifier
//  identifierList , identifier
//  private[parser] lazy val typeName = specifierQualifierList abstractDeclarator.?
//  private[parser] lazy val abstractDeclarator = pointer
//  pointer.?directAbstractDeclarator
//  private[parser] lazy val directAbstractDeclarator = ( abstractDeclarator )
//  directAbstractDeclarator.?[ typeQualifierList.?
//  assignmentExpression.?]
//  directAbstractDeclarator.?[ static typeQualifierList.?
//  assignmentExpression ]
//  directAbstractDeclarator.?[ typeQualifierList static
//    assignmentExpression ]
//  directAbstractDeclarator.?[*]
//  directAbstractDeclarator.?( parameterTypeList.?)
//  private[parser] lazy val typedefName = identifier
//  private[parser] lazy val initializer = assignmentExpression
//      { initializerList }
//  { initializerList , }
//  private[parser] lazy val initializerList = designation.?initializer
//      initializerList , designation.?initializer
//    private[parser] lazy val designation = designatorList =
//  private[parser] lazy val designatorList = designator
//  designatorList designator
//  private[parser] lazy val designator:
//  [ constantExpression ]
//  . identifier
//  private[parser] lazy val static_assertDeclaration = _Static_assert ( constantExpression , stringLiteral ) ;
//  A.2.3 Statements
//  private[parser] lazy val statement = labeledStatement
//  compoundStatement
//  expressionStatement
//  selectionStatement
//  iterationStatement
//  jumpStatement
//  private[parser] lazy val labeledStatement = identifier : statement
//  case constantExpression : statement
//  default : statement
//  private[parser] lazy val compoundStatement = { blockItemList.?}
//  private[parser] lazy val blockItemList = blockItem
//  blockItemList blockItem
//  private[parser] lazy val blockItem = declaration
//  statement
//  private[parser] lazy val expressionStatement = expression.?;
//  private[parser] lazy val selectionStatement:
//  if ( expression ) statement
//  if ( expression ) statement else statement
//  switch ( expression ) statement
//    private[parser] lazy val iterationStatement:
//  while ( expression ) statement
//  do statement while ( expression ) ;
//  for ( expression.?; expression.?; expression.?) statement
//  for ( declaration expression.?; expression.?) statement
//  private[parser] lazy val jumpStatement = goto identifier ;
//  continue ;
//  break ;
//  return expression.?;
//  A.2.4 External definitions
//  private[parser] lazy val translationUnit = externalDeclaration
//  translationUnit externalDeclaration
//  private[parser] lazy val externalDeclaration = functionDefinition
//  declaration
//  private[parser] lazy val functionDefinition = declarationSpecifiers declarator declarationList.?compoundStatement
//  private[parser] lazy val declarationList = declaration
//  declarationList declaration
//    A.3 Preprocessing directives
//  private[parser] lazy val preprocessingFile = group.?
//  private[parser] lazy val group = groupPart
//  group groupPart
//  private[parser] lazy val groupPart:
//  ifSection
//  controlLine
//  textLine
//  # nonDirective
//  private[parser] lazy val ifSection:
//  ifGroup elifGroups.?elseGroup.?endifLine
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
//  private[parser] lazy val textLine = ppTokens.?newLine
//  private[parser] lazy val nonDirective = ppTokens newLine
//  private[parser] lazy val lparen = a ( character not immediately preceded by whiteSpace
//  private[parser] lazy val replacementList = ppTokens.?
//  private[parser] lazy val ppTokens = preprocessingToken
//  ppTokens preprocessingToken
//  private[parser] lazy val newLine = the newLine character
//}