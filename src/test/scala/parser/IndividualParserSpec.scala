package parser

import fastparse.core.Parsed
import org.scalatest.FunSuite
import fastparse.all._

// For testing small parts of the parser
class IndividualParserSpec extends FunSuite {


  def good[T, Elem, Repr](p: Parsed[T, Elem, Repr], expected: T): Unit = {
    p match {
      case Parsed.Success(x, y) => assert (x == expected)
      case Parsed.Failure(x, y, z) =>
        println(p)
        println(x)
        println(y)
        println(z)
        assert (false)
    }
  }

  def bad[T, Elem, Repr](p: Parsed[T, Elem, Repr]): Unit = {
    p match {
      case Parsed.Success(x, y) => assert (false)
      case Parsed.Failure(x, y, z) =>
    }
  }

  def createParser() = new SimpleCFastParse

  test("hexadecimalDigit") {
    val p = createParser()
    good(p.hexadecimalDigit.parse("d"), HexDigit('d'))
    good(p.hexadecimalDigit.parse("0"), HexDigit('0'))
    bad(p.hexadecimalDigit.parse("g"))
  }

  test("hexQuad") {
    val p = createParser()
    good(p.hexQuad.parse("d01d"), HexQuad(HexDigit('d'),HexDigit('0'),HexDigit('1'),HexDigit('d')))
    bad(p.hexQuad.parse("g"))
  }

  test("nonDigit") {
    val p = createParser()
    good(p.nondigit.parse("A"), Nondigit('A'))
    good(p.nondigit.parse("a"), Nondigit('a'))
    bad(p.nondigit.parse("0"))
  }

  test("identifierNondigit") {
    val p = createParser()
    good(p.identifierNondigit.parse("A"), IdentifierNondigit1(Nondigit('A')))
    good(p.identifierNondigit.parse("a"), IdentifierNondigit1(Nondigit('a')))
    bad(p.identifierNondigit.parse("0"))
  }

  test("identifier") {
    val p = createParser()
    good(p.identifier.parse("A"), Identifier("A"))
    good(p.identifier.parse("yabba"), Identifier("yabba"))
    good(p.identifier.parse("yabba3"), Identifier("yabba3"))
    good(p.identifier.parse("yabba3doo"), Identifier("yabba3doo"))
    bad(p.identifier.parse("3yabba"))
  }

  test("decimalFloatingConstant") {
    val p = createParser()
    assert(Math.abs(p.decimalFloatingConstant.parse("123.12312").get.value.v - 123.12312f) < 0.01)
    assert(Math.abs(p.decimalFloatingConstant.parse("123.12E1").get.value.v - 1231.2) < 0.01)
    assert(Math.abs(p.decimalFloatingConstant.parse("123E1").get.value.v - 1230f) < 0.01)
  }

  test("floatingConstant") {
    val p = createParser()
    assert(Math.abs(p.floatingConstant.parse("123.12312").get.value.v - 123.12312f) < 0.01)
    assert(Math.abs(p.floatingConstant.parse("123.12E1").get.value.v - 1231.2) < 0.01)
    assert(Math.abs(p.floatingConstant.parse("123E1").get.value.v - 1230f) < 0.01)
  }

  test("exponentPart") {
    val p = createParser()
    good(p.exponentPart.parse("e123"), Exponent(123))
    good(p.exponentPart.parse("E123"), Exponent(123))
    good(p.exponentPart.parse("E-123"), Exponent(-123))
  }

  test("stringLiteral") {
    val p = createParser()
    good(p.stringLiteral.parse(""""hello""""), StringLiteral("hello"))
    good(p.stringLiteral.parse("""u"hello""""), StringLiteral("hello"))
  }

  test("token") {
    val p = createParser()
    good(p.token.parse(""""hello""""), StringLiteral("hello"))
    good(p.token.parse("""inline"""), Keyword("inline"))
    good(p.token.parse("""|"""), Punctuator("|"))
  }

  test("headerName") {
    val p = createParser()
    good(p.headerName.parse(""""hello.h""""), HeaderName("hello.h"))
    good(p.headerName.parse("""<hello.h>"""), HeaderName("hello.h"))
  }

  test("jumpStatement") {
    val p = createParser()
    good(p.jumpStatement.parse("""continue;"""), Continue())
//    good(p.jumpStatement.parse("""goto hello;"""), Goto(Identifier("hello")))
    good(p.jumpStatement.parse("""break;"""), Break())
//    good(p.jumpStatement.parse("""return;"""), Break())
  }

  test("primaryExpression") {
    val p = createParser()
    good(p.primaryExpression.parse(""""hello""""), StringLiteral("hello"))
    good(p.primaryExpression.parse("""1234"""), IntConstant(1234))
    good(p.primaryExpression.parse("""1"""), IntConstant(1))
  }


  test("?") {
    val p = createParser()
    import fastparse.all._
    println((p.postfixExpression ~ End).parse("""hello++"""))
  }

  test("how to parse binary ops") {
    import fastparse.all._

    // Stackoverflow as we just loop through expr for every
    if (false) {
      val term: P[Any] = CharIn('0' to '9')
      val expr: P[Any] = P(expr ~ "+" ~ expr) | term
      val top = expr ~ End
      println(top.parse("1+1"))
    }

    // Only matches "1", doesn't match "+1"
    if (false) {
      val term: P[Any] = CharIn('0' to '9')
      val expr: P[Any] = term | P(expr ~ "+" ~ expr)
      val top = expr ~ End
      println(top.parse("1+1"))
    }

    // Works, but can only match +
    if (false) {
      val term: P[Any] = CharIn('0' to '9')
      val expr: P[Any] = term ~ P("+" ~ term).rep
      val top = expr ~ End
      println(top.parse("1+1"))
      println(top.parse("1+1+2"))
    }

    // Trying to get it matching + and -.  Fails, back to stackoverflow
    if (false) {
      val term: P[Any] = CharIn('0' to '9')
      lazy val add: P[Any] = expr ~ P("+" ~ expr).rep
      lazy val sub: P[Any] = expr ~ P("-" ~ expr).rep
      lazy val expr = add | sub
      val top = expr ~ End
      println(top.parse("1+1"))
      println(top.parse("1+1-2"))
    }

    // Attempting to refactor left recursion.  Working, not handling + and - yet.
    if (false) {
      val term: P[Any] = CharIn('0' to '9')
      lazy val add: P[Any] = term ~ add2
      lazy val add2: P[Any] = P(P("+") ~ add) | End
      val top = add ~ End
      println(top.parse("1+1"))
      println(top.parse("1+1+2"))
    }

    // Works!
    val term: P[Any] = CharIn('0' to '9')
    lazy val add: P[Any] = term ~ add2
    lazy val add2: P[Any] = P(P("+") ~ expr) | End
    lazy val sub: P[Any] = term ~ sub2
    lazy val sub2: P[Any] = P(P("-") ~ expr) | End
    lazy val expr = add | sub
    val top = expr ~ End
    println(top.parse("1+1"))
    println(top.parse("1+1+2"))
    println(top.parse("1+1-2"))

  }

  test("postfix++") {
    import fastparse.all._
    val parser = createParser()
    good((parser.postfixExpression ~ End).parse("hello++"), PostfixExpressionPlusPlus(Identifier("hello")))
  }

  test("postfixExpression ++ --") {
    import fastparse.all._
    val parser = createParser()
    good((parser.postfixExpression ~ End).parse("hello++"), PostfixExpressionPlusPlus(Identifier("hello")))
    good((parser.postfixExpression ~ End).parse("hello++--"), PostfixExpressionMinusMinus(PostfixExpressionPlusPlus(Identifier("hello"))))
    good((parser.postfixExpression ~ End).parse("hello++--++"), PostfixExpressionPlusPlus(PostfixExpressionMinusMinus(PostfixExpressionPlusPlus(Identifier("hello")))))
  }

  test("postfixExpression []") {
    import fastparse.all._
    val parser = createParser()
    good((parser.postfixExpression ~ End).parse("hello[1]"), PostfixExpressionIndex(Identifier("hello"), IntConstant(1)))
    good((parser.postfixExpression ~ End).parse("hello[world]"), PostfixExpressionIndex(Identifier("hello"), Identifier("world")))
    good((parser.postfixExpression ~ End).parse("hello[world]++"), PostfixExpressionPlusPlus(PostfixExpressionIndex(Identifier("hello"), Identifier("world"))))
  }

  test("postfixExpression .") {
    val parser = createParser()
    good((parser.postfixExpression ~ End).parse("hello.world"), PostfixExpressionDot(Identifier("hello"), Identifier("world")))
    good((parser.postfixExpression ~ End).parse("hello.world.again"), PostfixExpressionDot(PostfixExpressionDot(Identifier("hello"), Identifier("world")), Identifier("again")))
  }

  test("unaryExpression ++") {
    val parser = createParser()
    good((parser.unaryExpression ~ End).parse("++hello"), UnaryExpressionPlusPlus(Identifier("hello")))
    good((parser.unaryExpression ~ End).parse("--hello"), UnaryExpressionMinusMinus(Identifier("hello")))
    good((parser.unaryExpression ~ End).parse("++hello++"), UnaryExpressionPlusPlus(PostfixExpressionPlusPlus(Identifier("hello"))))
//    good((parser.postfixExpression ~ End).parse("hello.world.again"), PostfixExpressionDot(PostfixExpressionDot(Identifier("hello"), Identifier("world")), Identifier("again")))
  }

  test("typeName") {
    val parser = createParser()
    good((parser.typeName ~ End).parse("int"), TypeName("int"))
  }

  test("cast") {
    val parser = createParser()
    good((parser.castExpression ~ End).parse("(int)hello"), CastExpression(TypeName("int"), Identifier("hello")))
  }

  test("*") {
    val parser = createParser()
    good((parser.multiplicativeExpression ~ End).parse("hello*world"), ExpressionMultiply(Identifier("hello"), Identifier("world")))
    good((parser.multiplicativeExpression ~ End).parse("hello*world*3"), ExpressionMultiply(Identifier("hello"), ExpressionMultiply(Identifier("world"),IntConstant(3))))
    good((parser.multiplicativeExpression ~ End).parse("hello*world*3++"), ExpressionMultiply(Identifier("hello"), ExpressionMultiply(Identifier("world"),PostfixExpressionPlusPlus(IntConstant(3)))))
    good((parser.multiplicativeExpression ~ End).parse("hello*world*(3++)"), ExpressionMultiply(Identifier("hello"), ExpressionMultiply(Identifier("world"),PostfixExpressionPlusPlus(IntConstant(3)))))
  }

  test("=") {
    val parser = createParser()
    good((parser.assignmentExpression ~ End).parse("hello=world"), ExpressionAssignment(Identifier("hello"), Identifier("world")))
    good((parser.assignmentExpression ~ End).parse("hello*=world"), ExpressionAssignment(Identifier("hello"), ExpressionMultiply(Identifier("hello"), Identifier("world"))))
  }

  test("top level expression") {
    val parser = createParser()
    good((parser.expression ~ End).parse("hello=world"), ExpressionAssignment(Identifier("hello"), Identifier("world")))
    good((parser.expression ~ End).parse("hello*=world"), ExpressionAssignment(Identifier("hello"), ExpressionMultiply(Identifier("hello"), Identifier("world"))))
    good((parser.expression ~ End).parse("hello*world*(3++)"), ExpressionMultiply(Identifier("hello"), ExpressionMultiply(Identifier("world"),PostfixExpressionPlusPlus(IntConstant(3)))))
    good((parser.expression ~ End).parse("++hello++"), UnaryExpressionPlusPlus(PostfixExpressionPlusPlus(Identifier("hello"))))
  }

  test("top level statement") {
    val parser = createParser()
    good((parser.statement ~ End).parse("hello=world;"), ExpressionStatement(ExpressionAssignment(Identifier("hello"), Identifier("world"))))
    good((parser.statement ~ End).parse("hello*=world;"), ExpressionStatement(ExpressionAssignment(Identifier("hello"), ExpressionMultiply(Identifier("hello"), Identifier("world")))))
    good((parser.statement ~ End).parse("hello*world*(3++);"), ExpressionStatement(ExpressionMultiply(Identifier("hello"), ExpressionMultiply(Identifier("world"),PostfixExpressionPlusPlus(IntConstant(3))))))
    good((parser.statement ~ End).parse("++hello++;"), ExpressionStatement(UnaryExpressionPlusPlus(PostfixExpressionPlusPlus(Identifier("hello")))))
  }

  test("expressionStatement") {
    val parser = createParser()
    val x = ExpressionStatement(ExpressionAssignment(Identifier("hello"), Identifier("world")))
    good((parser.expressionStatement ~ End).parse("hello=world;"), x)
    good((parser.expressionStatement ~ End).parse(";"), ExpressionEmptyStatement())
  }

  test("if") {
    val parser = createParser()
    good((parser.selectionStatement ~ End).parse("if(hello==world)hello=1;"), SelectionIf(ExpressionEquals(Identifier("hello"), Identifier("world")), ExpressionStatement(ExpressionAssignment(Identifier("hello"), IntConstant(1)))))
  }

  test("whitespace") {
    val parser = createParser()
    good((parser.selectionStatement ~ End).parse("if(hello==world)hello=1;"), SelectionIf(ExpressionEquals(Identifier("hello"), Identifier("world")), ExpressionStatement(ExpressionAssignment(Identifier("hello"), IntConstant(1)))))
    good((parser.selectionStatement ~ End).parse("if(hello==world) hello=1;"), SelectionIf(ExpressionEquals(Identifier("hello"), Identifier("world")), ExpressionStatement(ExpressionAssignment(Identifier("hello"), IntConstant(1)))))
  }

}