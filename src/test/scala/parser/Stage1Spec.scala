package parser

import fastparse.core.Parsed
import org.scalatest.FunSuite

class Stage1Spec extends FunSuite {
  //  def createParser() = new CParser
  //  def parse(parser: CParser, raw: String): parser.ParseResult[Program] = {
  //    val x = parser.parse(parser.program, raw)
  //    x
  //  }
  //
  //  def checkOutput(p: Program, expected: String): Boolean = {
  //    val x = p.toString
  //    if (x !== expected) {
  //      println(x)
  //      println(expected)
  //    }
  //    x === expected
  //  }
  //
  //  def checkOutput(actual: String, expected: String): Boolean = {
  //    val actualStrip = actual.replace("$\\w+", "").replaceAll("  ", " ").replace("\r\n", "\n").stripMargin.trim
  //    val expectedStrip = expected.replace("$\\w+", "").replaceAll("  ", " ").replace("\r\n", "\n").stripMargin.trim
  //    if (actualStrip !== expectedStrip) {
  //      println(actualStrip)
  //      println(expectedStrip)
  //    }
  //    assert (actualStrip === expectedStrip)
  //    actualStrip === expectedStrip
  //  }
  //
  //  test("multi_digit.c") {
  //    val raw =
  //      """int main() {
  //        |    return 100;
  //        |}""".stripMargin
  //    val parser = createParser()
  //    parse(parser, raw) match {
  //      case parser.Success(m,_) =>
  //        assert(m.f.identifier == "main")
  //        assert(m.f.statement.exp.intLiteral == 100)
  //        assert(checkOutput(m, """ .globl _foo
  //                               |_foo:
  //                               | movl    $100, %eax
  //                               | ret
  //                               |""".stripMargin))
  //      case parser.Failure(msg,_) =>
  //        println("FAILURE: " + msg)
  //        assert(false)
  //      case parser.Error(msg,_) =>
  //        println("ERROR: " + msg)
  //        assert(false)
  //    }
  //  }

  //  test("function") {
  //    val f = Function("foo", Statement(Exp(200)))
  //    assert(checkOutput(f.toString, """ .globl _foo
  //                            |_foo:
  //                            | movl    $200, %eax
  //                            | ret
  //                            |""".stripMargin))
  //  }
  //
  //  test("simple") {
  //    val raw = """hello0123""".stripMargin
  //    val c = new SimpleCParse
  //    println(c.parse(c.identifier, raw))
  //  }
  //
  //  test("fastparse") {
  //    import fastparse.all._
  //    val parseA = P( "a" )
  //
  //    val Parsed.Success(value, successIndex) = parseA.parse("a")
  //    assert(value == (), successIndex == 1)
  //
  //    val failure = parseA.parse("b").asInstanceOf[Parsed.Failure]
  //    assert(failure.lastParser == ("a": P0))
  //      assert(failure.index == 0)
  //      assert(failure.extra.traced.trace == """parseA:1:1 / "a":1:1 ..."b"""")
  //
  //  }

  def good[T, Elem, Repr](p: Parsed[T, Elem, Repr], expected: T): Unit = {
    p match {
      case Parsed.Success(x, y) => assert (x == expected)
      case Parsed.Failure(x, y, z) =>
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
    good((parser.postfixExpressionSimple ~ End).parse("hello++"), PostfixExpressionPlusPlus(Identifier("hello")))
  }

  test("postfixExpression ++ --") {
    import fastparse.all._
    val parser = createParser()
    good((parser.postfixExpressionSimple ~ End).parse("hello++"), PostfixExpressionPlusPlus(Identifier("hello")))
    good((parser.postfixExpressionSimple ~ End).parse("hello++--"), PostfixExpressionMinusMinus(PostfixExpressionPlusPlus(Identifier("hello"))))
    good((parser.postfixExpressionSimple ~ End).parse("hello++--++"), PostfixExpressionPlusPlus(PostfixExpressionMinusMinus(PostfixExpressionPlusPlus(Identifier("hello")))))
  }

  test("postfixExpression ++ --") {
    import fastparse.all._
    val parser = createParser()
    good((parser.postfixExpressionSimple ~ End).parse("hello++"), PostfixExpressionPlusPlus(Identifier("hello")))
    good((parser.postfixExpressionSimple ~ End).parse("hello++--"), PostfixExpressionMinusMinus(PostfixExpressionPlusPlus(Identifier("hello"))))
    good((parser.postfixExpressionSimple ~ End).parse("hello++--++"), PostfixExpressionPlusPlus(PostfixExpressionMinusMinus(PostfixExpressionPlusPlus(Identifier("hello")))))
  }


  test("?2") {
    import fastparse.all._
    val parser = createParser()
    println((parser.postfixExpressionSimple ~ End).parse("hello++"))

    /*
      hello[i]++
      (((hello)[i])++)
      1. expr only, no operand, param=none
      2. expr=StringConstant(hello) op=[] param=StringConstant(i)
      3. expr=all that op=++ param=none

      hello[i]++(y,b)

    x = Op("hello[i]", "++", -)
    Op(x, "()", "y,b")

     */
  }

}