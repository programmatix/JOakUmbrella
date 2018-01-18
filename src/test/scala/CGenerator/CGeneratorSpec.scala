package CGenerator

import fastparse.core.Parsed
import generating.CGenerator
import org.scalatest.FunSuite
import parsing._
import pprint.PPrinter

class CGeneratorSpec extends FunSuite {
  val gg = createGenerator()
  val pp = createParser()

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

  def createParser() = new CParser
  def createGenerator() = new CGenerator
  def check(v: String, print: Boolean = false) = confirm(v, print)

  test("hello++") {
    val g = new CGenerator()
    val out = g.generateExpression(PostfixExpressionPlusPlus(Identifier("hello")))
    assert(out(0) == "hello")
    assert(out(1) == "++")
  }

  test("hello[world]++") {
    val in = PostfixExpressionPlusPlus(PostfixExpressionIndex(Identifier("hello"), Identifier("world")))
    val g = new CGenerator()
    val out = g.generateExpression(in)
    assert(out == Seq("hello", "[", "world", "]", "++"))
  }

//  def testStringsIgnoreWhitespace(in1: String, in2: String): Boolean = {
//    var idx1 = 0
//    var idx2 = 0
//    var done = false
//    while (!done) {
//      if (idx1 >= in1.length) done = true
//      else if (idx2 >= in2.length) done = true
//      else {
//
//
//        idx1 += 1
//        idx2 += 1
//      }
//    }
//  }

  def clean(s: String) = s.replaceAll(" ", "").replaceAll("\r\n", "").replaceAll("\n", "")

  def confirm(in: String, print: Boolean = false) = {

    val p = createParser()
    val g = createGenerator()

    // Don't do this, it messes up PP
//    val parsed = p.parse(in.replaceAll("\r\n", "").replaceAll("\n", ""))
    val parsed = p.parse(in)

    parsed match {
      case CParseSuccess(x) =>
        val after = g.generateTranslationUnit(x)
        if (print) {
          PPrinter.Color.log(x, width = 3, height = 1000)
        }
//        println(pretty)
        val mashed = after.mkString(" ", " ", " ")
        assert (clean(in) == clean(mashed))
//        if (in.replaceAll(" ", "") != mashed.replaceAll(" ", "")) {
//          assert(in == mashed)
//        }
      case CParseFail(x) =>
        println(parsed)
        assert (false)
    }

  }

    test("function simple") {
      val raw = """int main(int argc) { return 0; }"""
      confirm(raw)
    }

  test("function newlines") {
    val raw =
      """int main(int argc) {
        |return 0;
        |}""".stripMargin
    confirm(raw)
  }

  test("1") {
    confirm("""int main(int argc) {
                |    int hello;
                |    hello = 1 + 2;
                |    return 0;
                |}
                |""".stripMargin)

  }

  test("includes") {
    val raw = """#include "stdio.h"
                |#include<stdlib.h>
                |""".stripMargin
    pp.parse(raw) match {
      case CParseSuccess(v) =>
        val gen = gg.generateTranslationUnit(v)
        PPrinter.BlackWhite.log(v)
        val mashed = gen.mkString(" ", " ", " ")
        assert (clean(raw) == clean(mashed))
      case _ => assert(false)
    }
  }

  test("single linked list") {
    val raw = parsing.TestUtils.loadTestResource("/Snippet1.c")
    check(raw, print = true)
  }

  test("count while") {
    check("""int count()
            |{
            |    struct node *n;
            |    int c=0;
            |    n=head;
            |    while(n!=NULL)
            |    {
            |    n=n->next;
            |    c++;
            |    }
            |    return c;
            |}""".stripMargin)
  }

  test("simple") {
    check("int count() { return 0; }")
    check(
      """int count()
        |{
        |    return 0;
        |}""".stripMargin)
  }

  test("single linked list insert") {
    val raw = parsing.TestUtils.loadTestResource("/Snippet2.c")
    check(raw)
  }

  test("void addafter(int num, int loc)") {
    val raw = """void addafter(int num, int loc) {}"""
    check(raw, true)
  }

  test("int i,num") {
    val raw = """int  main()
                |{
                |    int i,num;
                |}""".stripMargin
    check(raw, true)

    TranslationUnit(
      List(
        FunctionDefinition(
          DeclarationSpecifiers(
            List(
              TypeSpecifierSimple(
                "int"
              )
            )
          ),
          Declarator(
            None,
            FunctionDeclaration(
              Identifier(
                "main"
              ),
              ParameterTypeList(
                List(

                ),
                false
              )
            )
          ),
          None,
          CompoundStatement(
            List(
              SimpleDeclaration(
                DeclarationSpecifiers(
                  List(
                    TypeSpecifierSimple(
                      "int"
                    )
                  )
                ),
                Some(
                  List(
                    DeclaratorEmpty(
                      Declarator(
                        None,
                        DirectDeclaratorOnly(
                          Identifier(
                            "i"
                          )
                        )
                      )
                    ),
                    DeclaratorEmpty(
                      Declarator(
                        None,
                        DirectDeclaratorOnly(
                          Identifier(
                            "num"
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  }
}
