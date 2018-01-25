package CGenerator

import fastparse.core.Parsed
import generating.{CGeneratedPrinter, CGenerator}
import generating.CGenerator._
import org.scalatest.FunSuite
import parsing._
import pprint.PPrinter

class CGeneratorSpec extends FunSuite {
  private val gg = createGenerator()
  private val pp = createParser()

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

  // Minor diffs
  ignore("hello++") {
    val g = new CGenerator()
    val out = g.generateExpression(PostfixExpressionPlusPlus(Identifier("hello")))
    assert(out(0) == GenStrings(List("hello")))
    assert(out(1) == GenStrings(List("++")))
  }

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
//          PPrinter.Color.log(after, width = 10, height = 1000)
          println(after)
        }
//        println(pretty)
        val mashed = CGeneratedPrinter.print(after)
        assert (clean(in) == clean(mashed))
//        if (in.replaceAll(" ", "") != mashed.replaceAll(" ", "")) {
//          assert(in == mashed)
//        }
      case CParseFail(x) =>
        println(parsed)
        assert (false)
    }

  }

  def confirmSnippet(in: String, print: Boolean = false) = {

    val p = createParser()
    val g = createGenerator()

    // Don't do this, it messes up PP
    //    val parsed = p.parse(in.replaceAll("\r\n", "").replaceAll("\n", ""))
    val parsed = p.parseSnippet(in)

    parsed match {
      case CParseSuccess(x) =>
        val after = g.generateSeqBlockItem(x)
        if (print) {
          PPrinter.Color.log(x, width = 3, height = 1000)
          println(after)
        }
        //        println(pretty)
        val mashed = CGeneratedPrinter.print(after)
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
        val mashed = CGeneratedPrinter.print(gen)
        assert (clean(raw) == clean(mashed))
      case _ => assert(false)
    }
  }

  // Minor diffs
  ignore("single linked list") {
    val raw = parsing.TestUtils.loadTestResource("/Snippet1.c")
    check(raw, print = true)
  }

  // Minor diffs
  ignore("count while") {
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
            |}""".stripMargin, true)
  }

  test("simple") {
    check("int count() { return 0; }")
    check(
      """int count()
        |{
        |    return 0;
        |}""".stripMargin)
  }

  // Minor diffs
  ignore("single linked list insert") {
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
  }

  test("while") {
    val raw = """    while(1)
                |    {
                |    c++;
                |    }
                |""".stripMargin
    confirmSnippet(raw, true)
  }
}
