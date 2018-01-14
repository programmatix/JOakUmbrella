package parser

import fastparse.core.Parsed
import org.scalatest.FunSuite

class CGeneratorSpec extends FunSuite {
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

  test("hello++") {
    val g = new CGenerator()
    val out = g.generate(PostfixExpressionPlusPlus(Identifier("hello")))
    assert(out(0) == "hello")
    assert(out(1) == "++")
  }

  test("hello[world]++") {
    val in = PostfixExpressionPlusPlus(PostfixExpressionIndex(Identifier("hello"), Identifier("world")))
    val g = new CGenerator()
    val out = g.generate(in)
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

  def confirm(in: String) = {
    def clean(s: String) = s.replaceAll(" ", "").replaceAll("\r\n", "").replaceAll("\n", "")

    val p = createParser()
    val g = createGenerator()

    val parsed = p.parse(in.replaceAll("\r\n", "").replaceAll("\n", ""))

    parsed match {
      case Parsed.Success(x, y) =>
        val after = g.generate(x)
        val mashed = after.mkString(" ", " ", " ")
        assert (clean(in) == clean(mashed))
//        if (in.replaceAll(" ", "") != mashed.replaceAll(" ", "")) {
//          assert(in == mashed)
//        }
      case Parsed.Failure(x, y, z) =>
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

}
