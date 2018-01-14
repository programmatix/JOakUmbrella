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

  def createParser() = new SimpleCFastParse

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
}
