package parser

import scala.collection.mutable.ArrayBuffer

class CGenerator {
  def generate(exp: Expression): Seq[String] = {
    val out = ArrayBuffer.empty[String]
    exp match {
      case v: Identifier =>
        out += v.v
      case v: PostfixExpressionIndex =>
        out ++= generate(v.v1)
        out += "["
        out ++= generate(v.v2)
        out += "]"
      case v: PostfixExpressionMinusMinus =>
        out ++= generate(v.v1)
        out += "--"
      case v: PostfixExpressionPlusPlus =>
        out ++= generate(v.v1)
        out += "++"
    }
    out
  }
}
