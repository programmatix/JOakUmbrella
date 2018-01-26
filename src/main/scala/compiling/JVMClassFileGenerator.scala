package compiling

//import compiling.JVMByteCode._
//import compiling.JVMClassFileGenerator._
//
//import scala.collection.mutable.ArrayBuffer

//object JVMClassFileGenerator {
//  case class Variable(name: String, idx: Int)
//  case class FunctionContext(variables: Seq[Variable], currentVar: Option[Variable]) {
//  }
//
//  object FunctionContext {
//    def addVariable(fc: FunctionContext, v: Variable): FunctionContext = {
//      fc.copy(variables = fc.variables :+ v, currentVar = Some(v))
//    }
//  }
//}
//
//// Parses out the JVM instructions from JVMByteCodeGenerator and handles them
//class JVMClassFileGenerator {
//
//  def parse(in: Seq[JVMByteCode.Generated]): JVMClassFile = {
//    val out = ArrayBuffer.empty[JVMByteCode.ByteCode]
//
//    var fc = FunctionContext(Seq(), None)
////    val stack = mutable.Stack.empty[FunctionContext]
//
//
//    in.foreach {
//      case byteCode: JVMByteCode.ByteCode =>
//        out += byteCode
//
//      case command: JVMByteCode.Command   =>
//        command match {
//          case v: DeclareVariable =>
//            val variable = Variable(v.name.v, fc.variables.length)
//            fc = FunctionContext.addVariable(fc, variable)
//
//          case v: StoreExpressionInCurrentVar =>
//            fc.currentVar match {
//              case Some(currentVar) =>
//                out += istore(currentVar.idx)
//              case _ =>
//                throw JVMInterimBadState("Current variable is required but not available")
//            }
//
//          case v: DefineFunction =>
//            out += Function(v)
////            fc = FunctionContext(Seq(), Seq())
//        }
//
//
//      case v: Generated =>
//        println(s"Ignoring ${v}")
//
//    }
//
//    out.toVector
//  }
//}
