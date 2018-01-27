package jvm

import java.io.{ByteArrayOutputStream, File, FileOutputStream, PrintWriter}
import java.nio.charset.StandardCharsets

import compiling.JVMByteCodeGenerator
import parsing.{CParseFail, CParseSuccess, CParser}
import pprint.PPrinter

import scala.io.Source
;

object JVMClassFileWriter {
  def main(args: Array[String]) = {
    if (args.length != 2) {
      println("usage: program <c file> <output class file>")
    }
    else {
      val p = new CParser
//      val i = new JVMClassFileGenerator
//      val cw = new JVMClassFileWriter
      val cFilename = args(0)

//      val classFilename = args(1)

      val in = Source.fromFile(cFilename, "UTF-8").getLines().mkString("\n")
      p.parse(in) match {
        case CParseSuccess(v) =>
          PPrinter.Color.log(v)

          val className = (new File(cFilename)).getName.stripSuffix(".c")
          val cf = new JVMClassFileBuilderForWriting(50, 0, None, className)
          val g = new JVMByteCodeGenerator(cf)
          g.generateTranslationUnit(v)
//          val generated = i.parse(interim)
          val byteCode = new ByteArrayOutputStream()
          cf.write(byteCode, StandardCharsets.UTF_8)

//          val byteCode = cw.process(generated)

//          PPrinter.Color.log(interim)
//          PPrinter.Color.log(generated)
          PPrinter.Color.log(byteCode)

//          val classFile = cw.write(byteCode.toByteArray, "test", "Test")
          val classFilename = cf.className + ".class"
          val classOutputFile = new FileOutputStream(classFilename)
          classOutputFile.write(byteCode.toByteArray)
          classOutputFile.close()

          val classOutputBytecodeFile = new PrintWriter(new FileOutputStream(classFilename.replace(".class", ".bc")))
          cf.write(classOutputBytecodeFile, StandardCharsets.UTF_8)
//          classOutputBytecodeFile.write(sb.toString)
          classOutputBytecodeFile.close()

        //          println(classFile)
        case CParseFail(v) =>
          println(s"Failed to parse: ${v}")
      }
    }
  }
}
