package compiling

import java.io.{ByteArrayOutputStream, FileOutputStream}
import java.nio.charset.StandardCharsets

import compiling.JVMByteCode.GenParams
import parsing.{CParseFail, CParseSuccess, CParser}
import pprint.PPrinter

import scala.io.Source

class JVMClassFileWriter {
  import scala.language.implicitConversions

  private implicit def stringToBytes(x: String): Array[Byte] = (x + '\n').getBytes("UTF-8")

  def process(input: Seq[JVMByteCode.ByteCode]): Seq[String] = {
    implicit val genParams = GenParams()
    input.flatMap(_.gen(genParams).split('\n'))
  }

  def write(input: Seq[JVMByteCode.ByteCode], packageName: String, className: String): Array[Byte] = {
    val out = new ByteArrayOutputStream()

    //https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.1
    //ClassFile {
    //    u4             magic; //CAFEBABE
    val charset = StandardCharsets.UTF_8
    out.write("CAFEBABE".getBytes(charset))

    //    u2             minor_version;
    out.write(0)
    out.write(1)

    //    u2             major_version;
    out.write(0)
    out.write(0)

    //    u2             constant_pool_count;
    out.write(0)
    out.write(0)

    //    cp_info        constant_pool[constant_pool_count-1]; //string constants etc...
    //    u2             access_flags;
    out.write(0)
    out.write(0)

    //    u2             this_class;
    out.write(0)
    out.write(0)

    //    u2             super_class;
    out.write(0)
    out.write(0)

    //    u2             interfaces_count;
    out.write(0)
    out.write(0)

    //    u2             interfaces[interfaces_count];
    out.write(0)
    out.write(0)

    //    u2             fields_count;
    out.write(0)
    out.write(0)

    //    field_info     fields[fields_count];
    out.write(0)
    out.write(0)

    //    u2             methods_count;
    out.write(0)
    out.write(0)

    //    method_info    methods[methods_count];
    out.write(0)
    out.write(0)

    //    u2             attributes_count;
    out.write(0)
    out.write(0)

    //    attribute_info attributes[attributes_count];
    out.write(0)
    out.write(0)
    //}

    implicit val genParams = GenParams()
    input.foreach(in => {
      val gen = in.gen(genParams)
      val bytes = gen.getBytes(charset)
      out.write(bytes)
      out.write('\n')
    })


    out.write(s"public class $packageName.$className {")
    out.write("}")
    out.toByteArray
  }

}

object JVMClassFileWriter {
  def main(args: Array[String]) = {
    if (args.length != 2) {
      println("usage: program <c file> <output class file>")
    }
    else {
      val p = new CParser
      val g = new JVMByteCodeGenerator
      val i = new JVMByteCodeInterim
      val cw = new JVMClassFileWriter
      val cFilename = args(0)
      val classFilename = args(1)
      val in = Source.fromFile(cFilename, "UTF-8").getLines().mkString("\n")
      p.parse(in) match {
        case CParseSuccess(v) =>
          val interim = g.generateTranslationUnit(v)
          val generated = i.parse(interim)

          PPrinter.Color.log(v)
          PPrinter.Color.log(interim)
          PPrinter.Color.log(generated)

          val classFile = cw.write(generated, "test", "Test")
          val classOutputFile = new FileOutputStream(classFilename)
          classOutputFile.write(classFile)

//          println(classFile)
        case CParseFail(v)    =>
          println(s"Failed to parse: ${v}")
      }
    }
  }
}