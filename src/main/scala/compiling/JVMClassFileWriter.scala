package compiling

import java.io.{ByteArrayOutputStream, File, FileOutputStream, PrintWriter}
import java.nio.charset.StandardCharsets

import parsing.{CParseFail, CParseSuccess, CParser}
import pprint.PPrinter

import scala.io.Source

//class JVMClassFileWriter {
//
//  //  import scala.language.implicitConversions
//  //
//  //  private implicit def stringToBytes(x: String): Array[Byte] = (x + '\n').getBytes("UTF-8")
//
//  def process(input: Seq[JVMByteCode.ByteCode]): Seq[String] = {
//    implicit val genParams = GenParams()
//    input.flatMap(_.gen(genParams).split('\n'))
//  }
//
//  // https://docs.oracle.com/javase/specs/jvms/se6/html/ClassFile.doc.html
//  def write(input: Seq[JVMByteCode.ByteCode], packageName: String, className: String): Array[Byte] = {
//    val out = new ByteArrayOutputStream()
//
//    //https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.1
//    //ClassFile {
//    //    u4             magic; //CAFEBABE
//    val charset = StandardCharsets.UTF_8
//    out.write(0xca)
//    out.write(0xfe)
//    out.write(0xba)
//    out.write(0xbe)
//
//    /*
//    Java 1.2 uses major version 46
//    Java 1.3 uses major version 47
//    Java 1.4 uses major version 48
//    Java 5 uses major version 49
//    Java 6 uses major version 50
//    Java 7 uses major version 51
//    Java 8 uses major version 52
//    Java 9 uses major version 53
//    */
//
//    //    u2             minor_version;
//    out.write(0)
//    out.write(0)
//
//    //    u2             major_version;
//    out.write(0)
//    out.write(50)
//
//    //    u2             constant_pool_count;
//    // The value of the constant_pool_count item is equal to the number of entries in the constant_pool table plus one.
//    // The constant_pool table is indexed from 1 to constant_pool_count-1.
//    out.write(0)
//    out.write(3)
//
//    //    cp_info        constant_pool[constant_pool_count-1]; //string constants etc...
//    /*
//    The CONSTANT_Class_info structure is used to represent a class or an interface:
//
//        CONSTANT_Class_info {
//          u1 tag; // 7
//          u2 name_index; // CONSTANT_Utf8_info
//        }
//     */
//    out.write(7)
//    out.write(0)
//    out.write(2)
//
//    /*
//    CONSTANT_Utf8_info {
//    	u1 tag; // 1
//    	u2 length;
//    	u1 bytes[length];
//    }
//
//     */
//    out.write(1)
//    val fullClassName = packageName + "/" + className
//    out.write(0)
//    out.write(fullClassName.length)
//    out.write(fullClassName.getBytes(charset))
//
//    // Method
//    out.write(1)
//    val methodName = "main"
//    out.write(0)
//    out.write(methodName.length)
//    out.write(methodName.getBytes(charset))
//
//
//
//
//    //    u2             access_flags;
//    out.write(0)
//    out.write(0x0001) // public
//
//    //    u2             this_class;
//    // The value of the this_class item must be a valid index into the constant_pool table. The constant_pool entry at that index must be a CONSTANT_Class_info
//    // The constant_pool table is indexed from 1 to constant_pool_count-1.
//    out.write(0)
//    out.write(1)
//
//    //    u2             super_class;
//    // For a class, the value of the super_class item either must be zero or must be a valid index into the constant_pool table.
//    out.write(0)
//    out.write(0)
//
//    //    u2             interfaces_count;
//    out.write(0)
//    out.write(0)
//
//    //    u2             interfaces[interfaces_count];
//    out.write(0)
//    out.write(0)
//
//    //    u2             fields_count;
//    out.write(0)
//    out.write(0)
//
//    //    field_info     fields[fields_count];
//    // The field_info (§4.5) structures represent all fields, both class variables and instance variables, declared by this class or interface type.
//    out.write(0)
//    out.write(0)
//
//    //    u2             methods_count;
//    out.write(0)
//    out.write(1)
//
//    //    method_info    methods[methods_count];
//    // Each value in the methods table must be a method_info (§4.6) structure giving a complete description of a method in this class or interface.
//    /*
//
//    method_info {
//    	u2 access_flags;
//    	u2 name_index;
//    	u2 descriptor_index;
//    	u2 attributes_count;
//    	attribute_info attributes[attributes_count];
//    }
//
//
//     */
//
//    out.write(0)
//    out.write(1 | 8) // public static
//
//    // The value of the name_index item must be a valid index into the constant_pool table. The constant_pool entry at that index must be a CONSTANT_Utf8_info
//    out.write(0)
//    out.write(2)
//
//    //    u2             attributes_count;
//    out.write(0)
//    out.write(0)
//
//    //    attribute_info attributes[attributes_count];
//    out.write(0)
//    out.write(0)
//    //}
//
//    implicit val genParams = GenParams()
//    input.foreach(in => {
//      val gen = in.gen(genParams)
//      val bytes = gen.getBytes(charset)
//      out.write(bytes)
//      out.write('\n')
//    })
//
//
//    //    out.write(s"public class $packageName.$className {".)
//    //    out.write("}")
//    out.toByteArray
//  }
//
//}

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