package compiling

import java.io.ByteArrayOutputStream
import java.nio.charset.Charset

import compiling.JVMByteCode.{GenParams, Types}
import compiling.JVMClassFileTypes._
import parsing.Identifier

import scala.scalajs.niocharset.StandardCharsets


class JVMClassFileBuilderForWriting(
                                     /*
                                        Java 1.2 uses major version 46
                                        Java 1.3 uses major version 47
                                        Java 1.4 uses major version 48
                                        Java 5 uses major version 49
                                        Java 6 uses major version 50
                                        Java 7 uses major version 51
                                        Java 8 uses major version 52
                                        Java 9 uses major version 53
                                      */
                                     val jvmMajorVersion: Int,
                                     val jvmMinorVersion: Int,
                                     val packageName: Option[String],
                                     val className: String

                                   ) extends JVMClassFileBuilder {
  private val STRING_CODE = addUTF8("Code")

  private val fullClassName = packageName match {
    case Some(v) => v + "/" + className
    case _       => className
  }

  private val thisConstantIdx = addConstant(CONSTANT_Class_info(addUTF8(fullClassName)))

  // Subclass Object
  private val superClassConstantIdx = addConstant(CONSTANT_Class_info(addUTF8("java/lang/Object")))


  def addFunction(name: Identifier, variables: Seq[JVMByteCode.DeclareVariable], ret: Types, definition: Seq[JVMByteCode.Generated]): Unit = {
    //    val code = attribute_info(
    //      addUTF8("AttributeForCodeFor" + name.v),
    //    )

    val genParams = GenParams()
    val codeBuffer = new ByteArrayOutputStream()
    val charset = StandardCharsets.UTF_8
    var codeLength = 0
    definition.foreach(v => codeLength += v.write(codeBuffer, charset, genParams))
    assert(codeLength < 65536)
    val codeBytes = codeBuffer.toByteArray

    val code = Code_attribute(
      STRING_CODE,
      maxStack = 1,
      maxLocals = 0,
      definition,
      codeBytes,
      Seq()
    )


    val methodDescriptor = JVMClassFileBuilderUtils.createMethodDescriptor(ret, variables.map(_.types))

    val method = method_info(
      0 | 8, // Public static
      // https://docs.oracle.com/javase/specs/jvms/se6/html/Concepts.doc.html#21410
      addUTF8(name.v),
      addUTF8(methodDescriptor),
      Seq(code)
    )

    methods += method
  }

  def write(out: ByteArrayOutputStream, charset: Charset): Unit = {
    //    ClassFile {
    //      u4 magic;
    out.write(0xca)
    out.write(0xfe)
    out.write(0xba)
    out.write(0xbe)

    //      u2 minor_version;
    JVMClassFileBuilderUtils.writeShort(out, jvmMinorVersion)

    //      u2 major_version;
    JVMClassFileBuilderUtils.writeShort(out, jvmMajorVersion)

    //      u2 constant_pool_count;
    JVMClassFileBuilderUtils.writeShort(out, constantPool.length + 1)
    //    JVMClassFileBuilderUtils.writeShort(out, 2)

    //      cp_info constant_pool[constant_pool_count-1];
    constantPool.foreach(_.write(out, charset))
    //    constantPool.head.write(out, charset)

    //      u2 access_flags;
    JVMClassFileBuilderUtils.writeShort(out, 1) // public

    //      u2 this_class;
    JVMClassFileBuilderUtils.writeShort(out, thisConstantIdx)

    //      u2 super_class;
    JVMClassFileBuilderUtils.writeShort(out, superClassConstantIdx)

    //      u2 interfaces_count;
    //      u2 interfaces[interfaces_count];
    JVMClassFileBuilderUtils.writeShort(out, 0)

    //      u2 fields_count;
    //      field_info fields[fields_count];
    JVMClassFileBuilderUtils.writeShort(out, 0)

    //      u2 methods_count;
    JVMClassFileBuilderUtils.writeShort(out, methods.length)
    //    JVMClassFileBuilderUtils.writeShort(out, 0)

    //      method_info methods[methods_count];
    methods.foreach(_.write(out, charset))

    //      u2 attributes_count;
    //      attribute_info attributes[attributes_count];
    JVMClassFileBuilderUtils.writeShort(out, 0)

    out.close()
  }
}

//object JVMClassFile {
//  def process(input: Seq[JVMByteCode.ByteCode]): JVMClassFile = {
//    input.foreach(in => {
//      in match {
//        case v: ByteCodeComplex =>
//        case v =>
//      }
//    })
//  }
//}