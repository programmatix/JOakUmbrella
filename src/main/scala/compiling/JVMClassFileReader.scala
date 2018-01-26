package compiling

import java.io._
import java.nio.charset.StandardCharsets

import compiling.JVMClassFileTypes._

import scala.collection.mutable.ArrayBuffer

// Java doesn't support unsigned bytes
object JVMClassFileReaderUtils {
  def readShort(in: ByteArrayInputStream): Int = {
    (readByte(in) << 8) | readByte(in)
  }

  def readByte(in: ByteArrayInputStream): Int = {
    val rawByte = in.read()
    rawByte & 0xFF
  }

  def readInt(in: ByteArrayInputStream): Int = {
    (readByte(in) << 24) | (readByte(in) << 16) | (readByte(in) << 8) | readByte(in)
  }

  def readFloat(in: ByteArrayInputStream): Float = {
    (readByte(in) << 24) | (readByte(in) << 16) | (readByte(in) << 8) | readByte(in)
  }

  def readDouble(in: ByteArrayInputStream): Double = {
    (readByte(in) << 56) | (readByte(in) << 48) | (readByte(in) << 40) | (readByte(in) << 32) | (readByte(in) << 24) | (readByte(in) << 16) | (readByte(in) << 8) | readByte(in)
  }

  def readLong(in: ByteArrayInputStream): Long = {
    (readByte(in) << 56) | (readByte(in) << 48) | (readByte(in) << 40) | (readByte(in) << 32) | (readByte(in) << 24) | (readByte(in) << 16) | (readByte(in) << 8) | readByte(in)
  }


}

// Reads in a .class file into a JVMClassFileBuilder
object JVMClassFileReader {
  private def badParse(in: ByteArrayInputStream, err: String): Unit = {
    println("Invalid .class file: " + err)
    throw new RuntimeException()
  }

  private def readShort(in: ByteArrayInputStream): Int = JVMClassFileReaderUtils.readShort(in)

  private def readByte(in: ByteArrayInputStream): Int = JVMClassFileReaderUtils.readByte(in)

  private def readInt(in: ByteArrayInputStream): Int = JVMClassFileReaderUtils.readInt(in)

  private def readFloat(in: ByteArrayInputStream): Float = JVMClassFileReaderUtils.readFloat(in)

  private def readDouble(in: ByteArrayInputStream): Double = JVMClassFileReaderUtils.readDouble(in)

  private def readLong(in: ByteArrayInputStream): Long = JVMClassFileReaderUtils.readLong(in)

  private def good(in: ByteArrayInputStream, msg: String): Unit = {
    println(msg)
  }

  private def good(in: ByteArrayInputStream, indent: Int, msg: String): Unit = {
    println((" " * indent * 2) + msg)
  }

  private def addConstant(cf: JVMClassFileBuilder, constant: CONSTANT, idx: Int) = {
    println(s"Constant $idx: $constant")
    cf.addConstant(constant)
  }

  private def goodConstant(in: ByteArrayInputStream, indent: Int, cf: JVMClassFileBuilder, name: String, value: Int): Unit = {
    try {
      val constant = cf.getConstant(value)
      good(in, s"${" " * indent * 2}$name = $value (${constant})")
    }
    catch {
      case e: Throwable =>
        badParse(in, s"$name = $value (constant idx $value outside pool!!)")
    }
  }


  private def printCode(in: ByteArrayInputStream, cf: JVMClassFileBuilder, indent: Int, code: Array[Byte]): Unit = {
    var idx = 0
    val stream = new ByteArrayInputStream(code)

    while (idx < code.length) {

      val opcodehex = JVMClassFileReaderUtils.readByte(stream)

      val opcode = JVMOpCodes.getOpcode(opcodehex)
      println((" " * indent * 2) + opcode.gen(stream))
      idx += opcode.lengthInBytes
    }
  }

  private def readAttribute(in: ByteArrayInputStream, cf: JVMClassFileBuilder, indent: Int): Code_attribute = {
    val attribute_name_index = readShort(in)
      goodConstant(in, indent, cf, "attribute_name_index", attribute_name_index)

    val attribute_length = readInt(in)
    good(in, indent, s"attribute_length = $attribute_length")

    val max_stack = readShort(in)
    good(in, indent, s"max_stack = $max_stack")

    val max_locals = readShort(in)
    good(in, indent, s"max_locals = $max_locals")

    var code_length = readInt(in)
    good(in, indent, s"code_length = $code_length")

    val temp = new ByteArrayOutputStream()

    while (code_length > 0) {
      val b = in.read().toByte
      temp.write(b)
      code_length -= 1
    }
    val code = temp.toByteArray
    printCode(in, cf, indent + 1, code)
//    println(code)
//    println("\n")

    var exception_table_length = readShort(in)
    good(in, indent, s"exception_table_length = $exception_table_length")
    // TODO
    assert (exception_table_length == 0)

    var attributes_count = readShort(in)
    good(in, indent, s"attributes_count = $attributes_count")

    for(idx <- Range(0, attributes_count)) {
      // Can be LineNumberTable or LocalVariableTable, both optional and for debugging
      val attribute_name_index = readShort(in)
      var attribute_length = readInt(in)
      good(in, s"Skipping attribute len ${attribute_length} attribute_name_index = $attribute_name_index (${cf.getConstant(attribute_name_index)})")
      while (attribute_length > 0) {
        in.read()
        attribute_length -= 1
      }
    }

    Code_attribute(attribute_name_index,
      max_stack,
      max_locals,
      Seq(),
      code,
      Seq()
    )
  }

  private def readMethod(in: ByteArrayInputStream, cf: JVMClassFileBuilder, indentIn: Int): Unit = {
    good(in, indentIn, "Method:")
    val indent = indentIn + 1

    val access_flags = readShort(in)
    good(in, indent, s"access_flags = $access_flags")

    val name_index = readShort(in)
    good(in, indent,s"name_index = $name_index (${cf.getConstant(name_index)})")

    val descriptor_index = readShort(in)
    good(in, indent, s"descriptor_index = $descriptor_index (${cf.getConstant(descriptor_index)})")

    val attributes_count = readShort(in)
    good(in, indent, s"attributes_count = $attributes_count")

    val attributes = ArrayBuffer.empty[Code_attribute]
    for (idx <- Range(0, attributes_count)) {
      good(in, indent, s"Method Attribute $idx")
      attributes += readAttribute(in, cf, indent + 1)
    }

    method_info(
      access_flags,
      name_index,
      descriptor_index,
      attributes
    )
  }

  private def read(in: ByteArrayInputStream): Unit = {
    if (in.read != 0xca) badParse(in,"Initial bytes are not 'cafebabe'")
    if (in.read != 0xfe) badParse(in,"Initial bytes are not 'cafebabe'")
    if (in.read != 0xba) badParse(in,"Initial bytes are not 'cafebabe'")
    if (in.read != 0xbe) badParse(in,"Initial bytes are not 'cafebabe'")

    val charset = StandardCharsets.UTF_8

    val minor_version = readShort(in)
    val major_version = readShort(in)

    good(in, s"Version: major=$major_version minor=$minor_version")

    val constantPoolCount = readShort(in)

    good(in, s"Constant pool count = $constantPoolCount")

    val cf = new JVMClassFileBuilder(major_version, minor_version, "test", "test")
    
    for(idx <- Range(1, constantPoolCount)) {
      val tag = readByte(in)
      tag match {
        case 1 =>
          var len = readShort(in)
          val temp = new ByteArrayOutputStream()

          while (len > 0) {
            temp.write(in.read().toByte)
            len -= 1
          }
          val value = new String(temp.toByteArray, charset)
          addConstant(cf, CONSTANT_Utf8_info(value), idx)
        case 3 =>
          val value = readInt(in)
          addConstant(cf, CONSTANT_Integer_info(value), idx)
        case 4 =>
          val value = readFloat(in)
          addConstant(cf, CONSTANT_Float_info(value), idx)
        case 5 =>
          val value = readLong(in)
          addConstant(cf, CONSTANT_Long_info(value), idx)
        case 6 =>
          val value = readDouble(in)
          addConstant(cf, CONSTANT_Double_info(value), idx)
        case 7 =>
          val index = readShort(in)
          addConstant(cf, CONSTANT_Class_info(index), idx)
        case 8 =>
          val index1 = readShort(in)
          addConstant(cf, CONSTANT_String_info(index1), idx)
        case 9 =>
          val index1 = readShort(in)
          val index2 = readShort(in)
          addConstant(cf, CONSTANT_Fieldref_info(index1, index2), idx)
        case 10 =>
          val index1 = readShort(in)
          val index2 = readShort(in)
          addConstant(cf, CONSTANT_Methodref_info(index1, index2), idx)
        case 11 =>
          val index1 = readShort(in)
          val index2 = readShort(in)
          addConstant(cf, CONSTANT_InterfaceMethodref_info(index1, index2), idx)
        case 12 =>
          val index1 = readShort(in)
          val index2 = readShort(in)
          addConstant(cf, CONSTANT_NameAndType_info(index1, index2), idx)
      }
    }

    val access_flags = readShort(in)
    good(in, s"access_flags = $access_flags")

    val this_class = readShort(in)
    good(in, s"this_class = $this_class")

    val super_class = readShort(in)
    good(in, s"super_class = $super_class")

    val interfaces_count = readShort(in)
    good(in, s"interfaces_count = $interfaces_count")
    assert(interfaces_count == 0)

    for (idx <- Range(0, interfaces_count)) {

    }

    val fields_count = readShort(in)
    good(in, s"fields_count = $fields_count")
    assert(fields_count == 0)

    for (idx <- Range(0, fields_count)) {

    }

    val methods_count = readShort(in)
    good(in, s"methods_count = $methods_count")
    for (idx <- Range(0, methods_count)) {
      readMethod(in, cf, 0)
    }

    val attributes_count = readShort(in)
    good(in, s"attributes_count = $attributes_count")

    for (idx <- Range(0, attributes_count)) {
//      println("Attribute:")
      val attribute_name_index = readShort(in)
      var attribute_length = readInt(in)
      good(in, s"Skipping attribute $idx len ${attribute_length} attribute_name_index = $attribute_name_index (${cf.getConstant(attribute_name_index)})")
      while (attribute_length > 0) {
        in.read()
        attribute_length -= 1
      }
    }

    assert(in.read() == -1)
  }

  def main(args: Array[String]) = {
    if (args.length != 1) {
      println("usage: program <.class file>")
    }
    else {
      val file = new File(args(0))
      val fileContent = new Array[Byte](file.length.asInstanceOf[Int])
      new FileInputStream(file).read(fileContent)
      val lines = new ByteArrayInputStream(fileContent)
      read(lines)
    }
  }

}
