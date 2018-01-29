package jvm

import java.io._
import java.nio.charset.StandardCharsets
import java.nio.file.Files

import compiling.JVMOpCodes
import jvm.JVMByteCode.{JVMOpCodeWithArgs, JVMVarInt}
import jvm.JVMClassFileTypes._

import scala.collection.mutable.ArrayBuffer

// Java doesn't support unsigned bytes
object JVMClassFileReaderUtils {
  // Quick two's complement refresher:
  // Way of representing integers in binary.  For a 4 bit number:
  // 0000 = 0, 0001 = 1, 0010 = 2 etc.
  // 1111 = -1, 1110 = -2, 1101 = -3
  // Avoids using a sign bit, which also avoids pain of having two zeros - 0000 (+0) and 1000 (-0)
  // Makes the maths easier:
  // E.g. on a 4bit number,  2 (0010) and -2 (1110) = 10000, but the top bit is overflow, so it leaves 0 : -2+2=0
  // For an 8 bit number, twos complement is (2^8 - number)
  // Negating a number (reversing the sign) is performed by taking its twos complement
  @Deprecated
  def twosComplementRepresentation(in: Int, bits: Int): Int = {
    if (in == 0) 0
    else if (in > 0) {
      // The top bit can't be set
      assert ((in & (2 ^ bits)) == 0)
      in
    }
    else {
      0
    }
  }

  // Takes a raw byte (say, read from an input stream), and returns it as a 4-byte int in twos complement representation
//  def extendByteAsTwosComplement(in: Byte): Int = {
//    val topBitSet = (in & 0x80) != 0
//    if (topBitSet) {
//      val out = 0xffffff00 | in
//      out
//    }
//    else in
//  }

  def extendByteAsTwosComplement(in: Int): Int = {
    val topBitSet = (in & 0x80) != 0
    if (topBitSet) {
      val out = 0xffffff00 | in
      out
    }
    else in
  }

  def extendShortAsTwosComplement(in: Int): Int = {
    val topBitSet = (in & 0x8000) != 0
    if (topBitSet) {
      val out = 0xffff0000 | in
      out
    }
    else in
  }

  // Java and bytes:
  // Integer.parseInt("11110111", 2) == 247, not -9, e.g. it doesn't do the twos complement.  Use extendByteAsTwosComplement
  // Raw file is 0xffff -> in.read() is 255

  def readByteUnsigned(in: ByteArrayInputStream): Int = {
    // Returns 0-255, or -1 if end of stream reached
    val rawByte = in.read()
    rawByte
  }

  def readByteTwosComplement(in: ByteArrayInputStream): Int = {
    // Returns 0-255, or -1 if end of stream reached
    val rawByte = in.read()
    extendByteAsTwosComplement(rawByte)
  }

  def readShortTwosComplement(in: ByteArrayInputStream): Int = {
    extendShortAsTwosComplement(readByteTwosComplement(in) << 8) | readByteTwosComplement(in)
  }

  def readIntTwosComplement(in: ByteArrayInputStream): Int = {
    (readByteUnsigned(in) << 24) | (readByteUnsigned(in) << 16) | (readByteUnsigned(in) << 8) | readByteUnsigned(in)
  }

  def readFloatTwosComplement(in: ByteArrayInputStream): Float = {
    (readByteUnsigned(in) << 24) | (readByteUnsigned(in) << 16) | (readByteUnsigned(in) << 8) | readByteUnsigned(in)
  }

  def readDoubleTwosComplement(in: ByteArrayInputStream): Double = {
    (readByteUnsigned(in) << 56) | (readByteUnsigned(in) << 48) | (readByteUnsigned(in) << 40) | (readByteUnsigned(in) << 32) | (readByteUnsigned(in) << 24) | (readByteUnsigned(in) << 16) | (readByteUnsigned(in) << 8) | readByteUnsigned(in)
  }

  def readLongTwosComplements(in: ByteArrayInputStream): Long = {
    (readByteUnsigned(in) << 56) | (readByteUnsigned(in) << 48) | (readByteUnsigned(in) << 40) | (readByteUnsigned(in) << 32) | (readByteUnsigned(in) << 24) | (readByteUnsigned(in) << 16) | (readByteUnsigned(in) << 8) | readByteUnsigned(in)
  }


}

// Reads in a .class file into a JVMClassFileBuilder
object JVMClassFileReader {
  private def badParse(in: ByteArrayInputStream, err: String): Unit = {
    println("Invalid .class file: " + err)
    throw new RuntimeException()
  }

  private def readShort(in: ByteArrayInputStream): Int = JVMClassFileReaderUtils.readShortTwosComplement(in)

  private def readByte(in: ByteArrayInputStream): Int = JVMClassFileReaderUtils.readByteTwosComplement(in)

  private def readInt(in: ByteArrayInputStream): Int = JVMClassFileReaderUtils.readIntTwosComplement(in)

  private def readFloat(in: ByteArrayInputStream): Float = JVMClassFileReaderUtils.readFloatTwosComplement(in)

  private def readDouble(in: ByteArrayInputStream): Double = JVMClassFileReaderUtils.readDoubleTwosComplement(in)

  private def readLong(in: ByteArrayInputStream): Long = JVMClassFileReaderUtils.readLongTwosComplements(in)

  private def good(in: ByteArrayInputStream, msg: String): Unit = {
    println(msg)
  }

  private def good(in: ByteArrayInputStream, indent: Int, msg: String): Unit = {
    println((" " * indent * 2) + msg)
  }

  private def addConstant(params: ReadParams, cf: JVMClassFileBuilderForReading, constant: Constant, idx: Int) = {
    if (params.verbose) println(s"Constant $idx: $constant")
    cf.addConstant(constant)
  }

  private def goodConstant(in: ByteArrayInputStream, indent: Int, cf: JVMClassFileBuilderForReading, name: String, value: Int): Unit = {
    try {
      val constant = cf.getConstant(value)
      good(in, s"${" " * indent * 2}$name = $value (${constant})")
    }
    catch {
      case e: Throwable =>
        badParse(in, s"$name = $value (constant idx $value outside pool!!)")
    }
  }


  private def printCode(in: ByteArrayInputStream, cf: JVMClassFileBuilderForReading, indent: Int, code: Array[Byte]): Unit = {
    var idx = 0
    val stream = new ByteArrayInputStream(code)

    while (idx < code.length) {

      // Can't find where this is documented, but opcode is unsigned rather than usual signed bytes
      val opcodehex = JVMClassFileReaderUtils.readByteUnsigned(stream)

      val opcode = JVMOpCodes.getOpcode(opcodehex)
      println((" " * indent * 2) + opcode.gen(stream))
      idx += opcode.lengthInBytes
    }
  }

  private def readCode(in: ByteArrayInputStream, cf: JVMClassFileBuilderForReading, indent: Int, code: Array[Byte]): Vector[JVMOpCodeWithArgs] = {
    var idx = 0
    val stream = new ByteArrayInputStream(code)
    val out = ArrayBuffer.empty[JVMOpCodeWithArgs]

    while (idx < code.length) {

      // Can't find where this is documented, but opcode is unsigned rather than usual signed bytes
      val opcodehex = JVMClassFileReaderUtils.readByteUnsigned(stream)

      val opcode = JVMOpCodes.getOpcode(opcodehex)

      if (opcode.unsupported) {
        badParse(in, s"Cannot handle opcode ${opcode} yet")
      }

      //      println((" " * indent * 2) + opcode.gen(stream))

      val args = ArrayBuffer.empty[Int]
      for (argIdx <- opcode.args.indices) {
        val value = opcode.args(argIdx).lengthBytes match {
          case 1 => JVMClassFileReaderUtils.readByteTwosComplement(stream)
          case 2 => JVMClassFileReaderUtils.readShortTwosComplement(stream)
          case 4 => JVMClassFileReaderUtils.readIntTwosComplement(stream)
        }
        args += value
      }

      out += JVMByteCode.JVMOpCodeWithArgs(opcode, args.map(v => JVMVarInt(v)).toArray)

      idx += opcode.lengthInBytes


    }

    out.toVector
  }


  private def readAttribute(params: ReadParams, in: ByteArrayInputStream, cf: JVMClassFileBuilderForReading, indent: Int): CodeAttribute = {
    val attribute_name_index = readShort(in)
    if (params.verbose) goodConstant(in, indent, cf, "attribute_name_index", attribute_name_index)

    val attribute_length = readInt(in)
    if (params.verbose) good(in, indent, s"attribute_length = $attribute_length")

    val max_stack = readShort(in)
    if (params.verbose) good(in, indent, s"max_stack = $max_stack")

    val max_locals = readShort(in)
    if (params.verbose) good(in, indent, s"max_locals = $max_locals")

    var code_length = readInt(in)
    if (params.verbose) good(in, indent, s"code_length = $code_length")

    val temp = new ByteArrayOutputStream()

    while (code_length > 0) {
      val b = in.read().toByte
      temp.write(b)
      code_length -= 1
    }
    val code = temp.toByteArray
    val opcodes = readCode(in, cf, indent + 1, code)

    var exception_table_length = readShort(in)
    if (params.verbose) good(in, indent, s"exception_table_length = $exception_table_length")
    // TODO
    assert(exception_table_length == 0)

    var attributes_count = readShort(in)
    if (params.verbose) good(in, indent, s"attributes_count = $attributes_count")

    for (idx <- Range(0, attributes_count)) {
      // Can be LineNumberTable or LocalVariableTable, both optional and for debugging
      val attribute_name_index = readShort(in)
      var attribute_length = readInt(in)
      if (params.verbose) good(in, s"${" " * indent * 2}Skipping attribute len ${attribute_length} attribute_name_index = $attribute_name_index (${cf.getConstant(attribute_name_index)})")
      while (attribute_length > 0) {
        in.read()
        attribute_length -= 1
      }
    }

    CodeAttribute(attribute_name_index,
      max_stack,
      max_locals,
      opcodes,
      code,
      Seq()
    )
  }

  private def readMethod(params: ReadParams, in: ByteArrayInputStream, cf: JVMClassFileBuilderForReading, indentIn: Int): MethodInfo = {
    if (params.verbose) good(in, indentIn, "Method:")
    val indent = indentIn + 1

    val access_flags = readShort(in)
    if (params.verbose) good(in, indent, s"access_flags = $access_flags")

    val name_index = readShort(in)
    if (params.verbose) good(in, indent, s"name_index = $name_index (${cf.getConstant(name_index)})")

    val descriptor_index = readShort(in)
    if (params.verbose) good(in, indent, s"descriptor_index = $descriptor_index (${cf.getConstant(descriptor_index)})")

    val attributes_count = readShort(in)
    if (params.verbose) good(in, indent, s"attributes_count = $attributes_count")

    val attributes = ArrayBuffer.empty[CodeAttribute]
    for (idx <- Range(0, attributes_count)) {
      if (params.verbose) good(in, indent, s"Method Attribute $idx")
      attributes += readAttribute(params: ReadParams, in, cf, indent + 1)
    }

    MethodInfo(
      access_flags,
      name_index,
      descriptor_index,
      attributes
    )
  }

  case class ReadParams(verbose: Boolean = false)

  def read(file: File, params: ReadParams): Option[JVMClassFile] = {
    val bytes = Files.readAllBytes(file.toPath)
    val in = new ByteArrayInputStream(bytes)
    read(file.getCanonicalPath, in, params)
  }

  def read(fullPath: String, in: ByteArrayInputStream, params: ReadParams): Option[JVMClassFile] = {
    try {
      if (in.read != 0xca) badParse(in, "Initial bytes are not 'cafebabe'")
      if (in.read != 0xfe) badParse(in, "Initial bytes are not 'cafebabe'")
      if (in.read != 0xba) badParse(in, "Initial bytes are not 'cafebabe'")
      if (in.read != 0xbe) badParse(in, "Initial bytes are not 'cafebabe'")

      val charset = StandardCharsets.UTF_8

      val minor_version = readShort(in)
      val major_version = readShort(in)

      if (params.verbose) good(in, s"Version: major=$major_version minor=$minor_version")

      val constantPoolCount = readShort(in)

      if (params.verbose) good(in, s"Constant pool count = $constantPoolCount")

      val cf = new JVMClassFileBuilderForReading(fullPath, major_version, minor_version, Some("test"), "test")

      for (idx <- Range(1, constantPoolCount)) {
        val tag = readByte(in)
        tag match {
          case 1  =>
            var len = readShort(in)
            val temp = new ByteArrayOutputStream()

            while (len > 0) {
              temp.write(in.read().toByte)
              len -= 1
            }
            val value = new String(temp.toByteArray, charset)
            addConstant(params, cf, ConstantUtf8(value), idx)
          case 3  =>
            val value = readInt(in)
            addConstant(params, cf, ConstantInteger(value), idx)
          case 4  =>
            val value = readFloat(in)
            addConstant(params, cf, ConstantFloat(value), idx)
          case 5  =>
            val value = readLong(in)
            addConstant(params, cf, ConstantLong(value), idx)
          case 6  =>
            val value = readDouble(in)
            addConstant(params, cf, ConstantDouble(value), idx)
          case 7  =>
            val index = readShort(in)
            addConstant(params, cf, ConstantClass(index), idx)
          case 8  =>
            val index1 = readShort(in)
            addConstant(params, cf, ConstantString(index1), idx)
          case 9  =>
            val index1 = readShort(in)
            val index2 = readShort(in)
            addConstant(params, cf, ConstantFieldref(index1, index2), idx)
          case 10 =>
            val index1 = readShort(in)
            val index2 = readShort(in)
            addConstant(params, cf, ConstantMethodref(index1, index2), idx)
          case 11 =>
            val index1 = readShort(in)
            val index2 = readShort(in)
            addConstant(params, cf, ConstantInterfaceMethodref(index1, index2), idx)
          case 12 =>
            val index1 = readShort(in)
            val index2 = readShort(in)
            addConstant(params, cf, ConstantNameAndType(index1, index2), idx)
        }
      }

      val access_flags = readShort(in)
      if (params.verbose) good(in, s"access_flags = $access_flags")

      val this_class = readShort(in)
      if (params.verbose) good(in, s"this_class = $this_class")

      val super_class = readShort(in)
      if (params.verbose) good(in, s"super_class = $super_class")

      val interfaces_count = readShort(in)
      if (params.verbose) good(in, s"interfaces_count = $interfaces_count")
      assert(interfaces_count == 0)

      for (idx <- Range(0, interfaces_count)) {

      }

      val fields_count = readShort(in)
      if (params.verbose) good(in, s"fields_count = $fields_count")
      assert(fields_count == 0)

      for (idx <- Range(0, fields_count)) {

      }

      val methods_count = readShort(in)
      if (params.verbose) good(in, s"methods_count = $methods_count")
      for (idx <- Range(0, methods_count)) {
        val method = readMethod(params, in, cf, 0)
        cf.addMethod(method)
      }

      val attributes_count = readShort(in)
      if (params.verbose) good(in, s"attributes_count = $attributes_count")

      for (idx <- Range(0, attributes_count)) {
        val attribute_name_index = readShort(in)
        var attribute_length = readInt(in)
        if (params.verbose) good(in, s"Skipping attribute $idx len ${attribute_length} attribute_name_index = $attribute_name_index (${cf.getConstant(attribute_name_index)})")
        while (attribute_length > 0) {
          in.read()
          attribute_length -= 1
        }
      }

      assert(in.read() == -1)

      Some(cf.makeImmutable())
    }
    catch {
      case e: Throwable => None
    }
  }

  def mainReader(args: Array[String]): Unit = {
    if (args.length != 1) {
      println("usage: program <.class file>")
    }
    else {
      val file = new File(args(0))
      val fileContent = new Array[Byte](file.length.asInstanceOf[Int])
      new FileInputStream(file).read(fileContent)
      val lines = new ByteArrayInputStream(fileContent)
      read(file.getCanonicalPath, lines, ReadParams(verbose = true))
    }
  }

}
