package jvm

import java.io._
import java.nio.charset.StandardCharsets

import compiling.JVMOpCodes
import jvm.JVMByteCode.{JVMOpCodeWithArgs, JVMVarInt}
import jvm.JVMClassFileTypes.{ConstantValueAttribute, _}

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

  def extendIntAsTwosComplement(in: Int): Long = {
    // JVM does this for us - entirely possible that all code here is redundant!
    in.toLong
    //    val topBitSet = (in & 0x80000000) != 0
    //    if (topBitSet) {
    //      val compare = 0xffffffffffffffffL
    //      val out = compare | in
    //      out
    //    }
    //    else in
  }

  // Java and bytes:
  // Integer.parseInt("11110111", 2) == 247, not -9, e.g. it doesn't do the twos complement.  Use extendByteAsTwosComplement
  // Raw file is 0xffff -> in.read() is 255

  def readByteUnsigned(in: DataInputStream): Int = {
    // Returns 0-255, or -1 if end of stream reached
    val rawByte = in.read()
    rawByte
  }

  def readByteTwosComplement(in: DataInputStream): Int = {
    // Returns 0-255, or -1 if end of stream reached
    val rawByte = in.read()
    extendByteAsTwosComplement(rawByte)
  }

  def readShortTwosComplement(in: DataInputStream): Int = {
    extendShortAsTwosComplement(readByteTwosComplement(in) << 8) | readByteTwosComplement(in)
  }

  def readIntTwosComplement(in: DataInputStream): Int = {
    (readByteUnsigned(in) << 24) | (readByteUnsigned(in) << 16) | (readByteUnsigned(in) << 8) | readByteUnsigned(in)
  }

  def readFloatTwosComplement(in: DataInputStream): Float = {
    (readByteUnsigned(in) << 24) | (readByteUnsigned(in) << 16) | (readByteUnsigned(in) << 8) | readByteUnsigned(in)
  }

  def readDoubleTwosComplement(in: DataInputStream): Double = {
    (readByteUnsigned(in) << 56) | (readByteUnsigned(in) << 48) | (readByteUnsigned(in) << 40) | (readByteUnsigned(in) << 32) | (readByteUnsigned(in) << 24) | (readByteUnsigned(in) << 16) | (readByteUnsigned(in) << 8) | readByteUnsigned(in)
  }

  def readLongTwosComplements(in: DataInputStream): Long = {
    (readByteUnsigned(in) << 56) | (readByteUnsigned(in) << 48) | (readByteUnsigned(in) << 40) | (readByteUnsigned(in) << 32) | (readByteUnsigned(in) << 24) | (readByteUnsigned(in) << 16) | (readByteUnsigned(in) << 8) | readByteUnsigned(in)
  }


}

// Reads in a .class file into a JVMClassFileBuilder
object JVMClassFileReader {
  private def badParse(in: DataInputStream, err: String): Unit = {
    val msg = "Invalid .class file: " + err
    JVM.err(msg)
  }

  private def readUnsignedShort(in: DataInputStream): Int = in.readUnsignedShort() // JVMClassFileReaderUtils.readShortTwosComplement(in)

  private def readUnsignedByte(in: DataInputStream): Int = in.readUnsignedByte() // JVMClassFileReaderUtils.readByteTwosComplement(in)

  private def readInt(in: DataInputStream): Int = in.readInt() // JVMClassFileReaderUtils.readIntTwosComplement(in)

  private def readFloat(in: DataInputStream): Float = in.readFloat() // JVMClassFileReaderUtils.readFloatTwosComplement(in)

  private def readDouble(in: DataInputStream): Double = in.readDouble() // JVMClassFileReaderUtils.readDoubleTwosComplement(in)

  private def readLong(in: DataInputStream): Long = in.readLong() // JVMClassFileReaderUtils.readLongTwosComplements(in)

  private def good(in: DataInputStream, msg: String): Unit = {
    println(msg)
  }

  private def good(in: DataInputStream, indent: Int, msg: String): Unit = {
    println((" " * indent * 2) + msg)
  }

  private def addConstant(params: ReadParams, cf: JVMClassFileBuilderForReading, constant: Constant, idx: Int) = {
    if (params.verbose) println(s"Constant $idx: $constant")
    cf.addConstant(constant)
  }

  private def goodConstant(in: DataInputStream, indent: Int, cf: JVMClassFileBuilderForReading, name: String, value: Int): Unit = {
    try {
      val constant = cf.getConstant(value)
      good(in, s"${" " * indent * 2}$name = $value (${constant})")
    }
    catch {
      case e: Throwable =>
        badParse(in, s"$name = $value (constant idx $value outside pool!!)")
    }
  }


  private def printCode(in: DataInputStream, cf: JVMClassFileBuilderForReading, indent: Int, code: Array[Byte]): Unit = {
    var idx = 0
    //    val stream = new DataInputStream(code)

    while (idx < code.length) {

      // Can't find where this is documented, but opcode is unsigned rather than usual signed bytes
      val opcodehex = in.readUnsignedByte() // JVMClassFileReaderUtils.readByteUnsigned(stream)

      val opcode = JVMOpCodes.getOpcode(opcodehex)
      println((" " * indent * 2) + opcode.gen(in))
      idx += opcode.lengthInBytes
    }
  }

  //  private def readCode(in: DataInputStream, cf: JVMClassFileBuilderForReading, indent: Int, code: Array[Byte]): Vector[JVMOpCodeWithArgs] = {
  private def readCode(in: DataInputStream, cf: JVMClassFileBuilderForReading, indent: Int, codeLen: Int): Vector[JVMOpCodeWithArgs] = {
    var idx = 0
    //    val stream = new DataInputStream(code)
    val out = ArrayBuffer.empty[JVMOpCodeWithArgs]

    while (idx < codeLen) {

      // Can't find where this is documented, but opcode is unsigned rather than usual signed bytes
      val opcodehex = in.readUnsignedByte() // JVMClassFileReaderUtils.readByteUnsigned(stream)

      val opcode = JVMOpCodes.getOpcode(opcodehex)

      if (opcode.unsupported) {
        badParse(in, s"Cannot handle opcode ${opcode} yet")
      }

      //      println((" " * indent * 2) + opcode.gen(stream))

      val args = ArrayBuffer.empty[Int]
      for (argIdx <- opcode.args.indices) {
        val value = opcode.args(argIdx).lengthBytes match {
          case 1 => in.readByte() // JVMClassFileReaderUtils.readByteTwosComplement(stream)
          case 2 => in.readShort() // JVMClassFileReaderUtils.readShortTwosComplement(stream)
          case 4 => in.readInt() //  JVMClassFileReaderUtils.readIntTwosComplement(stream)
        }
        args += value
      }

      out += JVMByteCode.JVMOpCodeWithArgs(opcode, args.map(v => JVMVarInt(v)).toArray)

      idx += opcode.lengthInBytes


    }

    out.toVector
  }


  private def readMethodAttribute(params: ReadParams, in: DataInputStream, cf: JVMClassFileBuilderForReading, indent: Int): Attribute = {
    val attribute_name_index = readUnsignedShort(in)
    if (params.verbose) goodConstant(in, indent, cf, s"attribute_name_index (${cf.getConstant(attribute_name_index)})", attribute_name_index)

    val attribute_length = readInt(in)
    if (params.verbose) good(in, indent, s"attribute_length = $attribute_length")

    val attributeName = cf.getString(attribute_name_index)
    attributeName match {
      case "Code"       => readCodeAttribute(params, in, cf, indent, attribute_name_index)
      case "Exceptions" => readExceptionsAttribute(params, in, cf, indent, attribute_name_index)
    }


  }

  private def readExceptionsAttribute(params: ReadParams, in: DataInputStream, cf: JVMClassFileBuilderForReading, indent: Int, attribute_name_index: Int): ExceptionsAttribute = {
    val number_of_exceptions = readUnsignedShort(in)
    if (params.verbose) good(in, indent, s"number_of_exceptions = $number_of_exceptions")

    val indices = Range(0, number_of_exceptions).map(idx => {
      readUnsignedShort(in)
    })

    ExceptionsAttribute(attribute_name_index, indices)
  }

  private def readCodeAttribute(params: ReadParams, in: DataInputStream, cf: JVMClassFileBuilderForReading, indent: Int, attribute_name_index: Int): CodeAttribute = {
    val max_stack = readUnsignedShort(in)
    if (params.verbose) good(in, indent, s"max_stack = $max_stack")

    val max_locals = readUnsignedShort(in)
    if (params.verbose) good(in, indent, s"max_locals = $max_locals")

    var code_length = readInt(in)
    if (params.verbose) good(in, indent, s"code_length = $code_length")

    val temp = new ByteArrayOutputStream()

    while (code_length > 0) {
      val b = in.readUnsignedByte().toByte
      temp.write(b)
      code_length -= 1
    }
    val code = temp.toByteArray
    val opcodes = readCode(new DataInputStream(new ByteArrayInputStream(code)), cf, indent + 1, code_length)

    val exception_table_length = readUnsignedShort(in)
    if (params.verbose) good(in, indent, s"exception_table_length = $exception_table_length")
    for (idx <- Range(0, exception_table_length)) {
      readUnsignedShort(in)
      readUnsignedShort(in)
      readUnsignedShort(in)
      readUnsignedShort(in)
    }

    var attributes_count = readUnsignedShort(in)
    if (params.verbose) good(in, indent, s"attributes_count = $attributes_count")

    for (idx <- Range(0, attributes_count)) {
      // Can be LineNumberTable or LocalVariableTable, both optional and for debugging
      val attribute_name_index = readUnsignedShort(in)
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

  private def readFieldAttribute(params: ReadParams, in: DataInputStream, cf: JVMClassFileBuilderForReading, indent: Int): Attribute = {
    val attribute_name_index = readUnsignedShort(in)
    val attributeName = cf.getString(attribute_name_index)
    if (params.verbose) goodConstant(in, indent, cf, s"attribute_name_index (${attributeName})", attribute_name_index)

    val attribute_length = readInt(in)
    if (params.verbose) good(in, indent, s"attribute_length = $attribute_length")

    attributeName match {
      case "Synthetic"     => SyntheticAttribute(attribute_name_index)
      case "Deprecated"    => DeprecatedAttribute(attribute_name_index)
      case "ConstantValue" =>
        val constantvalue_index = readUnsignedShort(in)
        if (params.verbose) good(in, indent, s"constantvalue_index = $constantvalue_index ${cf.getConstant(constantvalue_index)}")

        ConstantValueAttribute(attribute_name_index,
          constantvalue_index
        )
      case _               =>
        badParse(in, s"unsupported attribute ${attributeName}")
        null
    }
  }

  private def readField(params: ReadParams, in: DataInputStream, cf: JVMClassFileBuilderForReading, indentIn: Int): FieldInfo = {
    if (params.verbose) good(in, indentIn, "Field:")
    val indent = indentIn + 1

    val access_flags = readUnsignedShort(in)
    if (params.verbose) {
      val af = ArrayBuffer.empty[String]
      if ((access_flags & 0x1) != 0) af += "ACC_PUBLIC"
      if ((access_flags & 0x2) != 0) af += "ACC_PRIVATE"
      if ((access_flags & 0x4) != 0) af += "ACC_PROTECTED"
      if ((access_flags & 0x8) != 0) af += "ACC_STATIC"
      if ((access_flags & 0x10) != 0) af += "ACC_FINAL"
      if ((access_flags & 0x20) != 0) af += "ACC_SYNCHRONIZED"
      if ((access_flags & 0x40) != 0) af += "ACC_VOLATILE"
      if ((access_flags & 0x80) != 0) af += "ACC_TRANSIENT"
      good(in, indent, s"access_flags = ${af.mkString(", ")} ($access_flags)")
    }

    val name_index = readUnsignedShort(in)
    if (params.verbose) good(in, indent, s"name_index = $name_index (${cf.getConstant(name_index)})")

    val descriptor_index = readUnsignedShort(in)
    if (params.verbose) good(in, indent, s"descriptor_index = $descriptor_index (${cf.getConstant(descriptor_index)})")

    val attributes_count = readUnsignedShort(in)
    if (params.verbose) good(in, indent, s"attributes_count = $attributes_count")

    // Constant, Synthetic and Deprecated allowed
    val attributes = ArrayBuffer.empty[Attribute]
    for (idx <- Range(0, attributes_count)) {
      if (params.verbose) good(in, indent, s"Field Attribute $idx")

      attributes += readFieldAttribute(params: ReadParams, in, cf, indent + 1)
    }

    FieldInfo(
      access_flags,
      name_index,
      descriptor_index,
      attributes
    )
  }

  private def readMethod(params: ReadParams, in: DataInputStream, cf: JVMClassFileBuilderForReading, indentIn: Int): MethodInfo = {
    if (params.verbose) good(in, indentIn, "Method:")
    val indent = indentIn + 1

    val access_flags = readUnsignedShort(in)
    if (params.verbose) {
      val af = ArrayBuffer.empty[String]
      if ((access_flags & 0x1) != 0) af += "ACC_PUBLIC"
      if ((access_flags & 0x2) != 0) af += "ACC_PRIVATE"
      if ((access_flags & 0x4) != 0) af += "ACC_PROTECTED"
      if ((access_flags & 0x8) != 0) af += "ACC_STATIC"
      if ((access_flags & 0x10) != 0) af += "ACC_FINAL"
      if ((access_flags & 0x20) != 0) af += "ACC_SYNCHRONIZED"
      if ((access_flags & 0x100) != 0) af += "ACC_NATIVE"
      if ((access_flags & 0x400) != 0) af += "ACC_ABSTRACT"
      if ((access_flags & 0x800) != 0) af += "ACC_STRICT"
      good(in, indent, s"access_flags = ${af.mkString(", ")} ($access_flags)")
    }

    val name_index = readUnsignedShort(in)
    if (params.verbose) good(in, indent, s"name_index = $name_index (${cf.getConstant(name_index)})")

    val descriptor_index = readUnsignedShort(in)
    if (params.verbose) good(in, indent, s"descriptor_index = $descriptor_index (${cf.getConstant(descriptor_index)})")

    val attributes_count = readUnsignedShort(in)
    if (params.verbose) good(in, indent, s"attributes_count = $attributes_count")

    val attributes = ArrayBuffer.empty[Attribute]
    for (idx <- Range(0, attributes_count)) {
      if (params.verbose) good(in, indent, s"Method Attribute $idx")
      attributes += readMethodAttribute(params: ReadParams, in, cf, indent + 1)
    }

    MethodInfo(
      access_flags,
      name_index,
      descriptor_index,
      attributes
    )
  }

  case class ReadParams(verbose: Boolean = false)

  def read(packageName: Option[String], clsName: String, file: File, params: ReadParams): Option[JVMClassFile] = {
    //    val bytes = Files.readAllBytes(file.toPath)
    val stream = new FileInputStream(file)
    val in = new DataInputStream(stream)
    val out = read(packageName, clsName, file.getCanonicalPath, in, params)
    in.close()
    out
  }

  def read(packageName: Option[String], clsName: String, fullPath: String, in: DataInputStream, params: ReadParams): Option[JVMClassFile] = {
    //    try {
    if (in.readUnsignedByte() != 0xca) badParse(in, "Initial bytes are not 'cafebabe'")
    if (in.readUnsignedByte() != 0xfe) badParse(in, "Initial bytes are not 'cafebabe'")
    if (in.readUnsignedByte() != 0xba) badParse(in, "Initial bytes are not 'cafebabe'")
    if (in.readUnsignedByte() != 0xbe) badParse(in, "Initial bytes are not 'cafebabe'")

    val charset = StandardCharsets.UTF_8

    val minor_version = readUnsignedShort(in)
    val major_version = readUnsignedShort(in)

    if (major_version > 50) {
      val realVersion = major_version - 44
      JVM.err(s"can handle up to Java 6 only currently, and this is Java ${realVersion} - please recompile code as Java 6")
    }

    if (params.verbose) good(in, s"Version: major=$major_version minor=$minor_version")

    val constantPoolCount = readUnsignedShort(in)

    if (params.verbose) good(in, s"Constant pool count = $constantPoolCount")

    val cf = new JVMClassFileBuilderForReading(fullPath, major_version, minor_version, packageName, clsName)

    var idx = 1
    while (idx < constantPoolCount) {
      val tag = readUnsignedByte(in)
      tag match {
        case 1  =>
          var len = readUnsignedShort(in)
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
          // All 8-byte constants take up two entries in the constant_pool table of the class file
          idx += 1
          addConstant(params, cf, ConstantDummy(), idx)
        case 6  =>
          val value = readDouble(in)
          addConstant(params, cf, ConstantDouble(value), idx)
          // All 8-byte constants take up two entries in the constant_pool table of the class file
          idx = idx + 1
          addConstant(params, cf, ConstantDummy(), idx)
        case 7  =>
          val index = readUnsignedShort(in)
          addConstant(params, cf, ConstantClass(index), idx)
        case 8  =>
          val index1 = readUnsignedShort(in)
          addConstant(params, cf, ConstantString(index1), idx)
        case 9  =>
          val index1 = readUnsignedShort(in)
          val index2 = readUnsignedShort(in)
          addConstant(params, cf, ConstantFieldref(index1, index2), idx)
        case 10 =>
          val index1 = readUnsignedShort(in)
          val index2 = readUnsignedShort(in)
          addConstant(params, cf, ConstantMethodref(index1, index2), idx)
        case 11 =>
          val index1 = readUnsignedShort(in)
          val index2 = readUnsignedShort(in)
          addConstant(params, cf, ConstantInterfaceMethodref(index1, index2), idx)
        case 12 =>
          val index1 = readUnsignedShort(in)
          val index2 = readUnsignedShort(in)
          addConstant(params, cf, ConstantNameAndType(index1, index2), idx)
      }

      idx += 1
    }

    val access_flags = readUnsignedShort(in)
    if (params.verbose) good(in, s"access_flags = $access_flags")

    val this_class = readUnsignedShort(in)
    if (params.verbose) good(in, s"this_class = $this_class")

    val super_class = readUnsignedShort(in)
    if (params.verbose) good(in, s"super_class = $super_class")

    val interfaces_count = readUnsignedShort(in)
    if (params.verbose) good(in, s"interfaces_count = $interfaces_count")

    for (idx <- Range(0, interfaces_count)) {
      val index = readUnsignedShort(in)
      val interface = cf.getConstant(index).asInstanceOf[ConstantClass]
      cf.addInterface(interface)
    }

    val fields_count = readUnsignedShort(in)
    if (params.verbose) good(in, s"fields_count = $fields_count")

    for (idx <- Range(0, fields_count)) {
      val field = readField(params, in, cf, 0)
      cf.addField(field)
    }

    val methods_count = readUnsignedShort(in)
    if (params.verbose) good(in, s"methods_count = $methods_count")
    for (idx <- Range(0, methods_count)) {
      val method = readMethod(params, in, cf, 0)
      cf.addMethod(method)
    }

    val attributes_count = readUnsignedShort(in)
    if (params.verbose) good(in, s"attributes_count = $attributes_count")

    for (idx <- Range(0, attributes_count)) {
      val attribute_name_index = readUnsignedShort(in)
      var attribute_length = readInt(in)
      if (params.verbose) good(in, s"Skipping attribute $idx len ${attribute_length} attribute_name_index = $attribute_name_index (${cf.getConstant(attribute_name_index)})")
      while (attribute_length > 0) {
        in.read()
        attribute_length -= 1
      }
    }

    assert(in.read() == -1)

    Some(cf.makeImmutable())
    //    }
    //    catch {
    //      case e: Throwable =>
    //        JVM.err(s"Failed to read classfile: ${e}")
    //        None
    //    }
  }

  //  def mainReader(args: Array[String]): Unit = {
  //    if (args.length != 1) {
  //      println("usage: program <.class file>")
  //    }
  //    else {
  //      val file = new File(args(0))
  //      val fileContent = new Array[Byte](file.length.asInstanceOf[Int])
  //      new FileInputStream(file).read(fileContent)
  //      val lines = new DataInputStream(fileContent)
  //      read(file.getCanonicalPath, lines, ReadParams(verbose = true))
  //    }
  //  }

}
