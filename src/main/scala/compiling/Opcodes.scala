package compiling

import java.io.ByteArrayInputStream

//sealed trait JVMOpCode {
//  val hexcode: Int
//  val name: String
//  val desc: String
//  val lengthInBytes: Byte
//  val args: Vector[JVMOpCodeArg]
//
//  // Assumes the hexcode has just been read from 'in', and reads and prints subsequent args
//  def gen(in: ByteArrayInputStream): String
//}

case class JVMOpCodeArg(lengthBytes: Int, name: String) {
  def gen(in: ByteArrayInputStream): String = {
    lengthBytes match {
      case 1 =>
        val value = JVMClassFileReaderUtils.readByte(in)
        value.toString
      case 2 =>
        val value = JVMClassFileReaderUtils.readShort(in)
        value.toString
      case 4 =>
        val value = JVMClassFileReaderUtils.readInt(in)
        value.toString
    }
  }
}

case class JVMOpCode(hexcode: Int, name: String, desc: String, lengthInBytes: Int, args: Array[JVMOpCodeArg], unsupported: Boolean) {
  def gen(in: ByteArrayInputStream): String = {
    name + " " + args.map(_.gen(in)).mkString(" ") + (if (unsupported) " DANGER: UNSUPPORTED OPCODE, REMAINING DECODE WILL BE INCORRECT" else "")
  }
}

//case class JVMOpCode1Arg1Byte(hexcode: Int, name: String, desc: String, lengthInBytes: Byte) extends JVMOpCode {
//  override def gen(in: ByteArrayInputStream): String = {
//    val value = in.read()
//    name + " " + value
//  }
//}
//
//case class JVMOpCode2(hexcode: Int, name: String, desc: String, lengthInBytes: Byte) extends JVMOpCode {
//  override def gen(in: ByteArrayInputStream): String = {
//    val value1 = in.read()
//    val value2 = in.read()
//    name + " " + value1 + " " + value2
//  }
//}
//
//case class JVMOpCode4(hexcode: Int, name: String, desc: String, lengthInBytes: Byte) extends JVMOpCode {
//  override def gen(in: ByteArrayInputStream): String = {
//    val value1 = in.read()
//    name + " " + value1 + " " + value2
//  }
//}


object JVMOpCode {
  def create(name: String, hexcode: Int, variablesDesc: String, stackDesc: String, desc: String): JVMOpCode = {
//    val numArgs = if (variablesDesc.nonEmpty) variablesDesc.head.toByte else 0
//    val lengthInBytes: Byte = (1 + numArgs).toByte

    var unsupported = false

    val args: Array[JVMOpCodeArg] = variablesDesc.trim match {
      case "" => Array()
      case "1: index" => Array(JVMOpCodeArg(1,"index"))
      case "2: indexbyte1, indexbyte2" => Array(JVMOpCodeArg(2,"index"))
      case "1: byte" => Array(JVMOpCodeArg(1,"byte"))
      case "2: branchbyte1, branchbyte2" => Array(JVMOpCodeArg(2,"branch"))
      case "4: branchbyte1, branchbyte2, branchbyte3, branchbyte4" => Array(JVMOpCodeArg(4,"branch"))
      case "2: index, const" => Array(JVMOpCodeArg(1,"index"),JVMOpCodeArg(1,"const"))
      case "4: indexbyte1, indexbyte2, 0, 0" => Array(JVMOpCodeArg(2,"index"),JVMOpCodeArg(2,"ignored"))
      case "4: indexbyte1, indexbyte2, count, 0" => Array(JVMOpCodeArg(2,"index"),JVMOpCodeArg(1,"count"),JVMOpCodeArg(1,"ignored"))
      case _ =>
        unsupported = true
        Array()
    }

    val lenInBytes = 1 + args.foldLeft(0)((a,b) => a + b.lengthBytes)

    JVMOpCode(hexcode, name, desc, lenInBytes, args, unsupported)
  }
}

object JVMOpCodes {
  // Created from https://en.wikipedia.org/wiki/Java_bytecode_instruction_listings and regex:
  // \s*(.*?)	(..)\t.... ....\t(.*?)\t(.*?)\t(.*)
  // JVMOpCode.create("$1", 0x$2, "$3", "$4", "$5")\n
  val opcodes: Seq[JVMOpCode] = Vector(
    JVMOpCode.create("aaload", 0x32, "", "arrayref, index → value", "load onto the stack a reference from an array"),
    JVMOpCode.create("aastore", 0x53, "", "arrayref, index, value →", "store into a reference in an array"),
    JVMOpCode.create("aconst_null", 0x01, "", "→ null", "push a null reference onto the stack"),
    JVMOpCode.create("aload", 0x19, "1: index", "→ objectref", "load a reference onto the stack from a local variable #index"),
    JVMOpCode.create("aload_0", 0x2a, "", "→ objectref", "load a reference onto the stack from local variable 0"),
    JVMOpCode.create("aload_1", 0x2b, "", "→ objectref", "load a reference onto the stack from local variable 1"),
    JVMOpCode.create("aload_2", 0x2c, "", "→ objectref", "load a reference onto the stack from local variable 2"),
    JVMOpCode.create("aload_3", 0x2d, "", "→ objectref", "load a reference onto the stack from local variable 3"),
    JVMOpCode.create("anewarray", 0xbd, "2: indexbyte1, indexbyte2", "count → arrayref", "create a new array of references of length count and component type identified by the class reference index (indexbyte1 << 8 + indexbyte2) in the constant pool"),
    JVMOpCode.create("areturn", 0xb0, "", "objectref → [empty]", "return a reference from a method"),
    JVMOpCode.create("arraylength", 0xbe, "", "arrayref → length", "get the length of an array"),
    JVMOpCode.create("astore", 0x3a, "1: index", "objectref →", "store a reference into a local variable #index"),
    JVMOpCode.create("astore_0", 0x4b, "", "objectref →", "store a reference into local variable 0"),
    JVMOpCode.create("astore_1", 0x4c, "", "objectref →", "store a reference into local variable 1"),
    JVMOpCode.create("astore_2", 0x4d, "", "objectref →", "store a reference into local variable 2"),
    JVMOpCode.create("astore_3", 0x4e, "", "objectref →", "store a reference into local variable 3"),
    JVMOpCode.create("athrow", 0xbf, "", "objectref → [empty], objectref", "throws an error or exception (notice that the rest of the stack is cleared, leaving only a reference to the Throwable)"),
    JVMOpCode.create("baload", 0x33, "", "arrayref, index → value", "load a byte or Boolean value from an array"),
    JVMOpCode.create("bastore", 0x54, "", "arrayref, index, value →", "store a byte or Boolean value into an array"),
    JVMOpCode.create("bipush", 0x10, "1: byte", "→ value", "push a byte onto the stack as an integer value"),
    JVMOpCode.create("breakpoint", 0xca, "", "", "reserved for breakpoints in Java debuggers; should not appear in any class file"),
    JVMOpCode.create("caload", 0x34, "", "arrayref, index → value", "load a char from an array"),
    JVMOpCode.create("castore", 0x55, "", "arrayref, index, value →", "store a char into an array"),
    JVMOpCode.create("checkcast", 0xc0, "2: indexbyte1, indexbyte2", "objectref → objectref", "checks whether an objectref is of a certain type, the class reference of which is in the constant pool at index (indexbyte1 << 8 + indexbyte2)"),
    JVMOpCode.create("d2f", 0x90, "", "value → result", "convert a double to a float"),
    JVMOpCode.create("d2i", 0x8e, "", "value → result", "convert a double to an int"),
    JVMOpCode.create("d2l", 0x8f, "", "value → result", "convert a double to a long"),
    JVMOpCode.create("dadd", 0x63, "", "value1, value2 → result", "add two doubles"),
    JVMOpCode.create("daload", 0x31, "", "arrayref, index → value", "load a double from an array"),
    JVMOpCode.create("dastore", 0x52, "", "arrayref, index, value →", "store a double into an array"),
    JVMOpCode.create("dcmpg", 0x98, "", "value1, value2 → result", "compare two doubles"),
    JVMOpCode.create("dcmpl", 0x97, "", "value1, value2 → result", "compare two doubles"),
    JVMOpCode.create("dconst_0", 0x0e, "", "→ 0.0", "push the constant 0.0 (a double) onto the stack"),
    JVMOpCode.create("dconst_1", 0x0f, "", "→ 1.0", "push the constant 1.0 (a double) onto the stack"),
    JVMOpCode.create("ddiv", 0x6f, "", "value1, value2 → result", "divide two doubles"),
    JVMOpCode.create("dload", 0x18, "1: index", "→ value", "load a double value from a local variable #index"),
    JVMOpCode.create("dload_0", 0x26, "", "→ value", "load a double from local variable 0"),
    JVMOpCode.create("dload_1", 0x27, "", "→ value", "load a double from local variable 1"),
    JVMOpCode.create("dload_2", 0x28, "", "→ value", "load a double from local variable 2"),
    JVMOpCode.create("dload_3", 0x29, "", "→ value", "load a double from local variable 3"),
    JVMOpCode.create("dmul", 0x6b, "", "value1, value2 → result", "multiply two doubles"),
    JVMOpCode.create("dneg", 0x77, "", "value → result", "negate a double"),
    JVMOpCode.create("drem", 0x73, "", "value1, value2 → result", "get the remainder from a division between two doubles"),
    JVMOpCode.create("dreturn", 0xaf, "", "value → [empty]", "return a double from a method"),
    JVMOpCode.create("dstore", 0x39, "1: index", "value →", "store a double value into a local variable #index"),
    JVMOpCode.create("dstore_0", 0x47, "", "value →", "store a double into local variable 0"),
    JVMOpCode.create("dstore_1", 0x48, "", "value →", "store a double into local variable 1"),
    JVMOpCode.create("dstore_2", 0x49, "", "value →", "store a double into local variable 2"),
    JVMOpCode.create("dstore_3", 0x4a, "", "value →", "store a double into local variable 3"),
    JVMOpCode.create("dsub", 0x67, "", "value1, value2 → result", "subtract a double from another"),
    JVMOpCode.create("dup", 0x59, "", "value → value, value", "duplicate the value on top of the stack"),
    JVMOpCode.create("dup_x1", 0x5a, "", "value2, value1 → value1, value2, value1", "insert a copy of the top value into the stack two values from the top. value1 and value2 must not be of the type double or long."),
    JVMOpCode.create("dup_x2", 0x5b, "", "value3, value2, value1 → value1, value3, value2, value1", "insert a copy of the top value into the stack two (if value2 is double or long it takes up the entry of value3, too) or three values (if value2 is neither double nor long) from the top"),
    JVMOpCode.create("dup2", 0x5c, "", "{value2, value1} → {value2, value1}, {value2, value1}", "duplicate top two stack words (two values, if value1 is not double nor long; a single value, if value1 is double or long)"),
    JVMOpCode.create("dup2_x1", 0x5d, "", "value3, {value2, value1} → {value2, value1}, value3, {value2, value1}", "duplicate two words and insert beneath third word (see explanation above)"),
    JVMOpCode.create("dup2_x2", 0x5e, "", "{value4, value3}, {value2, value1} → {value2, value1}, {value4, value3}, {value2, value1}", "duplicate two words and insert beneath fourth word"),
    JVMOpCode.create("f2d", 0x8d, "", "value → result", "convert a float to a double"),
    JVMOpCode.create("f2i", 0x8b, "", "value → result", "convert a float to an int"),
    JVMOpCode.create("f2l", 0x8c, "", "value → result", "convert a float to a long"),
    JVMOpCode.create("fadd", 0x62, "", "value1, value2 → result", "add two floats"),
    JVMOpCode.create("faload", 0x30, "", "arrayref, index → value", "load a float from an array"),
    JVMOpCode.create("fastore", 0x51, "", "arrayref, index, value →", "store a float in an array"),
    JVMOpCode.create("fcmpg", 0x96, "", "value1, value2 → result", "compare two floats"),
    JVMOpCode.create("fcmpl", 0x95, "", "value1, value2 → result", "compare two floats"),
    JVMOpCode.create("fconst_0", 0x0b, "", "→ 0.0f", "push 0.0f on the stack"),
    JVMOpCode.create("fconst_1", 0x0c, "", "→ 1.0f", "push 1.0f on the stack"),
    JVMOpCode.create("fconst_2", 0x0d, "", "→ 2.0f", "push 2.0f on the stack"),
    JVMOpCode.create("fdiv", 0x6e, "", "value1, value2 → result", "divide two floats"),
    JVMOpCode.create("fload", 0x17, "1: index", "→ value", "load a float value from a local variable #index"),
    JVMOpCode.create("fload_0", 0x22, "", "→ value", "load a float value from local variable 0"),
    JVMOpCode.create("fload_1", 0x23, "", "→ value", "load a float value from local variable 1"),
    JVMOpCode.create("fload_2", 0x24, "", "→ value", "load a float value from local variable 2"),
    JVMOpCode.create("fload_3", 0x25, "", "→ value", "load a float value from local variable 3"),
    JVMOpCode.create("fmul", 0x6a, "", "value1, value2 → result", "multiply two floats"),
    JVMOpCode.create("fneg", 0x76, "", "value → result", "negate a float"),
    JVMOpCode.create("frem", 0x72, "", "value1, value2 → result", "get the remainder from a division between two floats"),
    JVMOpCode.create("freturn", 0xae, "", "value → [empty]", "return a float"),
    JVMOpCode.create("fstore", 0x38, "1: index", "value →", "store a float value into a local variable #index"),
    JVMOpCode.create("fstore_0", 0x43, "", "value →", "store a float value into local variable 0"),
    JVMOpCode.create("fstore_1", 0x44, "", "value →", "store a float value into local variable 1"),
    JVMOpCode.create("fstore_2", 0x45, "", "value →", "store a float value into local variable 2"),
    JVMOpCode.create("fstore_3", 0x46, "", "value →", "store a float value into local variable 3"),
    JVMOpCode.create("fsub", 0x66, "", "value1, value2 → result", "subtract two floats"),
    JVMOpCode.create("getfield", 0xb4, "2: indexbyte1, indexbyte2", "objectref → value", "get a field value of an object objectref, where the field is identified by field reference in the constant pool index (indexbyte1 << 8 + indexbyte2)"),
    JVMOpCode.create("getstatic", 0xb2, "2: indexbyte1, indexbyte2", "→ value", "get a static field value of a class, where the field is identified by field reference in the constant pool index (indexbyte1 << 8 + indexbyte2)"),
    JVMOpCode.create("goto", 0xa7, "2: branchbyte1, branchbyte2", "[no change]", "goes to another instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)"),
    JVMOpCode.create("goto_w", 0xc8, "4: branchbyte1, branchbyte2, branchbyte3, branchbyte4", "[no change]", "goes to another instruction at branchoffset (signed int constructed from unsigned bytes branchbyte1 << 24 + branchbyte2 << 16 + branchbyte3 << 8 + branchbyte4)"),
    JVMOpCode.create("i2b", 0x91, "", "value → result", "convert an int into a byte"),
    JVMOpCode.create("i2c", 0x92, "", "value → result", "convert an int into a character"),
    JVMOpCode.create("i2d", 0x87, "", "value → result", "convert an int into a double"),
    JVMOpCode.create("i2f", 0x86, "", "value → result", "convert an int into a float"),
    JVMOpCode.create("i2l", 0x85, "", "value → result", "convert an int into a long"),
    JVMOpCode.create("i2s", 0x93, "", "value → result", "convert an int into a short"),
    JVMOpCode.create("iadd", 0x60, "", "value1, value2 → result", "add two ints"),
    JVMOpCode.create("iaload", 0x2e, "", "arrayref, index → value", "load an int from an array"),
    JVMOpCode.create("iand", 0x7e, "", "value1, value2 → result", "perform a bitwise AND on two integers"),
    JVMOpCode.create("iastore", 0x4f, "", "arrayref, index, value →", "store an int into an array"),
    JVMOpCode.create("iconst_m1", 0x02, "", "→ -1", "load the int value −1 onto the stack"),
    JVMOpCode.create("iconst_0", 0x03, "", "→ 0", "load the int value 0 onto the stack"),
    JVMOpCode.create("iconst_1", 0x04, "", "→ 1", "load the int value 1 onto the stack"),
    JVMOpCode.create("iconst_2", 0x05, "", "→ 2", "load the int value 2 onto the stack"),
    JVMOpCode.create("iconst_3", 0x06, "", "→ 3", "load the int value 3 onto the stack"),
    JVMOpCode.create("iconst_4", 0x07, "", "→ 4", "load the int value 4 onto the stack"),
    JVMOpCode.create("iconst_5", 0x08, "", "→ 5", "load the int value 5 onto the stack"),
    JVMOpCode.create("idiv", 0x6c, "", "value1, value2 → result", "divide two integers"),
    JVMOpCode.create("if_acmpeq", 0xa5, "2: branchbyte1, branchbyte2", "value1, value2 →", "if references are equal, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)"),
    JVMOpCode.create("if_acmpne", 0xa6, "2: branchbyte1, branchbyte2", "value1, value2 →", "if references are not equal, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)"),
    JVMOpCode.create("if_icmpeq", 0x9f, "2: branchbyte1, branchbyte2", "value1, value2 →", "if ints are equal, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)"),
    JVMOpCode.create("if_icmpge", 0xa2, "2: branchbyte1, branchbyte2", "value1, value2 →", "if value1 is greater than or equal to value2, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)"),
    JVMOpCode.create("if_icmpgt", 0xa3, "2: branchbyte1, branchbyte2", "value1, value2 →", "if value1 is greater than value2, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)"),
    JVMOpCode.create("if_icmple", 0xa4, "2: branchbyte1, branchbyte2", "value1, value2 →", "if value1 is less than or equal to value2, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)"),
    JVMOpCode.create("if_icmplt", 0xa1, "2: branchbyte1, branchbyte2", "value1, value2 →", "if value1 is less than value2, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)"),
    JVMOpCode.create("if_icmpne", 0xa0, "2: branchbyte1, branchbyte2", "value1, value2 →", "if ints are not equal, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)"),
    JVMOpCode.create("ifeq", 0x99, "2: branchbyte1, branchbyte2", "value →", "if value is 0, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)"),
    JVMOpCode.create("ifge", 0x9c, "2: branchbyte1, branchbyte2", "value →", "if value is greater than or equal to 0, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)"),
    JVMOpCode.create("ifgt", 0x9d, "2: branchbyte1, branchbyte2", "value →", "if value is greater than 0, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)"),
    JVMOpCode.create("ifle", 0x9e, "2: branchbyte1, branchbyte2", "value →", "if value is less than or equal to 0, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)"),
    JVMOpCode.create("iflt", 0x9b, "2: branchbyte1, branchbyte2", "value →", "if value is less than 0, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)"),
    JVMOpCode.create("ifne", 0x9a, "2: branchbyte1, branchbyte2", "value →", "if value is not 0, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)"),
    JVMOpCode.create("ifnonnull", 0xc7, "2: branchbyte1, branchbyte2", "value →", "if value is not null, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)"),
    JVMOpCode.create("ifnull", 0xc6, "2: branchbyte1, branchbyte2", "value →", "if value is null, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)"),
    JVMOpCode.create("iinc", 0x84, "2: index, const", "[No change]", "increment local variable #index by signed byte const"),
    JVMOpCode.create("iload", 0x15, "1: index", "→ value", "load an int value from a local variable #index"),
    JVMOpCode.create("iload_0", 0x1a, "", "→ value", "load an int value from local variable 0"),
    JVMOpCode.create("iload_1", 0x1b, "", "→ value", "load an int value from local variable 1"),
    JVMOpCode.create("iload_2", 0x1c, "", "→ value", "load an int value from local variable 2"),
    JVMOpCode.create("iload_3", 0x1d, "", "→ value", "load an int value from local variable 3"),
    JVMOpCode.create("impdep1", 0xfe, "", "", "reserved for implementation-dependent operations within debuggers; should not appear in any class file"),
    JVMOpCode.create("impdep2", 0xff, "", "", "reserved for implementation-dependent operations within debuggers; should not appear in any class file"),
    JVMOpCode.create("imul", 0x68, "", "value1, value2 → result", "multiply two integers"),
    JVMOpCode.create("ineg", 0x74, "", "value → result", "negate int"),
    JVMOpCode.create("instanceof", 0xc1, "2: indexbyte1, indexbyte2", "objectref → result", "determines if an object objectref is of a given type, identified by class reference index in constant pool (indexbyte1 << 8 + indexbyte2)"),
    JVMOpCode.create("invokedynamic", 0xba, "4: indexbyte1, indexbyte2, 0, 0", "[arg1, [arg2 ...]] → result", "invokes a dynamic method and puts the result on the stack (might be void); the method is identified by method reference index in constant pool (indexbyte1 << 8 + indexbyte2)"),
    JVMOpCode.create("invokeinterface", 0xb9, "4: indexbyte1, indexbyte2, count, 0", "objectref, [arg1, arg2, ...] → result", "invokes an interface method on object objectref and puts the result on the stack (might be void); the interface method is identified by method reference index in constant pool (indexbyte1 << 8 + indexbyte2)"),
    JVMOpCode.create("invokespecial", 0xb7, "2: indexbyte1, indexbyte2", "objectref, [arg1, arg2, ...] → result", "invoke instance method on object objectref and puts the result on the stack (might be void); the method is identified by method reference index in constant pool (indexbyte1 << 8 + indexbyte2)"),
    JVMOpCode.create("invokestatic", 0xb8, "2: indexbyte1, indexbyte2", "[arg1, arg2, ...] → result", "invoke a static method and puts the result on the stack (might be void); the method is identified by method reference index in constant pool (indexbyte1 << 8 + indexbyte2)"),
    JVMOpCode.create("invokevirtual", 0xb6, "2: indexbyte1, indexbyte2", "objectref, [arg1, arg2, ...] → result", "invoke virtual method on object objectref and puts the result on the stack (might be void); the method is identified by method reference index in constant pool (indexbyte1 << 8 + indexbyte2)"),
    JVMOpCode.create("ior", 0x80, "", "value1, value2 → result", "bitwise int OR"),
    JVMOpCode.create("irem", 0x70, "", "value1, value2 → result", "logical int remainder"),
    JVMOpCode.create("ireturn", 0xac, "", "value → [empty]", "return an integer from a method"),
    JVMOpCode.create("ishl", 0x78, "", "value1, value2 → result", "int shift left"),
    JVMOpCode.create("ishr", 0x7a, "", "value1, value2 → result", "int arithmetic shift right"),
    JVMOpCode.create("istore", 0x36, "1: index", "value →", "store int value into variable #index"),
    JVMOpCode.create("istore_0", 0x3b, "", "value →", "store int value into variable 0"),
    JVMOpCode.create("istore_1", 0x3c, "", "value →", "store int value into variable 1"),
    JVMOpCode.create("istore_2", 0x3d, "", "value →", "store int value into variable 2"),
    JVMOpCode.create("istore_3", 0x3e, "", "value →", "store int value into variable 3"),
    JVMOpCode.create("isub", 0x64, "", "value1, value2 → result", "int subtract"),
    JVMOpCode.create("iushr", 0x7c, "", "value1, value2 → result", "int logical shift right"),
    JVMOpCode.create("ixor", 0x82, "", "value1, value2 → result", "int xor"),
    JVMOpCode.create("jsr", 0xa8, "2: branchbyte1, branchbyte2", "→ address", "jump to subroutine at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2) and place the return address on the stack"),
    JVMOpCode.create("jsr_w", 0xc9, "4: branchbyte1, branchbyte2, branchbyte3, branchbyte4", "→ address", "jump to subroutine at branchoffset (signed int constructed from unsigned bytes branchbyte1 << 24 + branchbyte2 << 16 + branchbyte3 << 8 + branchbyte4) and place the return address on the stack"),
    JVMOpCode.create("l2d", 0x8a, "", "value → result", "convert a long to a double"),
    JVMOpCode.create("l2f", 0x89, "", "value → result", "convert a long to a float"),
    JVMOpCode.create("l2i", 0x88, "", "value → result", "convert a long to a int"),
    JVMOpCode.create("ladd", 0x61, "", "value1, value2 → result", "add two longs"),
    JVMOpCode.create("laload", 0x2f, "", "arrayref, index → value", "load a long from an array"),
    JVMOpCode.create("land", 0x7f, "", "value1, value2 → result", "bitwise AND of two longs"),
    JVMOpCode.create("lastore", 0x50, "", "arrayref, index, value →", "store a long to an array"),
    JVMOpCode.create("lcmp", 0x94, "", "value1, value2 → result", "push 0 if the two longs are the same, 1 if value1 is greater than value2, -1 otherwise"),
    JVMOpCode.create("lconst_0", 0x09, "", "→ 0L", "push 0L (the number zero with type long) onto the stack"),
    JVMOpCode.create("lconst_1", 0x0a, "", "→ 1L", "push 1L (the number one with type long) onto the stack"),
    JVMOpCode.create("ldc", 0x12, "1: index", "→ value", "push a constant #index from a constant pool (String, int, float, Class, java.lang.invoke.MethodType, or java.lang.invoke.MethodHandle) onto the stack"),
    JVMOpCode.create("ldc_w", 0x13, "2: indexbyte1, indexbyte2", "→ value", "push a constant #index from a constant pool (String, int, float, Class, java.lang.invoke.MethodType, or java.lang.invoke.MethodHandle) onto the stack (wide index is constructed as indexbyte1 << 8 + indexbyte2)"),
    JVMOpCode.create("ldc2_w", 0x14, "2: indexbyte1, indexbyte2", "→ value", "push a constant #index from a constant pool (double or long) onto the stack (wide index is constructed as indexbyte1 << 8 + indexbyte2)"),
    JVMOpCode.create("ldiv", 0x6d, "", "value1, value2 → result", "divide two longs"),
    JVMOpCode.create("lload", 0x16, "1: index", "→ value", "load a long value from a local variable #index"),
    JVMOpCode.create("lload_0", 0x1e, "", "→ value", "load a long value from a local variable 0"),
    JVMOpCode.create("lload_1", 0x1f, "", "→ value", "load a long value from a local variable 1"),
    JVMOpCode.create("lload_2", 0x20, "", "→ value", "load a long value from a local variable 2"),
    JVMOpCode.create("lload_3", 0x21, "", "→ value", "load a long value from a local variable 3"),
    JVMOpCode.create("lmul", 0x69, "", "value1, value2 → result", "multiply two longs"),
    JVMOpCode.create("lneg", 0x75, "", "value → result", "negate a long"),
    JVMOpCode.create("lookupswitch", 0xab, "8+: <0–3 bytes padding>, defaultbyte1, defaultbyte2, defaultbyte3, defaultbyte4, npairs1, npairs2, npairs3, npairs4, match-offset pairs...", "key →", "a target address is looked up from a table using a key and execution continues from the instruction at that address"),
    JVMOpCode.create("lor", 0x81, "", "value1, value2 → result", "bitwise OR of two longs"),
    JVMOpCode.create("lrem", 0x71, "", "value1, value2 → result", "remainder of division of two longs"),
    JVMOpCode.create("lreturn", 0xad, "", "value → [empty]", "return a long value"),
    JVMOpCode.create("lshl", 0x79, "", "value1, value2 → result", "bitwise shift left of a long value1 by int value2 positions"),
    JVMOpCode.create("lshr", 0x7b, "", "value1, value2 → result", "bitwise shift right of a long value1 by int value2 positions"),
    JVMOpCode.create("lstore", 0x37, "1: index", "value →", "store a long value in a local variable #index"),
    JVMOpCode.create("lstore_0", 0x3f, "", "value →", "store a long value in a local variable 0"),
    JVMOpCode.create("lstore_1", 0x40, "", "value →", "store a long value in a local variable 1"),
    JVMOpCode.create("lstore_2", 0x41, "", "value →", "store a long value in a local variable 2"),
    JVMOpCode.create("lstore_3", 0x42, "", "value →", "store a long value in a local variable 3"),
    JVMOpCode.create("lsub", 0x65, "", "value1, value2 → result", "subtract two longs"),
    JVMOpCode.create("lushr", 0x7d, "", "value1, value2 → result", "bitwise shift right of a long value1 by int value2 positions, unsigned"),
    JVMOpCode.create("lxor", 0x83, "", "value1, value2 → result", "bitwise XOR of two longs"),
    JVMOpCode.create("monitorenter", 0xc2, "", "objectref →", "enter monitor for object ('grab the lock' – start of synchronized() section)"),
    JVMOpCode.create("monitorexit", 0xc3, "", "objectref →", "exit monitor for object ('release the lock' – end of synchronized() section)"),
    JVMOpCode.create("multianewarray", 0xc5, "3: indexbyte1, indexbyte2, dimensions", "count1, [count2,...] → arrayref", "create a new array of dimensions dimensions of type identified by class reference in constant pool index (indexbyte1 << 8 + indexbyte2); the sizes of each dimension is identified by count1, [count2, etc.]"),
    JVMOpCode.create("new", 0xbb, "2: indexbyte1, indexbyte2", "→ objectref", "create new object of type identified by class reference in constant pool index (indexbyte1 << 8 + indexbyte2)"),
    JVMOpCode.create("newarray", 0xbc, "1: atype", "count → arrayref", "create new array with count elements of primitive type identified by atype"),
    JVMOpCode.create("nop", 0x00, "", "[No change]", "perform no operation"),
    JVMOpCode.create("pop", 0x57, "", "value →", "discard the top value on the stack"),
    JVMOpCode.create("pop2", 0x58, "", "{value2, value1} →", "discard the top two values on the stack (or one value, if it is a double or long)"),
    JVMOpCode.create("putfield", 0xb5, "2: indexbyte1, indexbyte2", "objectref, value →", "set field to value in an object objectref, where the field is identified by a field reference index in constant pool (indexbyte1 << 8 + indexbyte2)"),
    JVMOpCode.create("putstatic", 0xb3, "2: indexbyte1, indexbyte2", "value →", "set static field to value in a class, where the field is identified by a field reference index in constant pool (indexbyte1 << 8 + indexbyte2)"),
    JVMOpCode.create("ret", 0xa9, "1: index", "[No change]", "continue execution from address taken from a local variable #index (the asymmetry with jsr is intentional)"),
    JVMOpCode.create("return", 0xb1, "", "→ [empty]", "return void from method"),
    JVMOpCode.create("saload", 0x35, "", "arrayref, index → value", "load short from array"),
    JVMOpCode.create("sastore", 0x56, "", "arrayref, index, value →", "store short to array"),
    JVMOpCode.create("sipush", 0x11, "2: byte1, byte2", "→ value", "push a short onto the stack as an integer value"),
    JVMOpCode.create("swap", 0x5f, "", "value2, value1 → value1, value2", "swaps two top words on the stack (note that value1 and value2 must not be double or long)"),
    JVMOpCode.create("tableswitch", 0xaa, "16+: [0–3 bytes padding], defaultbyte1, defaultbyte2, defaultbyte3, defaultbyte4, lowbyte1, lowbyte2, lowbyte3, lowbyte4, highbyte1, highbyte2, highbyte3, highbyte4, jump offsets...", "index →", "continue execution from an address in the table at offset index"),
    JVMOpCode.create("wide", 0xc4, "3/5: opcode, indexbyte1, indexbyte2\nor\niinc, indexbyte1, indexbyte2, countbyte1, countbyte2", "[same as for corresponding instructions]", "execute opcode, where opcode is either iload, fload, aload, lload, dload, istore, fstore, astore, lstore, dstore, or ret, but assume the index is 16 bit; or execute iinc, where the index is 16 bits and the constant to increment by is a signed 16 bit short")
  )

  private val opcodesMap: Map[Int, JVMOpCode] = opcodes.map(op => op.hexcode -> op).toMap

  def getOpcode(hex: Int): JVMOpCode = opcodesMap(hex)
}

