package compiling

import java.io.DataInputStream

import jvm.JVMClassFileReaderUtils

//sealed trait JVMOpCode {
//  val hexcode: Int
//  val name: String
//  val desc: String
//  val lengthInBytes: Byte
//  val args: Vector[JVMOpCodeArg]
//
//  // Assumes the hexcode has just been read from 'in', and reads and prints subsequent args
//  def gen(in: DataInputStream): String
//}

case class JVMOpCodeArg(lengthBytes: Int, name: String) {
  def gen(in: DataInputStream): String = {
    lengthBytes match {
      case 1 =>
        val value = JVMClassFileReaderUtils.readByteTwosComplement(in)
        value.toString
      case 2 =>
        val value = JVMClassFileReaderUtils.readShortTwosComplement(in)
        value.toString
      case 4 =>
        val value = JVMClassFileReaderUtils.readIntTwosComplement(in)
        value.toString
    }
  }
}

case class JVMOpCode(hexcode: Int, name: String, lengthInBytes: Int, args: Array[JVMOpCodeArg], unsupported: Boolean) {
  def gen(in: DataInputStream): String = {
    name + " " + args.map(_.gen(in)).mkString(" ") + (if (unsupported) " DANGER: UNSUPPORTED OPCODE, REMAINING DECODE WILL BE INCORRECT" else "")
  }
}


object JVMOpCode {
  def create(name: String, hexcode: Int, variablesDesc: String, stackDesc: String): JVMOpCode = {
    //    val numArgs = if (variablesDesc.nonEmpty) variablesDesc.head.toByte else 0
    //    val lengthInBytes: Byte = (1 + numArgs).toByte

    var unsupported = false

    val args: Array[JVMOpCodeArg] = variablesDesc.trim match {
      case ""                                                      => Array()
      case "1: atype"                                              => Array(JVMOpCodeArg(1, "atype"))
      case "1: index"                                              => Array(JVMOpCodeArg(1, "index"))
      case "2: byte1, byte2"                                       => Array(JVMOpCodeArg(2, "value"))
      case "2: indexbyte1, indexbyte2"                             => Array(JVMOpCodeArg(2, "index"))
      case "1: byte"                                               => Array(JVMOpCodeArg(1, "byte"))
      case "2: branchbyte1, branchbyte2"                           => Array(JVMOpCodeArg(2, "branch"))
      case "4: branchbyte1, branchbyte2, branchbyte3, branchbyte4" => Array(JVMOpCodeArg(4, "branch"))
      case "2: index, const"                                       => Array(JVMOpCodeArg(1, "index"), JVMOpCodeArg(1, "const"))
      case "4: indexbyte1, indexbyte2, 0, 0"                       => Array(JVMOpCodeArg(2, "index"), JVMOpCodeArg(2, "ignored"))
      case "4: indexbyte1, indexbyte2, count, 0"                   => Array(JVMOpCodeArg(2, "index"), JVMOpCodeArg(1, "count"), JVMOpCodeArg(1, "ignored"))
      case _                                                       =>
        unsupported = true
        Array()
    }

    val lenInBytes = 1 + args.foldLeft(0)((a, b) => a + b.lengthBytes)

    JVMOpCode(hexcode, name, lenInBytes, args, unsupported)
  }
}

object JVMOpCodes {
  // Created from https://en.wikipedia.org/wiki/Java_bytecode_instruction_listings and regex:
  // \s*(.*?)	(..)\t.... ....\t(.*?)\t(.*?)\t(.*)
  // val $1 = JVMOpCode.create("$1", 0x$2, "$3", "$4")\n
  val aaload = JVMOpCode.create("aaload", 0x32, "", "arrayref, index → value")
  val aastore = JVMOpCode.create("aastore", 0x53, "", "arrayref, index, value →")
  val aconst_null = JVMOpCode.create("aconst_null", 0x01, "", "→ null")
  val aload = JVMOpCode.create("aload", 0x19, "1: index", "→ objectref")
  val aload_0 = JVMOpCode.create("aload_0", 0x2a, "", "→ objectref")
  val aload_1 = JVMOpCode.create("aload_1", 0x2b, "", "→ objectref")
  val aload_2 = JVMOpCode.create("aload_2", 0x2c, "", "→ objectref")
  val aload_3 = JVMOpCode.create("aload_3", 0x2d, "", "→ objectref")
  val anewarray = JVMOpCode.create("anewarray", 0xbd, "2: indexbyte1, indexbyte2", "count → arrayref")
  val areturn = JVMOpCode.create("areturn", 0xb0, "", "objectref → [empty]")
  val arraylength = JVMOpCode.create("arraylength", 0xbe, "", "arrayref → length")
  val astore = JVMOpCode.create("astore", 0x3a, "1: index", "objectref →")
  val astore_0 = JVMOpCode.create("astore_0", 0x4b, "", "objectref →")
  val astore_1 = JVMOpCode.create("astore_1", 0x4c, "", "objectref →")
  val astore_2 = JVMOpCode.create("astore_2", 0x4d, "", "objectref →")
  val astore_3 = JVMOpCode.create("astore_3", 0x4e, "", "objectref →")
  val athrow = JVMOpCode.create("athrow", 0xbf, "", "objectref → [empty], objectref")
  val baload = JVMOpCode.create("baload", 0x33, "", "arrayref, index → value")
  val bastore = JVMOpCode.create("bastore", 0x54, "", "arrayref, index, value →")
  val bipush = JVMOpCode.create("bipush", 0x10, "1: byte", "→ value")
  val breakpoint = JVMOpCode.create("breakpoint", 0xca, "", "")
  val caload = JVMOpCode.create("caload", 0x34, "", "arrayref, index → value")
  val castore = JVMOpCode.create("castore", 0x55, "", "arrayref, index, value →")
  val checkcast = JVMOpCode.create("checkcast", 0xc0, "2: indexbyte1, indexbyte2", "objectref → objectref")
  val d2f = JVMOpCode.create("d2f", 0x90, "", "value → result")
  val d2i = JVMOpCode.create("d2i", 0x8e, "", "value → result")
  val d2l = JVMOpCode.create("d2l", 0x8f, "", "value → result")
  val dadd = JVMOpCode.create("dadd", 0x63, "", "value1, value2 → result")
  val daload = JVMOpCode.create("daload", 0x31, "", "arrayref, index → value")
  val dastore = JVMOpCode.create("dastore", 0x52, "", "arrayref, index, value →")
  val dcmpg = JVMOpCode.create("dcmpg", 0x98, "", "value1, value2 → result")
  val dcmpl = JVMOpCode.create("dcmpl", 0x97, "", "value1, value2 → result")
  val dconst_0 = JVMOpCode.create("dconst_0", 0x0e, "", "→ 0.0")
  val dconst_1 = JVMOpCode.create("dconst_1", 0x0f, "", "→ 1.0")
  val ddiv = JVMOpCode.create("ddiv", 0x6f, "", "value1, value2 → result")
  val dload = JVMOpCode.create("dload", 0x18, "1: index", "→ value")
  val dload_0 = JVMOpCode.create("dload_0", 0x26, "", "→ value")
  val dload_1 = JVMOpCode.create("dload_1", 0x27, "", "→ value")
  val dload_2 = JVMOpCode.create("dload_2", 0x28, "", "→ value")
  val dload_3 = JVMOpCode.create("dload_3", 0x29, "", "→ value")
  val dmul = JVMOpCode.create("dmul", 0x6b, "", "value1, value2 → result")
  val dneg = JVMOpCode.create("dneg", 0x77, "", "value → result")
  val drem = JVMOpCode.create("drem", 0x73, "", "value1, value2 → result")
  val dreturn = JVMOpCode.create("dreturn", 0xaf, "", "value → [empty]")
  val dstore = JVMOpCode.create("dstore", 0x39, "1: index", "value →")
  val dstore_0 = JVMOpCode.create("dstore_0", 0x47, "", "value →")
  val dstore_1 = JVMOpCode.create("dstore_1", 0x48, "", "value →")
  val dstore_2 = JVMOpCode.create("dstore_2", 0x49, "", "value →")
  val dstore_3 = JVMOpCode.create("dstore_3", 0x4a, "", "value →")
  val dsub = JVMOpCode.create("dsub", 0x67, "", "value1, value2 → result")
  val dup = JVMOpCode.create("dup", 0x59, "", "value → value, value")
  val dup_x1 = JVMOpCode.create("dup_x1", 0x5a, "", "value2, value1 → value1, value2, value1")
  val dup_x2 = JVMOpCode.create("dup_x2", 0x5b, "", "value3, value2, value1 → value1, value3, value2, value1")
  val dup2 = JVMOpCode.create("dup2", 0x5c, "", "{value2, value1} → {value2, value1}, {value2, value1}")
  val dup2_x1 = JVMOpCode.create("dup2_x1", 0x5d, "", "value3, {value2, value1} → {value2, value1}, value3, {value2, value1}")
  val dup2_x2 = JVMOpCode.create("dup2_x2", 0x5e, "", "{value4, value3}, {value2, value1} → {value2, value1}, {value4, value3}, {value2, value1}")
  val f2d = JVMOpCode.create("f2d", 0x8d, "", "value → result")
  val f2i = JVMOpCode.create("f2i", 0x8b, "", "value → result")
  val f2l = JVMOpCode.create("f2l", 0x8c, "", "value → result")
  val fadd = JVMOpCode.create("fadd", 0x62, "", "value1, value2 → result")
  val faload = JVMOpCode.create("faload", 0x30, "", "arrayref, index → value")
  val fastore = JVMOpCode.create("fastore", 0x51, "", "arrayref, index, value →")
  val fcmpg = JVMOpCode.create("fcmpg", 0x96, "", "value1, value2 → result")
  val fcmpl = JVMOpCode.create("fcmpl", 0x95, "", "value1, value2 → result")
  val fconst_0 = JVMOpCode.create("fconst_0", 0x0b, "", "→ 0.0f")
  val fconst_1 = JVMOpCode.create("fconst_1", 0x0c, "", "→ 1.0f")
  val fconst_2 = JVMOpCode.create("fconst_2", 0x0d, "", "→ 2.0f")
  val fdiv = JVMOpCode.create("fdiv", 0x6e, "", "value1, value2 → result")
  val fload = JVMOpCode.create("fload", 0x17, "1: index", "→ value")
  val fload_0 = JVMOpCode.create("fload_0", 0x22, "", "→ value")
  val fload_1 = JVMOpCode.create("fload_1", 0x23, "", "→ value")
  val fload_2 = JVMOpCode.create("fload_2", 0x24, "", "→ value")
  val fload_3 = JVMOpCode.create("fload_3", 0x25, "", "→ value")
  val fmul = JVMOpCode.create("fmul", 0x6a, "", "value1, value2 → result")
  val fneg = JVMOpCode.create("fneg", 0x76, "", "value → result")
  val frem = JVMOpCode.create("frem", 0x72, "", "value1, value2 → result")
  val freturn = JVMOpCode.create("freturn", 0xae, "", "value → [empty]")
  val fstore = JVMOpCode.create("fstore", 0x38, "1: index", "value →")
  val fstore_0 = JVMOpCode.create("fstore_0", 0x43, "", "value →")
  val fstore_1 = JVMOpCode.create("fstore_1", 0x44, "", "value →")
  val fstore_2 = JVMOpCode.create("fstore_2", 0x45, "", "value →")
  val fstore_3 = JVMOpCode.create("fstore_3", 0x46, "", "value →")
  val fsub = JVMOpCode.create("fsub", 0x66, "", "value1, value2 → result")
  val getfield = JVMOpCode.create("getfield", 0xb4, "2: indexbyte1, indexbyte2", "objectref → value")
  val getstatic = JVMOpCode.create("getstatic", 0xb2, "2: indexbyte1, indexbyte2", "→ value")
  val goto = JVMOpCode.create("goto", 0xa7, "2: branchbyte1, branchbyte2", "[no change]")
  val goto_w = JVMOpCode.create("goto_w", 0xc8, "4: branchbyte1, branchbyte2, branchbyte3, branchbyte4", "[no change]")
  val i2b = JVMOpCode.create("i2b", 0x91, "", "value → result")
  val i2c = JVMOpCode.create("i2c", 0x92, "", "value → result")
  val i2d = JVMOpCode.create("i2d", 0x87, "", "value → result")
  val i2f = JVMOpCode.create("i2f", 0x86, "", "value → result")
  val i2l = JVMOpCode.create("i2l", 0x85, "", "value → result")
  val i2s = JVMOpCode.create("i2s", 0x93, "", "value → result")
  val iadd = JVMOpCode.create("iadd", 0x60, "", "value1, value2 → result")
  val iaload = JVMOpCode.create("iaload", 0x2e, "", "arrayref, index → value")
  val iand = JVMOpCode.create("iand", 0x7e, "", "value1, value2 → result")
  val iastore = JVMOpCode.create("iastore", 0x4f, "", "arrayref, index, value →")
  val iconst_m1 = JVMOpCode.create("iconst_m1", 0x02, "", "→ -1")
  val iconst_0 = JVMOpCode.create("iconst_0", 0x03, "", "→ 0")
  val iconst_1 = JVMOpCode.create("iconst_1", 0x04, "", "→ 1")
  val iconst_2 = JVMOpCode.create("iconst_2", 0x05, "", "→ 2")
  val iconst_3 = JVMOpCode.create("iconst_3", 0x06, "", "→ 3")
  val iconst_4 = JVMOpCode.create("iconst_4", 0x07, "", "→ 4")
  val iconst_5 = JVMOpCode.create("iconst_5", 0x08, "", "→ 5")
  val idiv = JVMOpCode.create("idiv", 0x6c, "", "value1, value2 → result")
  val if_acmpeq = JVMOpCode.create("if_acmpeq", 0xa5, "2: branchbyte1, branchbyte2", "value1, value2 →")
  val if_acmpne = JVMOpCode.create("if_acmpne", 0xa6, "2: branchbyte1, branchbyte2", "value1, value2 →")
  val if_icmpeq = JVMOpCode.create("if_icmpeq", 0x9f, "2: branchbyte1, branchbyte2", "value1, value2 →")
  val if_icmpge = JVMOpCode.create("if_icmpge", 0xa2, "2: branchbyte1, branchbyte2", "value1, value2 →")
  val if_icmpgt = JVMOpCode.create("if_icmpgt", 0xa3, "2: branchbyte1, branchbyte2", "value1, value2 →")
  val if_icmple = JVMOpCode.create("if_icmple", 0xa4, "2: branchbyte1, branchbyte2", "value1, value2 →")
  val if_icmplt = JVMOpCode.create("if_icmplt", 0xa1, "2: branchbyte1, branchbyte2", "value1, value2 →")
  val if_icmpne = JVMOpCode.create("if_icmpne", 0xa0, "2: branchbyte1, branchbyte2", "value1, value2 →")
  val ifeq = JVMOpCode.create("ifeq", 0x99, "2: branchbyte1, branchbyte2", "value →")
  val ifge = JVMOpCode.create("ifge", 0x9c, "2: branchbyte1, branchbyte2", "value →")
  val ifgt = JVMOpCode.create("ifgt", 0x9d, "2: branchbyte1, branchbyte2", "value →")
  val ifle = JVMOpCode.create("ifle", 0x9e, "2: branchbyte1, branchbyte2", "value →")
  val iflt = JVMOpCode.create("iflt", 0x9b, "2: branchbyte1, branchbyte2", "value →")
  val ifne = JVMOpCode.create("ifne", 0x9a, "2: branchbyte1, branchbyte2", "value →")
  val ifnonnull = JVMOpCode.create("ifnonnull", 0xc7, "2: branchbyte1, branchbyte2", "value →")
  val ifnull = JVMOpCode.create("ifnull", 0xc6, "2: branchbyte1, branchbyte2", "value →")
  val iinc = JVMOpCode.create("iinc", 0x84, "2: index, const", "[No change]")
  val iload = JVMOpCode.create("iload", 0x15, "1: index", "→ value")
  val iload_0 = JVMOpCode.create("iload_0", 0x1a, "", "→ value")
  val iload_1 = JVMOpCode.create("iload_1", 0x1b, "", "→ value")
  val iload_2 = JVMOpCode.create("iload_2", 0x1c, "", "→ value")
  val iload_3 = JVMOpCode.create("iload_3", 0x1d, "", "→ value")
  val impdep1 = JVMOpCode.create("impdep1", 0xfe, "", "")
  val impdep2 = JVMOpCode.create("impdep2", 0xff, "", "")
  val imul = JVMOpCode.create("imul", 0x68, "", "value1, value2 → result")
  val ineg = JVMOpCode.create("ineg", 0x74, "", "value → result")
  val instanceof = JVMOpCode.create("instanceof", 0xc1, "2: indexbyte1, indexbyte2", "objectref → result")
  val invokedynamic = JVMOpCode.create("invokedynamic", 0xba, "4: indexbyte1, indexbyte2, 0, 0", "[arg1, [arg2 ...]] → result")
  val invokeinterface = JVMOpCode.create("invokeinterface", 0xb9, "4: indexbyte1, indexbyte2, count, 0", "objectref, [arg1, arg2, ...] → result")
  val invokespecial = JVMOpCode.create("invokespecial", 0xb7, "2: indexbyte1, indexbyte2", "objectref, [arg1, arg2, ...] → result")
  val invokestatic = JVMOpCode.create("invokestatic", 0xb8, "2: indexbyte1, indexbyte2", "[arg1, arg2, ...] → result")
  val invokevirtual = JVMOpCode.create("invokevirtual", 0xb6, "2: indexbyte1, indexbyte2", "objectref, [arg1, arg2, ...] → result")
  val ior = JVMOpCode.create("ior", 0x80, "", "value1, value2 → result")
  val irem = JVMOpCode.create("irem", 0x70, "", "value1, value2 → result")
  val ireturn = JVMOpCode.create("ireturn", 0xac, "", "value → [empty]")
  val ishl = JVMOpCode.create("ishl", 0x78, "", "value1, value2 → result")
  val ishr = JVMOpCode.create("ishr", 0x7a, "", "value1, value2 → result")
  val istore = JVMOpCode.create("istore", 0x36, "1: index", "value →")
  val istore_0 = JVMOpCode.create("istore_0", 0x3b, "", "value →")
  val istore_1 = JVMOpCode.create("istore_1", 0x3c, "", "value →")
  val istore_2 = JVMOpCode.create("istore_2", 0x3d, "", "value →")
  val istore_3 = JVMOpCode.create("istore_3", 0x3e, "", "value →")
  val isub = JVMOpCode.create("isub", 0x64, "", "value1, value2 → result")
  val iushr = JVMOpCode.create("iushr", 0x7c, "", "value1, value2 → result")
  val ixor = JVMOpCode.create("ixor", 0x82, "", "value1, value2 → result")
  val jsr = JVMOpCode.create("jsr", 0xa8, "2: branchbyte1, branchbyte2", "→ address")
  val jsr_w = JVMOpCode.create("jsr_w", 0xc9, "4: branchbyte1, branchbyte2, branchbyte3, branchbyte4", "→ address")
  val l2d = JVMOpCode.create("l2d", 0x8a, "", "value → result")
  val l2f = JVMOpCode.create("l2f", 0x89, "", "value → result")
  val l2i = JVMOpCode.create("l2i", 0x88, "", "value → result")
  val ladd = JVMOpCode.create("ladd", 0x61, "", "value1, value2 → result")
  val laload = JVMOpCode.create("laload", 0x2f, "", "arrayref, index → value")
  val land = JVMOpCode.create("land", 0x7f, "", "value1, value2 → result")
  val lastore = JVMOpCode.create("lastore", 0x50, "", "arrayref, index, value →")
  val lcmp = JVMOpCode.create("lcmp", 0x94, "", "value1, value2 → result")
  val lconst_0 = JVMOpCode.create("lconst_0", 0x09, "", "→ 0L")
  val lconst_1 = JVMOpCode.create("lconst_1", 0x0a, "", "→ 1L")
  val ldc = JVMOpCode.create("ldc", 0x12, "1: index", "→ value")
  val ldc_w = JVMOpCode.create("ldc_w", 0x13, "2: indexbyte1, indexbyte2", "→ value")
  val ldc2_w = JVMOpCode.create("ldc2_w", 0x14, "2: indexbyte1, indexbyte2", "→ value")
  val ldiv = JVMOpCode.create("ldiv", 0x6d, "", "value1, value2 → result")
  val lload = JVMOpCode.create("lload", 0x16, "1: index", "→ value")
  val lload_0 = JVMOpCode.create("lload_0", 0x1e, "", "→ value")
  val lload_1 = JVMOpCode.create("lload_1", 0x1f, "", "→ value")
  val lload_2 = JVMOpCode.create("lload_2", 0x20, "", "→ value")
  val lload_3 = JVMOpCode.create("lload_3", 0x21, "", "→ value")
  val lmul = JVMOpCode.create("lmul", 0x69, "", "value1, value2 → result")
  val lneg = JVMOpCode.create("lneg", 0x75, "", "value → result")
  val lookupswitch = JVMOpCode.create("lookupswitch", 0xab, "8+: <0–3 bytes padding>, defaultbyte1, defaultbyte2, defaultbyte3, defaultbyte4, npairs1, npairs2, npairs3, npairs4, match-offset pairs...", "key →")
  val lor = JVMOpCode.create("lor", 0x81, "", "value1, value2 → result")
  val lrem = JVMOpCode.create("lrem", 0x71, "", "value1, value2 → result")
  val lreturn = JVMOpCode.create("lreturn", 0xad, "", "value → [empty]")
  val lshl = JVMOpCode.create("lshl", 0x79, "", "value1, value2 → result")
  val lshr = JVMOpCode.create("lshr", 0x7b, "", "value1, value2 → result")
  val lstore = JVMOpCode.create("lstore", 0x37, "1: index", "value →")
  val lstore_0 = JVMOpCode.create("lstore_0", 0x3f, "", "value →")
  val lstore_1 = JVMOpCode.create("lstore_1", 0x40, "", "value →")
  val lstore_2 = JVMOpCode.create("lstore_2", 0x41, "", "value →")
  val lstore_3 = JVMOpCode.create("lstore_3", 0x42, "", "value →")
  val lsub = JVMOpCode.create("lsub", 0x65, "", "value1, value2 → result")
  val lushr = JVMOpCode.create("lushr", 0x7d, "", "value1, value2 → result")
  val lxor = JVMOpCode.create("lxor", 0x83, "", "value1, value2 → result")
  val monitorenter = JVMOpCode.create("monitorenter", 0xc2, "", "objectref →")
  val monitorexit = JVMOpCode.create("monitorexit", 0xc3, "", "objectref →")
  val multianewarray = JVMOpCode.create("multianewarray", 0xc5, "3: indexbyte1, indexbyte2, dimensions", "count1, [count2,...] → arrayref")
  val new_ = JVMOpCode.create("new", 0xbb, "2: indexbyte1, indexbyte2", "→ objectref")
  val newarray = JVMOpCode.create("newarray", 0xbc, "1: atype", "count → arrayref")
  val nop = JVMOpCode.create("nop", 0x00, "", "[No change]")
  val pop = JVMOpCode.create("pop", 0x57, "", "value →")
  val pop2 = JVMOpCode.create("pop2", 0x58, "", "{value2, value1} →")
  val putfield = JVMOpCode.create("putfield", 0xb5, "2: indexbyte1, indexbyte2", "objectref, value →")
  val putstatic = JVMOpCode.create("putstatic", 0xb3, "2: indexbyte1, indexbyte2", "value →")
  val ret = JVMOpCode.create("ret", 0xa9, "1: index", "[No change]")
  val return_ = JVMOpCode.create("return", 0xb1, "", "→ [empty]")
  val saload = JVMOpCode.create("saload", 0x35, "", "arrayref, index → value")
  val sastore = JVMOpCode.create("sastore", 0x56, "", "arrayref, index, value →")
  val sipush = JVMOpCode.create("sipush", 0x11, "2: byte1, byte2", "→ value")
  val swap = JVMOpCode.create("swap", 0x5f, "", "value2, value1 → value1, value2")
  val tableswitch = JVMOpCode.create("tableswitch", 0xaa, "16+: [0–3 bytes padding], defaultbyte1, defaultbyte2, defaultbyte3, defaultbyte4, lowbyte1, lowbyte2, lowbyte3, lowbyte4, highbyte1, highbyte2, highbyte3, highbyte4, jump offsets...", "index →")
  val wide = JVMOpCode.create("wide", 0xc4, "3/5: opcode, indexbyte1, indexbyte2\nor\niinc, indexbyte1, indexbyte2, countbyte1, countbyte2", "[same as for corresponding instructions]")


  val opcodes: Seq[JVMOpCode] = Vector(
    aaload,
    aastore,
    aconst_null,
    aload,
    aload_0,
    aload_1,
    aload_2,
    aload_3,
    anewarray,
    areturn,
    arraylength,
    astore,
    astore_0,
    astore_1,
    astore_2,
    astore_3,
    athrow,
    baload,
    bastore,
    bipush,
    breakpoint,
    caload,
    castore,
    checkcast,
    d2f,
    d2i,
    d2l,
    dadd,
    daload,
    dastore,
    dcmpg,
    dcmpl,
    dconst_0,
    dconst_1,
    ddiv,
    dload,
    dload_0,
    dload_1,
    dload_2,
    dload_3,
    dmul,
    dneg,
    drem,
    dreturn,
    dstore,
    dstore_0,
    dstore_1,
    dstore_2,
    dstore_3,
    dsub,
    dup,
    dup_x1,
    dup_x2,
    dup2,
    dup2_x1,
    dup2_x2,
    f2d,
    f2i,
    f2l,
    fadd,
    faload,
    fastore,
    fcmpg,
    fcmpl,
    fconst_0,
    fconst_1,
    fconst_2,
    fdiv,
    fload,
    fload_0,
    fload_1,
    fload_2,
    fload_3,
    fmul,
    fneg,
    frem,
    freturn,
    fstore,
    fstore_0,
    fstore_1,
    fstore_2,
    fstore_3,
    fsub,
    getfield,
    getstatic,
    goto,
    goto_w,
    i2b,
    i2c,
    i2d,
    i2f,
    i2l,
    i2s,
    iadd,
    iaload,
    iand,
    iastore,
    iconst_m1,
    iconst_0,
    iconst_1,
    iconst_2,
    iconst_3,
    iconst_4,
    iconst_5,
    idiv,
    if_acmpeq,
    if_acmpne,
    if_icmpeq,
    if_icmpge,
    if_icmpgt,
    if_icmple,
    if_icmplt,
    if_icmpne,
    ifeq,
    ifge,
    ifgt,
    ifle,
    iflt,
    ifne,
    ifnonnull,
    ifnull,
    iinc,
    iload,
    iload_0,
    iload_1,
    iload_2,
    iload_3,
    impdep1,
    impdep2,
    imul,
    ineg,
    instanceof,
    invokedynamic,
    invokeinterface,
    invokespecial,
    invokestatic,
    invokevirtual,
    ior,
    irem,
    ireturn,
    ishl,
    ishr,
    istore,
    istore_0,
    istore_1,
    istore_2,
    istore_3,
    isub,
    iushr,
    ixor,
    jsr,
    jsr_w,
    l2d,
    l2f,
    l2i,
    ladd,
    laload,
    land,
    lastore,
    lcmp,
    lconst_0,
    lconst_1,
    ldc,
    ldc_w,
    ldc2_w,
    ldiv,
    lload,
    lload_0,
    lload_1,
    lload_2,
    lload_3,
    lmul,
    lneg,
    lookupswitch,
    lor,
    lrem,
    lreturn,
    lshl,
    lshr,
    lstore,
    lstore_0,
    lstore_1,
    lstore_2,
    lstore_3,
    lsub,
    lushr,
    lxor,
    monitorenter,
    monitorexit,
    multianewarray,
    new_,
    newarray,
    nop,
    pop,
    pop2,
    putfield,
    putstatic,
    ret,
    return_,
    saload,
    sastore,
    sipush,
    swap,
    tableswitch,
    wide
  )

  private val opcodesMap: Map[Int, JVMOpCode] = opcodes.map(op => op.hexcode -> op).toMap

  def getOpcode(hex: Int): JVMOpCode = opcodesMap(hex)
}

