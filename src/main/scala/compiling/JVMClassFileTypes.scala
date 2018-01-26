package compiling

import java.io.{ByteArrayOutputStream, PrintWriter}
import java.nio.charset.Charset

import compiling.JVMByteCode._

object JVMClassFileTypes {

  /*
  cp_info {
    u1 tag;
    u1 info[];
  }
  */
  sealed trait CONSTANT {
    def tag(): Int
    def write(out: ByteArrayOutputStream, charset: Charset): Unit
  }
  /*
      CONSTANT_Class_info {
        u1 tag;
        u2 name_index;
      }
   */
  case class CONSTANT_Class_info(nameIndex: Int) extends CONSTANT {
    override def tag() = 7
    override def write(out: ByteArrayOutputStream, charset: Charset): Unit = {
      JVMClassFileBuilderUtils.writeByte(out, tag())
      JVMClassFileBuilderUtils.writeShort(out, nameIndex)
    }
  }
  /*
      CONSTANT_Utf8_info {
        u1 tag;
        u2 length;
        u1 bytes[length];
      }
   */
  case class CONSTANT_Utf8_info(value: String) extends CONSTANT {
    override def tag() = 1
    override def write(out: ByteArrayOutputStream, charset: Charset): Unit = {
      JVMClassFileBuilderUtils.writeByte(out, tag())
      JVMClassFileBuilderUtils.writeShort(out, value.length)
      out.write(value.getBytes(charset))
    }
  }
  /*
      CONSTANT_Fieldref_info {
        u1 tag;
        u2 class_index;
        u2 name_and_type_index;
      }
   */
  case class CONSTANT_Fieldref_info(classIndex: Int, nameAndTypeIndex: Int) extends CONSTANT {
    override def tag() = 9
    override def write(out: ByteArrayOutputStream, charset: Charset): Unit = {
      JVMClassFileBuilderUtils.writeByte(out, tag())
      JVMClassFileBuilderUtils.writeShort(out, classIndex)
      JVMClassFileBuilderUtils.writeShort(out, nameAndTypeIndex)
    }
  }
  case class CONSTANT_Methodref_info(classIndex: Int, nameAndTypeIndex: Int) extends CONSTANT {
    override def tag() = 10
    override def write(out: ByteArrayOutputStream, charset: Charset): Unit = {
      JVMClassFileBuilderUtils.writeByte(out, tag())
      JVMClassFileBuilderUtils.writeShort(out, classIndex)
      JVMClassFileBuilderUtils.writeShort(out, nameAndTypeIndex)
    }
  }
  case class CONSTANT_InterfaceMethodref_info(classIndex: Int, nameAndTypeIndex: Int) extends CONSTANT {
    override def tag() = 11
    override def write(out: ByteArrayOutputStream, charset: Charset): Unit = {
      JVMClassFileBuilderUtils.writeByte(out, tag())
      JVMClassFileBuilderUtils.writeShort(out, classIndex)
      JVMClassFileBuilderUtils.writeShort(out, nameAndTypeIndex)
    }
  }
  /*
      CONSTANT_String_info {
        u1 tag;
        u2 string_index;
      }
   */
  case class CONSTANT_String_info(stringIndex: Int) extends CONSTANT {
    override def tag() = 8
    override def write(out: ByteArrayOutputStream, charset: Charset): Unit = {
      JVMClassFileBuilderUtils.writeByte(out, tag())
      JVMClassFileBuilderUtils.writeShort(out, stringIndex)
    }
  }
  /*
      CONSTANT_Integer_info {
        u1 tag;
        u4 bytes;
      }
   */
  case class CONSTANT_Integer_info(value: Int) extends CONSTANT {
    override def tag() = 3
    override def write(out: ByteArrayOutputStream, charset: Charset): Unit = {
      JVMClassFileBuilderUtils.writeByte(out, tag())
      JVMClassFileBuilderUtils.writeInt(out, value)
    }
  }
  case class CONSTANT_Float_info(value: Float) extends CONSTANT {
    override def tag() = 4
    override def write(out: ByteArrayOutputStream, charset: Charset): Unit = {
      JVMClassFileBuilderUtils.writeByte(out, tag())
      JVMClassFileBuilderUtils.writeFloat(out, value)
    }
  }
  /*
      CONSTANT_Long_info {
        u1 tag;
        u4 high_bytes;
        u4 low_bytes;
      }
   */
  case class CONSTANT_Long_info(value: Long) extends CONSTANT {
    override def tag() = 5
    override def write(out: ByteArrayOutputStream, charset: Charset): Unit = {
      JVMClassFileBuilderUtils.writeByte(out, tag())
      JVMClassFileBuilderUtils.writeLong(out, value)
    }
  }
  case class CONSTANT_Double_info(value: Double) extends CONSTANT {
    override def tag() = 6
    override def write(out: ByteArrayOutputStream, charset: Charset): Unit = {
      JVMClassFileBuilderUtils.writeByte(out, tag())
      JVMClassFileBuilderUtils.writeDouble(out, value)
    }
  }
  /*
      CONSTANT_NameAndType_info {
        u1 tag;
        u2 name_index;
        u2 descriptor_index;
      }

   */
  case class CONSTANT_NameAndType_info(nameIndex: Int, descriptorIndex: Int) extends CONSTANT {
    override def tag() = 12
    override def write(out: ByteArrayOutputStream, charset: Charset): Unit = {
      JVMClassFileBuilderUtils.writeByte(out, tag())
      JVMClassFileBuilderUtils.writeShort(out, nameIndex)
      JVMClassFileBuilderUtils.writeShort(out, descriptorIndex)
    }
  }

  object JVMClassFileBuilderUtils {

    // The params a method takes and the return value
    // MethodDescriptor: ( ParameterDescriptor* ) ReturnDescriptor
    // ParameterDescriptor: FieldType
    // ReturnDescriptor: FieldType | V


//    def typesToStr(types: Types): String = {
//      if (types.types.length != 1) throw JVMByteCode.unsupported(s"Multiple types in ${types}")
//      types.types.head match {
//          // TODO handle C has signed and Java doesn't
//        case v: TypeSpecifierVoid     => "V"
//        case v: TypeSpecifierChar     => "C"
//        case v: TypeSpecifierShort    => "S"
//        case v: TypeSpecifierInt      => "I"
//        case v: TypeSpecifierLong     => "J"
//        case v: TypeSpecifierFloat    => "F"
//        case v: TypeSpecifierDouble   => "D"
//        case v: TypeSpecifierSigned   => throw unsupported("signed type")
//        case v: TypeSpecifierUnsigned => throw unsupported("unsigned type")
//        case v: TypeSpecifierBool     => "Z"
//        case v: TypeSpecifierComplex  => throw unsupported("_Complex type")
//        case _ => throw unsupported("unhandled type")
//
//      }
//    }

    def typesToStr(typ: JVMType): String = {
      typ match {
        case v: JVMTypeVoid           => "V"
        case v: JVMTypeChar           => "C"
        case v: JVMTypeShort => "S"
        case v: JVMTypeInt => "I"
        case v: JVMTypeLong => "J"
        case v: JVMTypeFloat => "F"
        case v: JVMTypeDouble => "D"
        case v: JVMTypeBoolean => "Z"
        case v: JVMTypeArray => "[" + typesToStr(v.typ)
        case v: JVMTypeString => "Ljava/lang/String;"
        case _ => throw unsupported(s"unhandled type ${typ}")
      }
    }

    def createMethodDescriptor(ret: JVMType, params: Seq[JVMType]): String = {
      s"(${params.map(typesToStr).mkString("")})${typesToStr(ret)}"
    }

    def writeByte(out: ByteArrayOutputStream, value: Int): Unit = {
//      assert (value <= Byte.MaxValue)
      out.write(value)
    }

    def writeShort(out: ByteArrayOutputStream, value: Int): Unit = {
      assert (value <= Byte.MaxValue) // TODO
      out.write(0)
      out.write(value)
    }

    def writeInt(out: ByteArrayOutputStream, value: Int): Unit = {
      assert (value <= Byte.MaxValue) // TODO
      out.write(0)
      out.write(0)
      out.write(0)
      out.write(value)
    }

    def writeFloat(out: ByteArrayOutputStream, value: Float): Unit = {
      assert (value <= Byte.MaxValue) // TODO
      out.write(0)
      out.write(0)
      out.write(0)
      out.write(value.toByte)
    }

    def writeLong(out: ByteArrayOutputStream, value: Long): Unit = {
      assert (value <= Byte.MaxValue) // TODO
      out.write(0)
      out.write(0)
      out.write(0)
      out.write(value.toByte)
    }

    def writeDouble(out: ByteArrayOutputStream, value: Double): Unit = {
      assert (value <= Byte.MaxValue) // TODO
      out.write(0)
      out.write(0)
      out.write(0)
      out.write(value.toByte)
    }
  }

  /*
      method_info {
        u2 access_flags;
        u2 name_index;
        u2 descriptor_index;
        u2 attributes_count;
        attribute_info attributes[attributes_count];
      }
   */
  case class method_info(accessFlags: Int,
                         nameIndex: Int,
                         descriptorIndex: Int,
//                         attributes: Seq[attribute_info]) {
                         attributes: Seq[Attribute]) {
    def write(out: ByteArrayOutputStream, charset: Charset): Unit = {
      JVMClassFileBuilderUtils.writeShort(out, accessFlags)
      JVMClassFileBuilderUtils.writeShort(out, nameIndex)
      JVMClassFileBuilderUtils.writeShort(out, descriptorIndex)
      JVMClassFileBuilderUtils.writeShort(out, attributes.length)
      attributes.foreach(_.write(out, charset))
    }

    def write(out: PrintWriter, charset: Charset): Unit = {
      attributes.foreach(_.write(out, charset))
    }
  }

  sealed trait Attribute {
    def lengthBytes(): Int
    def write(out: ByteArrayOutputStream, charset: Charset): Unit
    def write(out: PrintWriter, charset: Charset): Unit
  }

  /*
      attribute_info {
    	u2 attribute_name_index;
    	u4 attribute_length;
    	u1 info[attribute_length];
    }

      // Attributes are essentially unions, with the name indicating the type
   */
  case class attribute_info(attributeNameIndex: Int,
                            info: Attribute) {
    def lengthBytes(): Int = info.lengthBytes()

    def write(out: ByteArrayOutputStream, charset: Charset): Unit = {
      JVMClassFileBuilderUtils.writeShort(out, attributeNameIndex)
      val lengthBytes = info.lengthBytes()
      JVMClassFileBuilderUtils.writeInt(out, lengthBytes)
      info.write(out, charset)
    }
  }

  /*
    ConstantValue_attribute {
    	u2 attribute_name_index;
    	u4 attribute_length;
    	u2 constantvalue_index;
    }
   */
  case class ConstantValue_attribute(attributeNameIndex: Int,
                                     valueIndex: Int) extends Attribute {
    override def lengthBytes(): Int = 2+4+2
    def write(out: ByteArrayOutputStream, charset: Charset): Unit = {
      JVMClassFileBuilderUtils.writeShort(out, attributeNameIndex)
      JVMClassFileBuilderUtils.writeInt(out, 2)
      JVMClassFileBuilderUtils.writeShort(out, valueIndex)
    }

    override def write(out: PrintWriter, charset: Charset): Unit = {

    }
  }

  /*
  Code_attribute {
    	u2 attribute_name_index;
    	u4 attribute_length;
    	u2 max_stack;
    	u2 max_locals;
    	u4 code_length;
    	u1 code[code_length];
    	u2 exception_table_length;
    	{    	u2 start_pc;
    	      	u2 end_pc;
    	      	u2  handler_pc;
    	      	u2  catch_type;
    	}	exception_table[exception_table_length];
    	u2 attributes_count;
    	attribute_info attributes[attributes_count];
  }

  Contains the code for a single method
 */
  case class Code_attribute(attributeNameIndex: Int,
                            maxStack: Int,
                            maxLocals: Int,
                            codeOrig: Seq[JVMByteCode.Generated],
                            code: Array[Byte],
                            attributes: Seq[attribute_info]) extends Attribute {


    def write(out: ByteArrayOutputStream, charset: Charset): Unit = {
      JVMClassFileBuilderUtils.writeShort(out, attributeNameIndex)
      JVMClassFileBuilderUtils.writeInt(out, lengthBytes())
      JVMClassFileBuilderUtils.writeShort(out, maxStack)
      JVMClassFileBuilderUtils.writeShort(out, maxLocals)
      JVMClassFileBuilderUtils.writeInt(out, code.length)
      out.write(code)
      // Exceptions not supported
      JVMClassFileBuilderUtils.writeShort(out, 0)
      JVMClassFileBuilderUtils.writeShort(out, attributes.length)
      attributes.foreach(_.write(out, charset))
    }

    override def lengthBytes(): Int = {
      2+2+4+code.length+2+0+2+0
    }

    override def write(out: PrintWriter, charset: Charset): Unit = {
      val gp = GenParams()
      codeOrig.foreach(c => out.write(c.gen(gp) + '\n'))
    }
  }

}
