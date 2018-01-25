package compiling

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets

import compiling.JVMByteCode.GenParams

class JVMClassFileWriter {
  import scala.language.implicitConversions

  private implicit def stringToBytes(x: String): Array[Byte] = x.getBytes("UTF-8")

  def write(input: Seq[JVMByteCode.ByteCode], packageName: String, className: String): String = {
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
    })


    out.write(s"public class $packageName.$className {".getBytes(charset))
    out.write("}")
    out.toString()
  }

}
