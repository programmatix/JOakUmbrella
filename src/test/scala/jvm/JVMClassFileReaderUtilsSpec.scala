package jvm

import java.io.ByteArrayInputStream

import org.scalatest.FunSuite

class JVMClassFileReaderUtilsSpec extends FunSuite {
//  test("twosComplementRepresentation 3 bits") {
//    assert(JVMClassFileReaderUtils.twosComplementRepresentation(0, 3) == 0)
//    assert(JVMClassFileReaderUtils.twosComplementRepresentation(1, 3) == 7)
//    assert(JVMClassFileReaderUtils.twosComplementRepresentation(2, 3) == 6)
//    assert(JVMClassFileReaderUtils.twosComplementRepresentation(3, 3) == 5)
//    assert(JVMClassFileReaderUtils.twosComplementRepresentation(8, 3) == 0)
//    assert(JVMClassFileReaderUtils.twosComplementRepresentation(-4, 3) == 4)
//    assert(JVMClassFileReaderUtils.twosComplementRepresentation(-3, 3) == 3)
//    assert(JVMClassFileReaderUtils.twosComplementRepresentation(-1, 3) == 1)
//  }

  test("byte extends twos complement") {
    assert(JVMClassFileReaderUtils.extendByteAsTwosComplement(0xff.toByte).toBinaryString == "1111 1111 1111 1111 1111 1111 1111 1111".replace(" ", ""))
    assert(JVMClassFileReaderUtils.extendByteAsTwosComplement(0xff.toByte) == -1)

    assert(JVMClassFileReaderUtils.extendByteAsTwosComplement(0x80.toByte).toBinaryString == "1111 1111 1111 1111 1111 1111 1000 0000".replace(" ", ""))
    assert(JVMClassFileReaderUtils.extendByteAsTwosComplement(0x80.toByte) == -128)

    assert(JVMClassFileReaderUtils.extendByteAsTwosComplement(0xfe.toByte).toBinaryString == "1111 1111 1111 1111 1111 1111 1111 1110".replace(" ", ""))
    assert(JVMClassFileReaderUtils.extendByteAsTwosComplement(0xfe.toByte) == -2)

    assert(JVMClassFileReaderUtils.extendByteAsTwosComplement(0x0.toByte).toBinaryString == "0".replace(" ", ""))
    assert(JVMClassFileReaderUtils.extendByteAsTwosComplement(0x1.toByte).toBinaryString == "1".replace(" ", ""))
  }

  test("10000000") {
    val byte1 = Integer.parseInt("10000000", 2).toByte
    val data = Array(byte1)
    val in = new ByteArrayInputStream(data)
    val out = in.read()
    assert (out == 256)
  }

  test("f7") {
//    val rawInt = Integer.parse("11110111", 2)
//    assert (rawInt == -9)

    assert(Integer.parseInt("0111", 2) == 7)
    assert(Integer.parseInt("1111", 2) == -1)

    val nibble2 = Integer.parseInt("1111", 2)
      val nibble1 = Integer.parseInt("0111", 2)
    val nibble2Shifted = nibble2 << 4
    val byte = nibble2Shifted | nibble1

    assert(byte == -9)
  }

}
