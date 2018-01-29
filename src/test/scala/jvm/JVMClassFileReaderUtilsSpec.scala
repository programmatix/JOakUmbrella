package jvm

import org.scalatest.FunSuite

class JVMClassFileReaderUtilsSpec extends FunSuite {

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


}
