package org.bert

import java.io.{OutputStream, DataOutputStream, ByteArrayOutputStream, ByteArrayInputStream, InputStream, DataInputStream}

import scala.io.Source

case class BertTuple(val contents: Any*) {
}

object Bert {
  def encode(value: Any):Array[Byte] = {
    val out = new ByteArrayOutputStream
    new Encoder(out).encode(value)

    out.flush
    out.toByteArray
  }

  def decode(input: InputStream):Any = {
    new Decoder(input).decode
  }

  def decode(input: Array[Byte]):Any = {
    decode(new ByteArrayInputStream(input))
  }

}

object Constants {
  val SMALL_INT = 97.toByte
  val INT = 98.toByte
  val SMALL_BIGNUM = 110.toByte
  val LARGE_BIGNUM = 111.toByte
  val FLOAT = 99.toByte
  val ATOM = 100.toByte
  val SMALL_TUPLE = 104.toByte
  val LARGE_TUPLE = 105.toByte
  val NIL = 106.toByte
  val STRING = 107.toByte
  val LIST = 108.toByte
  val BIN = 109.toByte
  val FUN = 117.toByte
  val NEW_FUN = 112.toByte
  val MAGIC = 131.toByte
  val MAX_INT = (1 << 27) -1.toByte
  val MIN_INT = -(1 << 27).toByte
}
