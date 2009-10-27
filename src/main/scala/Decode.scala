package org.bert

import java.io.{OutputStream, DataOutputStream, ByteArrayOutputStream, ByteArrayInputStream, InputStream, DataInputStream}

class Decoder(innerInput: InputStream) {
  import Constants._
  val input = new DataInputStream(innerInput)

  def decode:Any = {
    input.readByte match {
      case MAGIC =>
    }

    readAny
  }

  def readAny: Any = {
    input.readByte match {
      case BIN => readBin
      case SMALL_TUPLE => readSmallTuple
      case NIL => List()
      case LIST => readList
      case SMALL_INT => readSmallInt
      case unknown => throw new Exception("unknown type: " + unknown)
    }
  }

  def readSmallInt = {
    input.readByte.toInt
  }

  def readList = {
    val length = readInt
    Stream.const {() => readAny}.map {_()}.take(length).toList
  }

  def readBin:Array[Byte] = {
    val length = readInt
    readBytes(length)
  }

  def readSmallTuple:BertTuple = {
    val length = readInt
    val values = Stream.const {() => readAny}.map {_()}.take(length).toArray
    BertTuple(values: _*)
  }

  def readInt = input.readInt

  def readBytes(length: Int):Array[Byte] = {
    val array = new Array[Byte](length)
    input.readFully(array, 0, length)

    array
  }
}