package org.bert

import java.io.{OutputStream, DataOutputStream, ByteArrayOutputStream, ByteArrayInputStream, InputStream, DataInputStream}

class BertDecodingException(cause: String) extends Exception(cause)

class Decoder(innerInput: InputStream) {
  import Constants._
  val input = new DataInputStream(innerInput)

  def decode:Any = {
    input.readByte match {
      case MAGIC =>
      case _ => throw new BertDecodingException("invalid format")
    }

    extractAny
  }

  def extractAny: Any = {
    readAny match {
      case BertTuple('bert, bertValue @ _*) => extractBert(bertValue)
      case other => other
    }
  }

  def extractBert(vals: Seq[Any]):Any = {
    val value = vals.slice(1, vals.length)
    vals(0) match {
      case 'dict => extractDict(value(0) match { case l:Seq[BertTuple] => l})
      case 'time => extractTime(value match { case l:Seq[Int] => l})
      case 'nil => null
      case 'true => true
      case 'false => false
    }
  }

  def extractTime(vals: Seq[Int]): java.util.Date = {
    val l = vals.map(_.toLong)
    val stamp = (l(0) * 1e9) + (l(1) * 1e3) + l(2)
    new java.util.Date(stamp.toLong)
  }

  def extractDict(vals: Seq[BertTuple]): Map[Any, Any] = {
    vals.foldLeft(Map[Any, Any]()) { case (m, BertTuple(k, v)) => {
      m + ((k, v))
    }}
  }

  def readAny: Any = {
    input.readByte match {
      case BIN => readBin
      case SMALL_TUPLE => readSmallTuple
      case NIL => List()
      case LIST => readList
      case SMALL_INT => readSmallInt
      case INT => readInt
      case ATOM => readAtom
      case FLOAT => readDouble
      case unknown => throw new BertDecodingException("unknown type: " + unknown)
    }
  }

  def readDouble = {
    new java.lang.Double(new String(readBytes(FLOAT_LENGTH)))
  }

  def readSmallInt = {
    input.readUnsignedByte.toInt
  }

  def readAtom = {
    val length = input.readShort
    Symbol(new String(readBytes(length)))
  }

  def readList = {
    val length = readInt
    val list = Stream.continually {() => extractAny}.map {_()}.take(length).toList
    input.readByte match { case NIL => }
    list
  }

  def readBin:Array[Byte] = {
    val length = readInt
    readBytes(length)
  }

  def readSmallTuple:BertTuple = {
    val length = input.readByte.toInt
    val values = Stream.continually {() => extractAny}.map {_()}.take(length).toArray
    BertTuple(values: _*)
  }

  def readInt = input.readInt

  def readBytes(length: Int):Array[Byte] = {
    val array = new Array[Byte](length)
    input.readFully(array, 0, length)

    array
  }

}