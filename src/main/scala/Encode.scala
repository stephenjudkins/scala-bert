package org.bert

import java.io.{OutputStream, DataOutputStream, ByteArrayOutputStream}


class Encoder(outInner: OutputStream) {

  import Constants._

  val out = new DataOutputStream(outInner)

  def encode(value: Any): Unit = {
    out.writeByte(MAGIC)
    encodeAny(value)
    out.flush
  }

  def encodeAny(value: Any): Unit = {
    value match {
      case t:BertTuple => encodeTuple(t)
      case s:Symbol => encodeSymbol(s)
      case m:Map[Any, Any] => encodeMap(m)
      case date:java.util.Date => encodeDate(date)
      case i:Int => encodeInt(i)
      case d:Double => encodeDouble(d)
      case f:Float => encodeFloat(f)
      case b:Boolean => encodeBoolean(b)
      case b:Array[Byte] => encodeBinary(b)
      case seq:Seq[Any] => encodeSeq(seq)
/*      case r:scala.util.matching.Regex => encodeRegex(r)*/
      case null => encodeNull
      case other => println("couldn't do: " + other)
    }
  }


  import java.util.regex.Pattern

  def encodeFloat(f: Float) = {
    encodeDouble(f.toDouble)
  }

  val FLOAT_LENGTH = 31
  def encodeDouble(d: Double) = {
    val bytes = String.format("%15.15e", new java.lang.Double(d)).getBytes
    val padded = bytes ++ Stream.const(0.toByte).take(FLOAT_LENGTH - bytes.length)
    out.writeByte(FLOAT)
    out.write(padded, 0, FLOAT_LENGTH)
  }

  def encodeBinary(b: Array[Byte]) = {
    out.writeByte(BIN)
    out.writeInt(b.length)
    out.write(b, 0, b.length)
  }

  def encodeNull = {
    encodeTuple(BertTuple('bert, 'nil))
  }

  def encodeBoolean(b: Boolean) = {
    encodeTuple(BertTuple('bert, Symbol(b.toString)))
  }

  def encodeInt(i: Int) = {
    if (i >= 0 && i < 256) {
      out.writeByte(SMALL_INT)
      out.writeByte(i)
    } else {
      out.writeByte(INT)
      out.writeInt(i)
    }
  }

  def encodeDate(date: java.util.Date) = {
    val time = date.getTime

    val value = List('bert, 'time) ::: List(time / 1e9, (time % 1e9) / 1e3, (time % 1e3)).map(_.floor.toInt)
    encodeAny(BertTuple(value: _*))
  }

  def encodeSeq(seq: Seq[Any]) = {
    seq.length match {
      case 0 => { out.writeByte(NIL) }
      case _ => {
        out.writeByte(LIST)
        out.writeInt(seq.length)

        seq.foreach { encodeAny(_) }

        out.writeByte(NIL)
      }
    }
  }

  def encodeMap(m: Map[Any, Any]) = {
    val tupleMap = m.map { case (k, v) => BertTuple(k, v) }.toList
    encodeTuple(BertTuple('bert, 'dict, tupleMap))
  }

  def encodeSymbol(s: Symbol) = {
    val value = s.name

    out.writeByte(ATOM)
    out.writeShort(value.length)
    out.writeBytes(value)
  }

  def encodeTuple(tuple: BertTuple) = {
    val contents = tuple.contents.toArray
    if (contents.length < 256) {
      out.writeByte(SMALL_TUPLE)
      out.writeByte(contents.length)
    } else {
      out.writeByte(LARGE_TUPLE)
      out.writeInt(contents.length)
    }
    contents.foreach { encodeAny(_) }
  }

}