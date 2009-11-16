package org.bert

import scala.util.parsing.combinator._
import scala.util.parsing.input._

import java.io.{OutputStream, DataOutputStream, ByteArrayOutputStream, ByteArrayInputStream, InputStream, DataInputStream}

object BertParsers extends Parsers {
  import Constants._
  override type Elem = Byte

  // always returns a byte
  def byte:Parser[Byte] = Parser { in => Success(in.first, in.rest) }

  // primitive types used by other, user-facing types
  def int:Parser[Int] = repN(4, byte) ^^ {(bytes) => new java.math.BigInteger(bytes.toArray).intValue }
  def short:Parser[Int] = byte ~ byte ^^ {case a ~ b => (a << 8) | (b & 0xff)}

  // parsers for BERT data types
  def smallInt:Parser[Int] = SMALL_INT ~> byte ^^ (_.toInt)
  def bigInt:Parser[Int] = INT ~> int

  def tuple:Parser[BertTuple] = prefixedList(SMALL_TUPLE, byte ^^ (_.toInt), any) ^^ {(args) => BertTuple(args:_*) }

  def nil:Parser[List[Nothing]] = NIL ^^ {(x) => Nil }

  def list:Parser[List[Any]] = prefixedList(LIST, any) <~ NIL

  def atom:Parser[Symbol] = prefixedList(ATOM, short, byte) ^^ {(bytes) => Symbol(new String(bytes.toArray))}

  def binary:Parser[Array[Byte]] = prefixedList(BIN, byte) ^^ (_.toArray)

  def float:Parser[Double] = FLOAT ~> repN(FLOAT_LENGTH, byte) ^^ {(bytes) =>
    new java.lang.Double(new String(bytes.toArray)).doubleValue
  }

  def prefixedList[T](flagParser: Parser[Any], parser: Parser[T]):Parser[List[T]] =
    prefixedList(flagParser, int, parser)

  def prefixedList[T](flagParser: Parser[Any], lengthParser: Parser[Int], elementParser: Parser[T]):Parser[List[T]] =
    (flagParser ~> lengthParser) >> {(length) => repN(length, elementParser)}

  def any:Parser[Any] = atom | nil | list | smallInt | bigInt | binary | tuple | float
  def complete:Parser[Any] = MAGIC ~> any

  def parse(reader: Reader[Byte]):Any = {
    complete(reader) match {
      case Success(result, next) => result
      case Failure(msg, next) => throw new RuntimeException(msg)
      case Error(msg, next) => throw new RuntimeException(msg)
    }
  }
}


object BertParser {
  class ByteArrayReader(bytes: Stream[Byte]) extends Reader[Byte] {
    def first = bytes.head
    def rest = new ByteArrayReader(bytes.tail)
    def pos = NoPosition
    def atEnd = bytes.isEmpty
  }

  def parse(input: InputStream) = {
    val byteStream = Stream.const { input.read _ }.map(_()).takeWhile( _ != -1).map(_.toByte)
    extractAny(BertParsers.parse(new ByteArrayReader(byteStream)))
  }

  def extractAny(v: Any): Any = {
    v match {
      case BertTuple('bert, bertValue @ _*) => extractBert(bertValue)
      case l:List[Any] => l.map(extractAny(_))
      case BertTuple(vals @ _*) => BertTuple(vals.map(extractAny(_)): _*)
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

}


