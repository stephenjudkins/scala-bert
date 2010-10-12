package org.bert.spec

import org.specs._
import org.specs.matcher.Matcher

import org.bert._

class BertSpec extends Specification {

  def binary(bytes: Int*) = bytes.map(_.toByte).toArray

  def beBinary(expected: Int*):Matcher[Array[Byte]] = beBinary(expected.map(_.toByte).toArray)

  def beBinary(expected: Array[Byte]) = new Matcher[Array[Byte]] {
    def apply(actual: => Array[Byte]) = {
      (expected.toList == actual.toList, "binary strings match", "binary strings differ: actual is " + actual.map(_.toInt).toList)
    }
  }
  val bert = binary(-125, 104, 9, 100, 0, 4, 117, 115, 101, 114, 104, 3, 100, 0, 4, 98, 101, 114, 116, 100, 0, 4, 100, 105, 99, 116, 108, 0, 0, 0, 1, 104, 2, 100, 0, 4, 110, 97, 109, 101, 100, 0, 6, 115, 121, 109, 98, 111, 108, 106, 108, 0, 0, 0, 1, 99, 57, 46, 57, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 101, 43, 48, 48, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 106, 104, 5, 100, 0, 4, 98, 101, 114, 116, 100, 0, 4, 116, 105, 109, 101, 98, 0, 0, 4, -26, 98, 0, 14, -28, -61, 97, 0, 104, 2, 100, 0, 4, 98, 101, 114, 116, 100, 0, 3, 110, 105, 108, 104, 2, 100, 0, 4, 98, 101, 114, 116, 100, 0, 4, 116, 114, 117, 101, 104, 2, 100, 0, 4, 98, 101, 114, 116, 100, 0, 5, 102, 97, 108, 115, 101, 100, 0, 4, 116, 114, 117, 101, 100, 0, 5, 102, 97, 108, 115, 101)

  val date = new java.util.Date(1254976067000L)

  val scala = BertTuple('user, Map('name -> 'symbol), List(9.9), date, null, true, false, 'true, 'false )

  "with serialized data" should {
    "encodes strings" in {
      Bert.encode("hello".getBytes) must beBinary(-125, 109, 0, 0, 0, 5, 104, 101, 108, 108, 111)
    }

    "encodes tuples" in {
      Bert.encode(BertTuple('foo, 'bar)) must beBinary(-125, 104, 2, 100, 0, 3, 102, 111, 111, 100, 0, 3, 98, 97, 114)
    }

    "encodes floats" in {
      Bert.encode(5.5) must beBinary(-125, 99, 53, 46, 53, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 101, 43, 48, 48, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    }

    "encodes dates" in {
      Bert.encode(new java.util.Date(1234567890000L)) must beBinary(-125, 104, 5, 100, 0, 4, 98, 101, 114, 116, 100, 0, 4, 116, 105, 109, 101, 98, 0, 0, 4, -46, 98, 0, 8, -86, 82, 97, 0)
    }

    "encodes regexen" in {
      Bert.encode("""(foo|spam)|(bar|eggs)""".r) must beBinary(-125, 104, 4, 100, 0, 4, 98, 101, 114, 116, 100, 0, 5, 114, 101, 103, 101, 120, 109, 0, 0, 0, 21, 40, 102, 111, 111, 124, 115, 112, 97, 109, 41, 124, 40, 98, 97, 114, 124, 101, 103, 103, 115, 41, 106)
    }

    "encodes integers < 128" in {
      Bert.encode(5) must beBinary(-125, 97, 5)
    }

    "encode integers >= 128" in {
      Bert.encode(128) must beBinary(-125, 97, 128)
    }

    "encodes" in {
      Bert.encode(scala) must beBinary(bert)
    }
  }

  "with Scala objects" should {
    "decodes a complex tuple" in {
      Bert.decode(bert) must_== scala
    }

    "decodes tuples" in {
      Bert.decode(binary(-125, 104, 2, 100, 0, 3, 102, 111, 111, 100, 0, 3, 98, 97, 114)) match {
        case b:BertTuple => b must_== BertTuple('foo, 'bar)
      }
    }

    "decodes strings" in {
      val bert = binary(-125, 109, 0, 0, 0, 5, 104, 101, 108, 108, 111)
      Bert.decode(bert) match {
        case a:Array[Byte] => new String(a) must_== "hello"
      }
    }

    "decodes floats" in {
      val bert = binary(-125, 99, 53, 46, 53, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 101, 43, 48, 48, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
      Bert.decode(bert) match {
        case f:Double => f must_== 5.5
      }
    }

    "decodes small ints" in {
      Bert.decode(binary(-125, 97, 42)) match {
        case i: Int => i must_== 42
      }
    }

    "decodes small ints >= 128" in {
      Bert.decode(binary(-125, 97, 128)) match {
        case i: Int => i must_== 128
      }
    }

    "decodes large ints" in {
      Bert.decode(binary(-125, 98, 0, 0, 16, 104)) match {
        case i: Int => i must_== 4200
      }
    }

    "decodes lists" in {
      Bert.decode(binary(-125, 108, 0, 0, 0, 3, 100, 0, 1, 97, 97, 1, 97, 5, 106)) match {
        case s:Seq[Any] => s.toList must_== List('a, 1, 5)
      }
    }
  }

  "decoding bytes and extracting Bert data types" should {
    "decode and extract maps" in {
      val bert = binary(-125, 104, 3, 100, 0, 4, 98, 101, 114, 116, 100, 0, 4, 100, 105, 99, 116, 108, 0, 0, 0, 1, 104, 2, 100, 0, 3, 102, 111, 111, 97, 1, 106)
      Bert.decode(bert) match {
        case m:Map[Any, Any] => {
          m must_== Map('foo -> 1)
        }
      }
    }

    "decode and extract time" in {
      val bert = binary(-125, 104, 5, 100, 0, 4, 98, 101, 114, 116, 100, 0, 4, 116, 105, 109, 101, 98, 0, 0, 4, -26, 98, 0, 14, -28, -61, 97, 0)
      Bert.decode(bert) match {
        case d:java.util.Date => d must_== date
      }
    }

    "decode and extract nil" in {
      Bert.decode(binary(-125, 104, 2, 100, 0, 4, 98, 101, 114, 116, 100, 0, 3, 110, 105, 108)) must beNull
    }

    "decode and extract true" in {
      Bert.decode(binary(-125, 104, 2, 100, 0, 4, 98, 101, 114, 116, 100, 0, 4, 116, 114, 117, 101)) match {
        case b:Boolean => b must beTrue
      }
    }

    "decode and extract false" in {
      Bert.decode(binary(-125, 104, 2, 100, 0, 4, 98, 101, 114, 116, 100, 0, 5, 102, 97, 108, 115, 101)) match {
        case b:Boolean => b must beFalse
      }
    }

    "decode a list followed by another value" in {
      Bert.decode(binary(-125, 104, 2, 108, 0, 0, 0, 1, 97, 1, 106, 100, 0, 3, 102, 111, 111)) match {
        case t:BertTuple => t must_== BertTuple(List(1), 'foo)
      }
    }

    "decode an atom" in {
      Bert.decode(binary(-125, 100, 0, 3, 102, 111, 111)) match {
        case s:Symbol => s must_== 'foo
      }
    }
  }

  "encoding and decoding" should {
    "work with floats" in {
      Bert.decode(Bert.encode(9.9)) must_== 9.9
    }

    "work with integers" in {
      Bert.decode(Bert.encode(128)) must_== 128
    }

    "work with binary" in {
      val binary = "?".getBytes("UTF-8")
      Bert.decode(Bert.encode(binary)) match {
        case actual:Array[Byte] => actual must beBinary(binary)
      }
    }

  }

}