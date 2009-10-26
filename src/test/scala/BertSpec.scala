package org.bert

import org.specs._

import org.bert._

object BertSpec extends Specification {
  val bert = Array(-125, 104, 9, 100, 0, 4, 117, 115, 101, 114, 104, 3, 100, 0, 4, 98, 101, 114, 116, 100, 0, 4, 100, 105, 99, 116, 108, 0, 0, 0, 1, 104, 2, 100, 0, 4, 110, 97, 109, 101, 109, 0, 0, 0, 3, 84, 80, 87, 106, 108, 0, 0, 0, 1, 99, 57, 46, 57, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 101, 43, 48, 48, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 106, 104, 5, 100, 0, 4, 98, 101, 114, 116, 100, 0, 4, 116, 105, 109, 101, 98, 0, 0, 4, -26, 98, 0, 14, -28, -61, 97, 0, 104, 2, 100, 0, 4, 98, 101, 114, 116, 100, 0, 3, 110, 105, 108, 104, 2, 100, 0, 4, 98, 101, 114, 116, 100, 0, 4, 116, 114, 117, 101, 104, 2, 100, 0, 4, 98, 101, 114, 116, 100, 0, 5, 102, 97, 108, 115, 101, 100, 0, 4, 116, 114, 117, 101, 100, 0, 5, 102, 97, 108, 115, 101).map(_.toByte)

  val date = new java.util.Date(1254976067000L)

  val scala = BertTuple('user, Map('name -> "TPW".getBytes), List(9.9), date, null, true, false, 'true, 'false )

  // TODO: make these not ugly

  "with serialized data" should {
    "encodes strings" in {
      Bert.encode("hello".getBytes).toList must_== Array(-125, 109, 0, 0, 0, 5, 104, 101, 108, 108, 111).map(_.toByte).toList
    }

    "encodes tuples" in {
      Bert.encode(BertTuple('foo, 'bar)).toList must_== List(-125, 104, 2, 100, 0, 3, 102, 111, 111, 100, 0, 3, 98, 97, 114).map(_.toByte)
    }

    "encodes floats" in {
      Bert.encode(5.5).toList must_== Array(-125, 99, 53, 46, 53, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 101, 43, 48, 48, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0).map(_.toByte).toList
    }

    "encodes dates" in {
      Bert.encode(new java.util.Date(1234567890000L)).toList must_== Array(-125, 104, 5, 100, 0, 4, 98, 101, 114, 116, 100, 0, 4, 116, 105, 109, 101, 98, 0, 0, 4, -46, 98, 0, 8, -86, 82, 97, 0).map(_.toByte).toList
    }

    "encodes" in {
      Bert.encode(scala).toList must_== bert.toList
    }
  }

  "with Scala objects" should {
    "decodes" in {
      // Bert.decode(bert) must_== scala
    }
  }

}