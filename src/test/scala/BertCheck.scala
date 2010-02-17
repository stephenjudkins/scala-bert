package org.bert.check

import org.scalacheck._

import org.bert._

object StringSerializationCheck extends Properties("String") {
  property("encodes and decodes") = Prop.forAll {(a: String) =>
    var encoding = "UTF-8"

    Bert.decode(Bert.encode(a.getBytes(encoding))) match {
      case b:Array[Byte] => new String(b, encoding) == a
      case _ => false
    }
  }
}

object IntegerSerializationCheck extends Properties("Int") {
  property("encodes and decodes") = Prop.forAll {(i: Int) =>
    Bert.decode(Bert.encode(i)) == i
  }
}
