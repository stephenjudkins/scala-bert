package org.bert.check

import org.scalacheck._
import Arbitrary.arbitrary
import org.bert._

object ByteArraySerializationCheck extends Properties("Array[Byte]") {
  property("encodes and decodes") = Prop.forAll {(bytes: Array[Byte]) =>

    Bert.decode(Bert.encode(bytes)) match {
      case actual:Array[Byte] => bytes.toList == actual.toList
    }
  }
}

object StringSerializationCheck extends Properties("String") {
  val encoding = "UTF-8"
  val utf8ValidStrings = arbitrary[String] suchThat { (s) => new String(s.getBytes(encoding), encoding) == s}

  property("encodes and decodes") = Prop.forAll(utf8ValidStrings) {(string) =>
    Bert.decode(Bert.encode(string.getBytes(encoding))) match {
      case bytes:Array[Byte] => new String(bytes, encoding) == string
    }
  }
}

object IntegerSerializationCheck extends Properties("Int") {
  property("encodes and decodes") = Prop.forAll {(i: Int) =>
    Bert.decode(Bert.encode(i)) == i
  }
}
