package com.davegurnell.play.php

import org.scalatest._
import play.api.data.validation.ValidationError

class PhpReadsSpec extends WordSpec with Matchers {
  "intReads" should {
    "read values" in {
      Php.fromPhp[Int](PhpInt(123)) should equal(PhpSuccess(123))
    }

    "fail if the type is incorrect" in {
      Php.fromPhp[Int](PhpString("abc")) should equal(PhpError(PhpPath, "error.expected.phpint"))
    }
  }

  "doubleReads" should {
    "read values" in {
      Php.fromPhp[Double](PhpDouble(123.456)) should equal(PhpSuccess(123.456))
    }

    "fail if the type is incorrect" in {
      Php.fromPhp[Double](PhpString("abc")) should equal(PhpError(PhpPath, "error.expected.phpdouble"))
    }
  }

  "booleanReads" should {
    "read values" in {
      Php.fromPhp[Boolean](PhpBoolean(true)) should equal(PhpSuccess(true))
    }

    "fail if the type is incorrect" in {
      Php.fromPhp[Boolean](PhpInt(123)) should equal(PhpError(PhpPath, "error.expected.phpboolean"))
    }
  }

  "stringReads" should {
    "read values" in {
      Php.fromPhp[String](PhpString("abc")) should equal(PhpSuccess("abc"))
    }

    "fail if the type is incorrect" in {
      Php.fromPhp[String](PhpInt(123)) should equal(PhpError(PhpPath, "error.expected.phpstring"))
    }
  }

  "optionReads" should {
    "read values" in {
      Php.fromPhp[Option[String]](PhpString("abc")) should equal(PhpSuccess(Some("abc")))
    }

    "read nulls" in {
      Php.fromPhp[Option[String]](PhpNull) should equal(PhpSuccess(None))
    }

    "fail if the type is incorrect" in {
      Php.fromPhp[Option[Int]](PhpString("abc")) should equal(PhpError(PhpPath, "error.expected.phpint"))
    }
  }

  "mapReads" should {
    "read arrays" in {
      val value = Php.arr(
        0 -> "a",
        1 -> "b",
        2 -> "c"
      )

      Php.fromPhp[Map[Int, String]](value) should equal(PhpSuccess(Map(0 -> "a", 1 -> "b", 2 -> "c")))
    }

    "read objects" in {
      val value = Php.obj("MyClass")(
        0 -> "a",
        1 -> "b",
        2 -> "c"
      )

      Php.fromPhp[Map[Int, String]](value) should equal(PhpSuccess(Map(0 -> "a", 1 -> "b", 2 -> "c")))
    }

    "fail if the key type is incorrect" in {
      Php.fromPhp[Map[Int, String]](Php.arr("a" -> "b")) should equal(PhpError(PhpPath \ "a", "error.expected.phpint"))
    }

    "fail if the value type is incorrect" in {
      Php.fromPhp[Map[String, Boolean]](Php.arr("a" -> "b")) should equal(PhpError(PhpPath \ "a", "error.expected.phpboolean"))
    }
  }

  "traversableReads" should {
    "read arrays" in {
      val value = Php.arr(
        0 -> "a",
        1 -> "b",
        2 -> "c"
      )

      Php.fromPhp[List[String]](value) should equal(PhpSuccess(List("a", "b", "c")))
      Php.fromPhp[Vector[String]](value) should equal(PhpSuccess(Vector("a", "b", "c")))
    }

    "read objects" in {
      val value = Php.obj("MyObject")(
        0 -> "a",
        1 -> "b",
        2 -> "c"
      )

      Php.fromPhp[List[String]](value) should equal(PhpSuccess(List("a", "b", "c")))
      Php.fromPhp[Vector[String]](value) should equal(PhpSuccess(Vector("a", "b", "c")))
    }

    "fail if the value type is incorrect" in {
      Php.fromPhp[List[String]](Php.arr(0 -> 1)) should equal(PhpError(PhpPath \ 0, "error.expected.phpstring"))
    }
  }

  "nested reads" should {
    "parse nested data" in {
      val value = Php.arr(
        0 -> Php.arr("a" -> Php.arr(true -> 123)),
        1 -> Php.arr("b" -> Php.arr(false -> 321))
      )

      val expected = PhpSuccess(Map(
        0 -> Map("a" -> Map(true  -> 123)),
        1 -> Map("b" -> Map(false -> 321))
      ))

      Php.fromPhp[Map[Int, Map[String, Map[Boolean, Int]]]](value) should equal(expected)
    }

    "preserve paths in read errors" in {
      val value = Php.arr(
        0 -> Php.arr("a" -> Php.arr(true -> "123")),
        1 -> Php.arr(123 -> 321)
      )

      val expected = PhpError(Seq(
        (PhpPath \ 0 \ "a" \ true) -> Seq(ValidationError("error.expected.phpint")),
        (PhpPath \ 1 \ 123)        -> Seq(ValidationError("error.expected.phpstring"))
      ))

      Php.fromPhp[Map[Int, Map[String, Map[Boolean, Int]]]](value) should equal(expected)
    }
  }

  ""
}