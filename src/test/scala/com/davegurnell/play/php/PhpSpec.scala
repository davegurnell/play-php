package com.davegurnell.play.php

import org.scalatest._

class PhpSpec extends WordSpec with Matchers {
  "Php.arr" should {
    "interpret simple arguments as PhpValues" in {
      val actual = Php.arr(
        "abc" -> 123,
        true -> 234.0
      )

      val expected = PhpArray(Seq(
        PhpString("abc") -> PhpInt(123),
        PhpBoolean(true) -> PhpDouble(234.0)
      ))

      actual should equal(expected)
    }
  }

  "Php.obj" should {
    "interpret simple arguments as PhpValues" in {
      val actual = Php.obj("MyClass")(
        "abc" -> 123,
        true -> 234.0
      )

      val expected = PhpObject("MyClass", Seq(
        PhpString("abc") -> PhpInt(123),
        PhpBoolean(true) -> PhpDouble(234.0)
      ))

      actual should equal(expected)
    }
  }

  // For more tests see PhpWritesSpec
  "Php.toPhp" should {
    "serialize a value" in {
      Php.toPhp("abc") should equal(PhpString("abc"))
    }
  }

  // For more tests see PhpReadsSpec
  "Php.fromPhp" should {
    "read a valid value" in {
      Php.fromPhp[String](PhpString("abc")) should equal(PhpSuccess("abc"))
    }

    "read an invalid value" in {
      Php.fromPhp[Int](PhpString("abc")) should equal(PhpError(PhpPath, "error.expected.phpint"))
    }
  }
}