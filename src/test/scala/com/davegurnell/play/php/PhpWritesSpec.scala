package com.davegurnell.play.php

import org.scalatest._
import scala.collection.immutable.Vector

class PhpWritesSpec extends WordSpec with Matchers {
  "intWrites" should {
    "write values" in {
      Php.toPhp(123) should equal(PhpInt(123))
    }
  }

  "doubleWrites" should {
    "write values" in {
      Php.toPhp(123.456) should equal(PhpDouble(123.456))
    }
  }

  "booleanWrites" should {
    "write values" in {
      Php.toPhp(true) should equal(PhpBoolean(true))
    }
  }

  "stringWrites" should {
    "write values" in {
      Php.toPhp("abc") should equal(PhpString("abc"))
    }
  }

  "optionWrites" should {
    "write values" in {
      Php.toPhp(Some("abc")) should equal(PhpString("abc"))
    }

    "write nulls" in {
      Php.toPhp(Option.empty[String]) should equal(PhpNull)
    }
  }

  "mapWrites" should {
    "write maps" in {
      Php.toPhp(Map("a" -> true, "b" -> false)) should equal(Php.arr(
        "a" -> true,
        "b" -> false
      ))
    }
  }

  "traversableWrites" should {
    "write seqs" in {
      Php.toPhp(Seq("a", "b", "c")) should equal(Php.arr(
        0 -> "a",
        1 -> "b",
        2 -> "c"
      ))
    }

    "write vectors" in {
      Php.toPhp(Vector("a", "b", "c")) should equal(Php.arr(
        0 -> "a",
        1 -> "b",
        2 -> "c"
      ))
    }
  }
}