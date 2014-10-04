package com.davegurnell.play.php

import org.scalatest._

class PhpPathSpec extends WordSpec with Matchers {
  "__" should {
    "equal PhpPath" in {
      __ should equal(PhpPath)
    }
  }

  "path constructors" when {
    val expected = PhpPath(Seq(
      PhpChildNode(PhpString("abc")),
      PhpSearchNode(PhpInt(123)),
      PhpChildNode(PhpBoolean(true))
    ))

    "the \\ and \\\\ methods" should {
      "create a path" in {
        (PhpPath \ "abc" \\ 123 \ true) should equal(expected)
      }
    }

    "the ++ method" should {
      "create a path" in {
        ((PhpPath \ "abc") ++ (PhpPath \\ 123 \ true)) should equal(expected)
      }
    }
  }

  "searches" when {
    val inner1 = Php.arr(
      123 -> true,
      234 -> false
    )

    val inner2 = Php.arr(
      123 -> 999.0,
      345 -> 888.0
    )

    val outer1 = Php.arr(
      "abc" -> inner1,
      "cde" -> inner2
    )

    val outer2 = Php.arr(
      123 -> inner1,
      234 -> inner2
    )

    val outer3 = Php.arr(
      123 -> outer2
    )

    "single uses of \\" should {
      "find all matching values" in {
        (PhpPath \ "abc")(outer1) should equal(Seq(inner1))
        (PhpPath \ "bcd")(outer1) should equal(Seq())
        (PhpPath \ "cde")(outer1) should equal(Seq(inner2))
      }

      "find single matching values" in {
        (PhpPath \ "abc").extractOne(outer1) should equal(PhpSuccess(inner1))
        (PhpPath \ "bcd").extractOne(outer1) should equal(PhpError(PhpPath \ "bcd", "error.path.missing"))
      }
    }

    "multiple uses of \\" should {
      "find matching values" in {
        (PhpPath \ "abc" \ 123)(outer1) should equal(Seq(PhpBoolean(true)))
        (PhpPath \ "bcd" \ 123)(outer1) should equal(Seq())
        (PhpPath \ "cde" \ 123)(outer1) should equal(Seq(PhpDouble(999.0)))
      }

      "find single matching values" in {
        (PhpPath \ "abc" \ 123).extractOne(outer1) should equal(PhpSuccess(PhpBoolean(true)))
        (PhpPath \ "bcd" \ 123).extractOne(outer1) should equal(PhpError(PhpPath \ "bcd" \ 123, "error.path.missing"))
      }
    }

    "single uses of \\\\" should {
      "find matching values" in {
        (PhpPath \\ 123)(outer1) should equal(Seq(PhpBoolean(true), PhpDouble(999.0)))
        (PhpPath \\ 123)(outer2) should equal(Seq(inner1, PhpBoolean(true), PhpDouble(999.0)))
      }

      "find single matching values" in {
        (PhpPath \\ 234).extractOne(outer1) should equal(PhpSuccess(PhpBoolean(false)))
      }

      "fail to find single matching values if there are multiples" in {
        (PhpPath \\ 123).extractOne(outer1) should equal(PhpError(PhpPath \\ 123, "error.path.result.multiple"))
      }
    }

    "combinations of \\ and \\\\" should {
      "find matching values" in {
        (PhpPath \\ 123 \ 123)(outer1) should equal(Seq())
        (PhpPath \ 123 \\ 123)(outer1) should equal(Seq())
        (PhpPath \\ 123 \ 123)(outer2) should equal(Seq(PhpBoolean(true)))
        (PhpPath \ 123 \\ 123)(outer2) should equal(Seq(PhpBoolean(true)))
      }

      "find single matching values" in {
        (PhpPath \ 123 \\ 123).extractOne(outer2) should equal(PhpSuccess(PhpBoolean(true)))
      }

      "fail to find single matching values if there are multiples" in {
        (PhpPath \ 123 \\ 123).extractOne(outer3) should equal(PhpError(PhpPath \ 123 \\ 123, "error.path.result.multiple"))
      }
    }
  }
}