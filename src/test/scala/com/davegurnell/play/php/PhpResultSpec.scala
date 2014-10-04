package com.davegurnell.play.php

import org.scalatest._

class PhpResultSpec extends WordSpec with Matchers {
  "repath" should {
    "prepend a path prefix to a PhpSuccess" in {
      val actual =
        PhpSuccess(PhpBoolean(true)).
          repath(PhpPath \ 1 \ 2).
          repath(PhpPath \ 3 \ 4)

      val expected = PhpSuccess(PhpBoolean(true), PhpPath \ 3 \ 4 \ 1 \ 2)

      actual should equal(expected)
    }

    "prepend a path prefix to a PhpError" in {
      val actual =
        PhpError(PhpPath, "epic.fail").
          repath(PhpPath \ 1 \ 2).
          repath(PhpPath \ 3 \ 4)

      val expected = PhpError(PhpPath \ 3 \ 4 \ 1 \ 2, "epic.fail")

      actual should equal(expected)
    }
  }

  "map" should {
    "preserve paths in PhpSuccess" in {
      val actual   = PhpSuccess(123, PhpPath \ 123).map(_.toString)
      val expected = PhpSuccess("123", PhpPath \ 123)
      actual should equal(expected)
    }

    "preserve paths in PhpError" in {
      val actual   = PhpError(PhpPath \ 123, "error.badness").map(_.toString)
      val expected = PhpError(PhpPath \ 123, "error.badness")
      actual should equal(expected)
    }
  }

  "flatMap" should {
    "preserve paths in PhpSuccess" in {
      val actual   = PhpSuccess(123, PhpPath \ 123).flatMap(_ => PhpSuccess(234, PhpPath \ 234))
      val expected = PhpSuccess(234, PhpPath \ 123 \ 234)
      actual should equal(expected)
    }

    "preserve paths in PhpError" in {
      val actual   = PhpError(PhpPath \ 123, "error.badness").flatMap(_ => PhpSuccess(234, PhpPath \ 234))
      val expected = PhpError(PhpPath \ 123, "error.badness")
      actual should equal(expected)
    }
  }
}