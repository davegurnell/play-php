package com.davegurnell.play.php

import org.scalatest._

class PhpValueSpec extends WordSpec with Matchers {
  "PhpString" should {
    "be comparable using value equality" in {
      PhpString("abc") should equal(PhpString("abc"))
      Set(PhpString("abc")) should contain(PhpString("abc"))
    }
  }
}