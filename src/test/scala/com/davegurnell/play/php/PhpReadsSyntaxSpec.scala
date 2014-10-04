package com.davegurnell.play.php

import org.scalatest._
import play.api.data.validation.ValidationError
import play.api.libs.functional.syntax._

class PhpReadsSyntaxSpec extends WordSpec with Matchers {
  case class Location(lon: Double, lat: Double)

  implicit val locationReads: PhpReads[Location] = (
    (PhpPath \ "lon").read[Double] and
    (PhpPath \ "lat").read[Double]
  )(Location.apply _)

  "applicative-style reads" should {
    "parse valid data" in {
      val value = Php.arr(
        "lat" -> 51.505,
        "lon" -> -0.12
      )

      val expected = PhpSuccess(Location(-0.12, 51.505))

      Php.fromPhp[Location](value) should equal(expected)
    }

    "report errors against invalid data" in {
      val value = Php.arr(
        "lat" -> "51.505",
        "lon" -> "-0.12"
      )

      val expected = PhpError(Seq(
        (PhpPath \ "lon") -> Seq(ValidationError("error.expected.phpdouble")),
        (PhpPath \ "lat") -> Seq(ValidationError("error.expected.phpdouble"))
      ))

      Php.fromPhp[Location](value) should equal(expected)
    }
  }
}