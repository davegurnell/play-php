Play PHP
========

Copyright 2014 Dave Gurnell. Released under the MIT license.

A Play-JSON-like API for reading and writing serialized PHP data.
Compatible with Play 2.3.4 and Scala 2.11.2.

Originally based on the [serialized-php-parser] Java project.

[serialized-php-parser]: https://code.google.com/p/serialized-php-parser

About
-----

Key features:

 - a `PhpValue` data type with subtypes for the various data types in serialized PHP
   (int, double, boolean, string, array, object, and null);

 - `Php.arr` and `Php.obj` methods for building arrays and objects from mixtures of
   `PhpValues` and Scala values;

 - `Php.parse` and `Php.serialize` methods for converting between `PhpValues` and raw
   serialized data in the form of `Array[Byte]` values;

 - `PhpReads`, `PhpWrites`, and `PhpFormats` type classes and the corresponding methods
   `Php.toPhp` and `Php.fromPhp`;

 - applicative-style syntax for defining `PhpReads` (`PhpWrites` and `PhpFormats` yet to come).

Points of note:

 - PHP's `serialize()` function doesn't care about character encodings, so `PhpString` data
   is stored as `Array[Byte]`. The user should manually specify a character encoding to
   convert between `PhpString` and `String` (although UTF8 is used as a default).

Synopsis
--------

Here is a synopsis:

~~~ scala
import com.davegurnell.play.php._

// Parsing: Array[Byte] => PhpValue
// --------------------------------

val php: PhpValue = Php.parse(
  """
  a:2:{s:3:"lon";d:-0.12;s:3:"lat";d:51.505;}
  """.trim.getBytes
)
// => PhpArray(Vector(
//   PhpString("lon") -> PhpDouble(-0.12),
//   PhpString("lat") -> PhpDouble(51.505)
// ))

// Serialization: PhpValue => Array[Byte]
// --------------------------------------

val serialized: Array[Byte] = Php.serialize(php)

val serializedAsString = new String(serialized)
// => """a:2:{s:3:"lon";d:-0.12;s:3:"lat";d:51.505;}"""

// PhpValue DSL
// ------------

// Arrays and objects can have any data type as field names:
val randomData: PhpArray = Php.arr(
  0 -> "abc",
  1 -> true,
  2 -> Php.obj("MyClass")(
    "field1" -> 123,
    "field2" -> PhpNull
  )
)

// Reading: PhpValue => Scala data
// -------------------------------

val readData: PhpResult[Map[String, Double]] =
  Php.fromPhp[Map[String, Double]](php)
// => PhpSuccess(Map("lon" -> -0.12, "lat" -> 51.505))

readData match {
  case PhpSuccess(data, _) => println(data)
  case PhpError(errors)    => println("FAIL")
}

// Writing: Scala data => PhpValue
// -------------------------------

val roundtripPhp = Php.toPhp(php)
// => PhpArray(Vector(
//   PhpString("lon") -> PhpDouble(-0.12),
//   PhpString("lat") -> PhpDouble(51.505)
// ))

// Custom PhpReads: PhpCalue => custom Scala data
// ----------------------------------------------

import play.api.libs.functional.syntax._

case class Location(lon: Double, lat: Double)

implicit val locationReads: PhpReads[Location] = (
  (PhpPath \ "lon").read[Double] and
  (PhpPath \ "lat").read[Double]
)(Location.apply _)

val readLocation = Php.fromPhp[Location](php)
// => PhpSuccess(Location(-0.12, 51.505))

val location = readLocation getOrElse Location(0, 0)
// => Location(-0.12, 51.505)

// Custom PhpWrites: custom Scala data => PhpValue
// -----------------------------------------------

// We don't currently support applicative-style syntax for writes:
implicit val locationWrites = PhpWrites[Location] { loc =>
  Php.arr(
    "lon" -> loc.lon,
    "lat" -> loc.lat
  )
}

val roundtripLocation = Php.toPhp[Location](location)
// => PhpArray(Vector(
//   PhpString("lon") -> PhpDouble(-0.12),
//   PhpString("lat") -> PhpDouble(51.505)
// ))
~~~