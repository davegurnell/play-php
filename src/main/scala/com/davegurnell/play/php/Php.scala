package com.davegurnell.play.php

object Php {
  /**
   * Creates a `PhpArray`.
   *
   * Arguments can be pairs of `PhpValues`, pairs of values that can be converted to `PhpValues`,
   * or a mixture of the two:
   *
   * {{{
   * Php.arr("field" -> "value")
   * // == PhpArray(Seq(PhpString("field") -> PhpValue("value")))
   * }}}
   */
  def arr(fields: (PhpValueWrapper, PhpValueWrapper) *) =
    PhpArray(fields.toList.map(pair => pair._1.value -> pair._2.value))

  /**
   * Creates a `PhpObject`.
   *
   * Arguments can be pairs of `PhpValues`, pairs of values that can be converted to `PhpValues`,
   * or a mixture of the two.
   *
   * {{{
   * Php.obj("MyClass")("field" -> "value")
   * // == PhpObject("MyClass", Seq(PhpString("field") -> PhpValue("value")))
   * }}}
   */
  def obj(className: String)(fields: (PhpValueWrapper, PhpValueWrapper) *) =
    PhpObject(className, fields.toList.map(pair => pair._1.value -> pair._2.value))

  /**
   * Converts a value of type `A` to a `PhpValue`.
   */
  def toPhp[A](value: A)(implicit writes: PhpWrites[A]): PhpValue =
    writes.writes(value)

  /**
   * Converts a `PhpValue` to a value of type `A`. The result is wrapped in
   * `PhpResult` to indicate that the conversion may succeed or fail.
   */
  def fromPhp[A](value: PhpValue)(implicit reads: PhpReads[A]): PhpResult[A] =
    reads.reads(value)

  /**
   * Convenience function for parse a `PhpValue` from an array of bytes.
   *
   * For more general parsing see `PhpParser.apply(inputStream)`.
   */
  def parse(input: Array[Byte]): PhpValue =
    PhpParser(input)

  /**
   * Convenience function for parse a `PhpValue` from a string.
   *
   * For more general parsing see `PhpParser.apply(inputStream)`.
   */
  def parse(input: String): PhpValue =
    PhpParser(input)

  /**
   * Convenience function for converting a `PhpValue` to an array of bytes.
   *
   * For more general serialization see `PhpSerializer.apply(value, outputStream)`.
   */
  def serialize(value: PhpValue): Array[Byte] =
    PhpSerializer(value)
}
