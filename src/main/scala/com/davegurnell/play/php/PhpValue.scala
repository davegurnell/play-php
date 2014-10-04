package com.davegurnell.play.php

import java.nio.charset.Charset

/**
 * Type-safe representation of a serialized PHP value.
 */
sealed trait PhpValue {
  /**
   * Retrieve a child keyed with `field`, or `PhpUndefined` if no such child is found.
   *
   *     val arr = PhpArray(Map(
   *       PhpString("a") -> PhpString("b")
   *     ))
   *
   *     (arr \ "a")       // == PhpString("b")
   *     (arr \ "missing") // == PhpUndefined
   */
  def \(field: PhpValue): PhpValue = PhpUndefined

  /**
   * Recursively search for descendents a keyed with `field`:
   *
   *     val arr = PhpArray(Map(
   *       PhpString("a") -> PhpArray(Map(
   *         PhpString("b") -> PhpString("c")
   *       ))
   *     ))
   *
   *     (arr \\ "a")      // == Seq(PhpOArray(Map(PhpString("b") -> PhpString("c"))))
   *     (arr \\ "b")      // == Seq(PhpString("c"))
   *     (arr \ "missing") // == Seq()
   */
  def \\(field: PhpValue): Seq[PhpValue] = Seq.empty[PhpValue]
}

final case class PhpInt(value: Int) extends PhpValue
final case class PhpDouble(value: Double) extends PhpValue
final case class PhpBoolean(value: Boolean) extends PhpValue
final case object PhpNull extends PhpValue

/**
 * Result of traversal operations on `PhpValues` that do yield missing values:
 *
 *     (myPhpValue \ "does" \ "not" \ "exist") == PhpUndefined
 */
final case object PhpUndefined extends PhpValue

/**
 * PHP object or associative array.
 *
 * Fields can have any `PhpValue` as a key or value.
 */
sealed trait PhpArrayLike extends PhpValue {
  def fields: Seq[(PhpValue, PhpValue)]

  override def \(field: PhpValue): PhpValue =
    fields.find {
      case (f, v) if f == field => true
      case _                    => false
    }.map(_._2).getOrElse(PhpUndefined)

  override def \\(field: PhpValue): Seq[PhpValue] =
    fields.foldLeft(Seq.empty[PhpValue]) { (accum, pair) =>
      pair match {
        case (f, v) if f == field => accum ++ (v +: (v \\ field))
        case (f, v)               => accum ++ (v \\ field)
      }
    }
}

/**
 * PHP associative array.
 */
final case class PhpArray(fields: Seq[(PhpValue, PhpValue)]) extends PhpArrayLike

/**
 * PHP associative array. We can use any PHP value as a key or a value.
 */
final case class PhpObject(className: String, fields: Seq[(PhpValue, PhpValue)]) extends PhpArrayLike {
  // TODO: Model access modifiers (public, private, etc)
}

/**
 * PHP string.
 *
 * Stored as a byte array to be character-encoding-agnostic.
 */
final case class PhpString(value: Array[Byte]) extends PhpValue {
  /** Interpret the value as a `String` using the platform default charset. */
  def stringValue: String = new String(value)

  /** Interpret the value as a `String` using the supplied charset. */
  def stringValue(charset: Charset) = new String(value, charset)

  // The `==` method compares byte arrays based on reference equality,
  // so we have to override equals() and hashCode().
  //
  // See Programming in Scala Sction 28.4 for a discussion.

  def canEqual(that: Any): Boolean =
    that.isInstanceOf[PhpString]

  override def equals(that: Any): Boolean = that match {
    case that: PhpString =>
      (that canEqual this) &&
      (that.value.length == this.value.length) &&
      (that.value sameElements this.value)
    case _ => false
  }

  override def hashCode: Int =
    41 * (41 + value.hashCode)

  override def toString: String = {
    try {
      s"PhpString(${stringValue})"
    } catch {
      case exn: Exception =>
        s"PhpString(<${value.length} bytes>)"
    }
  }
}

object PhpString {
  def apply(value: String): PhpString =
    PhpString(value.getBytes)

  def apply(value: String, charset: Charset): PhpString =
    PhpString(value.getBytes(charset))
}
