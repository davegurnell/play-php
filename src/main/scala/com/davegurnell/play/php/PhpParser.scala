package com.davegurnell.play.php

/*
 * Adapted from the `serialized-php-parser` Java library:
 *
 *     https://code.google.com/p/serialized-php-parser/
 *     Copyright (c) 2007 Zsolt Szász <zsolt at lorecraft dot com>
 *     MIT License: http://opensource.org/licenses/mit-license.php
 */

import java.io._
import scala.collection.immutable.Queue

object PhpParser {
  val IntStart     = 'i'.toByte
  val DoubleStart  = 'd'.toByte
  val BooleanStart = 'b'.toByte
  val StringStart  = 's'.toByte
  val ArrayStart   = 'a'.toByte
  val ObjectStart  = 'O'.toByte
  val NullStart    = 'N'.toByte

  val ArgDelim     = ':'.toByte
  val StringDelim  = '"'.toByte
  val ValueEnd     = ';'.toByte
  val FieldsStart  = '{'.toByte
  val FieldsEnd    = '}'.toByte

  val TrueValue    = '1'.toByte
  val FalseValue   = '0'.toByte

  def apply(input: String): PhpValue =
    new PhpParser(new ByteArrayInputStream(input.getBytes)).parse()

  def apply(input: Array[Byte]): PhpValue =
    new PhpParser(new ByteArrayInputStream(input)).parse()

  def apply(input: InputStream): PhpValue =
    new PhpParser(input).parse()
}

class PhpParser(input: InputStream) {
  import PhpParser._

  var position = 0L

  def error(message: String) =
    throw new PhpParserException(s"Position ${position - 1}: $message")

  def read(): Byte = {
    val ans = input.read().toByte
    position = position + 1
    ans
  }

  def read(num: Int): Array[Byte] = {
    val buffer = new Array[Byte](num)
    input.read(buffer)
    position = position + num
    buffer
  }

  def readUntil(delim: Byte): Array[Byte] = {
    val ans = Stream.continually(input.read).
      takeWhile(b => b != -1 && b != delim).
      map(_.toByte).
      toArray
    position = position + ans.length
    ans
  }

  def readInt(delim: Byte): Int =
    new String(readUntil(delim)).toInt

  def readDouble(delim: Byte): Double =
    new String(readUntil(delim)).toDouble

  def skip(exp: Byte): Unit = {
    val act = read()
    if(act != exp) {
      val expString = if(exp == -1) "EOF" else exp.toChar.toString
      val actString = if(act == -1) "EOF" else act.toChar.toString
      error(s"Expected <${expString}> found <${actString}>")
    }
  }

  def parse(): PhpValue = {
    read() match {
      case IntStart =>
        skip(ArgDelim)
        val str = new String(readUntil(ValueEnd))
        try {
          PhpInt(str.toInt)
        } catch {
          case exn: NumberFormatException =>
            PhpDouble(str.toDouble)
        }
      case DoubleStart =>
        skip(ArgDelim)
        PhpDouble(readDouble(ValueEnd))
      case BooleanStart =>
        skip(ArgDelim)
        val ans = read() match {
          case TrueValue  => PhpBoolean(true)
          case FalseValue => PhpBoolean(false)
          case other      => error(s"Unrecognised boolean value: $other")
        }
        skip(ValueEnd)
        ans
      case StringStart =>
        skip(ArgDelim)
        val len = readInt(ArgDelim)
        skip(StringDelim)
        val ans = PhpString(read(len))
        skip(StringDelim)
        skip(ValueEnd)
        ans
      case ArrayStart =>
        skip(ArgDelim)
        val length = readInt(ArgDelim)
        skip(FieldsStart)
        var fields = Queue.empty[(PhpValue, PhpValue)]
        for(i <- 0 until length) {
          val name  = parse()
          val value = parse()
          fields = fields :+ (name, value)
        }
        skip(FieldsEnd)
        PhpArray(fields)
      case ObjectStart =>
        skip(ArgDelim)
        val nameLength = readInt(ArgDelim)
        skip(StringDelim)
        val className = new String(read(nameLength))
        skip(StringDelim)
        skip(ArgDelim)
        val length = readInt(ArgDelim)
        skip(FieldsStart)
        var fields = Queue.empty[(PhpValue, PhpValue)]
        for(i <- 0 until length) {
          val name  = parse()
          val value = parse()
          fields = fields :+ (name, value)
        }
        skip(FieldsEnd)
        PhpObject(className, fields)
      case NullStart =>
        skip(ValueEnd)
        PhpNull
    }
  }
}
