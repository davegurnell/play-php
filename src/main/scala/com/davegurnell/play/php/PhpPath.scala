package com.davegurnell.play.php

import play.api.data.validation.ValidationError

object PhpPath extends PhpPath(Seq.empty) with PhpValueWrapperImplicits

/**
 * A path into a `PhpValue` that can be used to search for and extract
 * fields from a specified location:
 *
 * {{{
 * val path = PhpPath \ "a" \ "b"
 * val php  = Php.arr("a" -> Php.arr("b" -> 123))
 * val ans  = path.apply(php)
 * // ans == PhpInt(123)
 * }}}
 *
 * Use the `\` method to construct paths that search for immediate children,
 * and the `\\` method to construct paths that search for arbitarary descendents.
 *
 * Paths are also used to create `PhpReads` and `PhpWrites` that extract/insert
 * values at the relevant location.
 */
case class PhpPath(path: Seq[PhpPathNode]) extends PhpValueWrapperImplicits {
  /** Create a path that searches for immediate children of this path with the specified `field`. */
  def \ (field: PhpValueWrapper): PhpPath = PhpPath(path :+ PhpChildNode(field.value))

  /** Create a path that searches for descendents of this path with the specified `field`. */
  def \\(field: PhpValueWrapper): PhpPath = PhpPath(path :+ PhpSearchNode(field.value))

  /** Concatenate two paths. */
  def ++(that: PhpPath): PhpPath = PhpPath(this.path ++ that.path)

  /** Extract all fields at this path from `value`. */
  def apply(value: PhpValue): Seq[PhpValue] = extractAll(value)

  /**
   * Extract all values at this path from `value`.
   */
  def extractAll(value: PhpValue): Seq[PhpValue] =
    path.foldLeft(List(value))((accum, node) => accum.flatMap(node.apply))

  /**
   * Extract a single value at this path from `value`.
   *
   * If multiple matching values are present, a `PhpError` is returned.
   */
  def extractOne(value: PhpValue): PhpResult[PhpValue] = (this apply value) match {
    case Nil        => PhpError(this, "error.path.missing")
    case ans :: Nil => PhpSuccess(ans)
    case _          => PhpError(this, "error.path.result.multiple")
  }

  /**
   * Create a `PhpReads` that extracts a value of type `A`
   * from this location in a `PhpValue`.
   */
  def read[A](implicit r: PhpReads[A]): PhpReads[A] =
    PhpReads[A] { value =>
      extractOne(value).flatMap(r.reads(_).repath(this))
    }

  override def toString: String =
    "PhpPath(" + ("__" +: path.map(_.pathString)).mkString(" ") + ")"
}

private[php] sealed trait PhpPathNode {
  def field: PhpValue

  def apply(value: PhpValue): Seq[PhpValue]

  def pathString: String = {
    val operator = this match {
      case _ : PhpChildNode  => "\\"
      case _ : PhpSearchNode => "\\\\"
    }

    val argument = field match {
      case PhpInt(value)     => value.toString
      case PhpDouble(value)  => value.toString
      case PhpBoolean(value) => value.toString
      case PhpNull           => "<null>"
      case PhpUndefined      => "<undefined>"
      case str: PhpString    => try { str.stringValue } catch { case exn: Exception => "<string>" }
      case arr: PhpArray     => "<array>"
      case obj: PhpObject    => "<object>"
    }

    operator + " " + argument
  }
}

private[php] case class PhpChildNode(field: PhpValue) extends PhpPathNode {
  def apply(value: PhpValue): Seq[PhpValue] =
    Seq(value \ field).filterNot(_ == PhpUndefined)
}

private[php] case class PhpSearchNode(field: PhpValue) extends PhpPathNode {
  def apply(value: PhpValue): Seq[PhpValue] =
    (value \\ field).filterNot(_ == PhpUndefined)
}
