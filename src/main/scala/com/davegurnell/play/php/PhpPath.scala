package com.davegurnell.play.php

import play.api.data.validation.ValidationError

object PhpPath extends PhpPath(Seq.empty) with PhpValueWrapperImplicits

/**
 * Object representing a path into a `PhpValue`.
 *
 * Used to create `PhpReads` and `PhpWrites` that inspect the values at that point.
 */
case class PhpPath(path: Seq[PhpPathNode]) extends PhpValueWrapperImplicits {
  def \ (field: PhpValueWrapper): PhpPath = PhpPath(path :+ PhpChildNode(field.value))
  def \\(field: PhpValueWrapper): PhpPath = PhpPath(path :+ PhpSearchNode(field.value))

  def ++(that: PhpPath): PhpPath = PhpPath(this.path ++ that.path)

  def apply(value: PhpValue): Seq[PhpValue] = extractAll(value)

  def extractAll(value: PhpValue): Seq[PhpValue] =
    path.foldLeft(List(value))((accum, node) => accum.flatMap(node.apply))

  def extractOne(value: PhpValue): PhpResult[PhpValue] = (this apply value) match {
    case Nil        => PhpError(this, "error.path.missing")
    case ans :: Nil => PhpSuccess(ans)
    case _          => PhpError(this, "error.path.result.multiple")
  }

  def read[A](implicit r: PhpReads[A]): PhpReads[A] =
    PhpReads[A] { value =>
      extractOne(value).flatMap(r.reads(_).repath(this))
    }

  override def toString = "PhpPath(" + ("__" +: path.map(_.pathString)).mkString(" ") + ")"
}

sealed trait PhpPathNode {
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

case class PhpChildNode(field: PhpValue) extends PhpPathNode {
  def apply(value: PhpValue): Seq[PhpValue] =
    Seq(value \ field).filterNot(_ == PhpUndefined)
}

case class PhpSearchNode(field: PhpValue) extends PhpPathNode {
  def apply(value: PhpValue): Seq[PhpValue] =
    (value \\ field).filterNot(_ == PhpUndefined)
}
