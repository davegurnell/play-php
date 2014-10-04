package com.davegurnell.play.php

import play.api.data.validation.ValidationError

sealed trait PhpResult[+A] {
  def map[B](func: A => B): PhpResult[B] = this match {
    case PhpSuccess(value, path) => PhpSuccess(func(value), path)
    case error: PhpError         => error
  }

  def flatMap[B](func: A => PhpResult[B]): PhpResult[B] = this match {
    case PhpSuccess(value, path) => func(value).repath(path)
    case error: PhpError         => error
  }

  def repath(prefix: PhpPath): PhpResult[A] = this match {
    case PhpSuccess(value, path) => PhpSuccess(value, prefix ++ path)
    case PhpError(errors)        => PhpError(errors.map(pair => (prefix ++ pair._1) -> pair._2))
  }
}

case class PhpSuccess[A](value: A, path: PhpPath = PhpPath) extends PhpResult[A]

case class PhpError(errors: Seq[(PhpPath, Seq[ValidationError])]) extends PhpResult[Nothing]

object PhpError {
  /** Convenience constructor for `PhpErrors` with single paths and error messages. */
  def apply(path: PhpPath, message: String, args: String *): PhpError =
    PhpError(Seq(path -> Seq(ValidationError(message, args : _*))))
}
