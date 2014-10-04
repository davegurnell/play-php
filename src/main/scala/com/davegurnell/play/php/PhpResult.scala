package com.davegurnell.play.php

import play.api.data.validation.ValidationError
import play.api.libs.functional.Applicative

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

  def toOption = this match {
    case PhpSuccess(value, _) => Some(value)
    case PhpError(_)          => None
  }

  def getOrElse[B >: A](orElse: => B): B = this match {
    case PhpSuccess(value, _) => value
    case PhpError(_)          => orElse
  }
}

object PhpResult {
  implicit val applicative: Applicative[PhpResult] =
    new Applicative[PhpResult] {
      def pure[A](value: A): PhpResult[A] =
        PhpSuccess(value)

      def map[A, B](m: PhpResult[A], f: A => B): PhpResult[B] =
        m.map(f)

      def apply[A, B](mf: PhpResult[A => B], ma: PhpResult[A]): PhpResult[B] =
        (mf, ma) match {
          case ( PhpSuccess(f, _) , PhpSuccess(a, _) ) => PhpSuccess(f(a))
          case ( PhpError(e1)     , PhpError(e2)     ) => PhpError(PhpError.merge(e1, e2))
          case ( PhpError(e)      , _                ) => PhpError(e)
          case ( _                , PhpError(e)      ) => PhpError(e)
        }
    }
}

case class PhpSuccess[A](value: A, path: PhpPath = PhpPath) extends PhpResult[A]

case class PhpError(errors: Seq[(PhpPath, Seq[ValidationError])]) extends PhpResult[Nothing]

object PhpError {
  /** Convenience constructor for `PhpErrors` with single paths and error messages. */
  def apply(path: PhpPath, message: String, args: String *): PhpError =
    PhpError(Seq(path -> Seq(ValidationError(message, args : _*))))

  /** Merge two lists of paths/errors, ensuring that errors are still grouped by paths afterwards. */
  def merge(e1: Seq[(PhpPath, Seq[ValidationError])], e2: Seq[(PhpPath, Seq[ValidationError])]): Seq[(PhpPath, Seq[ValidationError])] =
    (e1 ++ e2).groupBy(_._1).mapValues(_.map(_._2).flatten).toList
}
