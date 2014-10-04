package com.davegurnell.play.php

import java.nio.charset.Charset
import scala.language.higherKinds
import scala.collection.generic
import play.api.data.validation.ValidationError
import play.api.libs.functional.{ Applicative, Functor }

/**
 * Object that can read `PhpValues` as type `A`.
 */
trait PhpReads[+A] {
  def reads(value: PhpValue): PhpResult[A]

  def map[B](func: A => B): PhpReads[B] =
    PhpReads[B] { value => this.reads(value).map(func) }
}

object PhpReads extends DefaultPhpReads {
  /** Construct a `PhpReads` from a function. */
  def apply[A](r: PhpValue => PhpResult[A]): PhpReads[A] =
    new PhpReads[A] { def reads(value: PhpValue) = r(value) }

  implicit val applicative: Applicative[PhpReads] =
    new Applicative[PhpReads] {
      def pure[A](value: A): PhpReads[A] =
        PhpReads[A] { _ =>
          PhpSuccess(value)
        }

      def map[A, B](m: PhpReads[A], f: A => B): PhpReads[B] =
        m.map(f)

      def apply[A, B](mf: PhpReads[A => B], ma: PhpReads[A]): PhpReads[B] =
        PhpReads[B] { value =>
          PhpResult.applicative(mf.reads(value), ma.reads(value))
        }
    }

  implicit val functor: Functor[PhpReads] =
    new Functor[PhpReads] {
      def fmap[A, B](reads: PhpReads[A], f: A => B): PhpReads[B] =
        reads.map(f)
    }
}

/**
 * Implementations of `PhpReads` for common data types,
 * all of which are provided by default via implicit scope.
 */
trait DefaultPhpReads {
  implicit val phpValueReads: PhpReads[PhpValue] =
    new PhpReads[PhpValue] {
      def reads(in: PhpValue) = PhpSuccess(in)
    }

  implicit val intReads: PhpReads[Int] =
    new PhpReads[Int] {
      def reads(in: PhpValue) = in match {
        case PhpInt(value) => PhpSuccess(value)
        case other         => PhpError(PhpPath, "error.expected.phpint")
      }
    }

  implicit val doubleReads: PhpReads[Double] =
    new PhpReads[Double] {
      def reads(in: PhpValue) = in match {
        case PhpDouble(value) => PhpSuccess(value)
        case PhpInt(value)    => PhpSuccess(value.toDouble)
        case other            => PhpError(PhpPath, "error.expected.phpdouble")
      }
    }

  implicit val booleanReads: PhpReads[Boolean] =
    new PhpReads[Boolean] {
      def reads(in: PhpValue) = in match {
        case PhpBoolean(value) => PhpSuccess(value)
        case other             => PhpError(PhpPath, "error.expected.phpboolean")
      }
    }

  def stringReads(charset: Charset): PhpReads[String] =
    new PhpReads[String] {
      def reads(in: PhpValue) = in match {
        case value: PhpString => PhpSuccess(value.stringValue(charset))
        case other            => PhpError(PhpPath, "error.expected.phpstring")
      }
    }

  implicit val stringReads: PhpReads[String] =
    new PhpReads[String] {
      def reads(in: PhpValue) = in match {
        case value: PhpString => PhpSuccess(value.stringValue)
        case other            => PhpError(PhpPath, "error.expected.phpstring")
      }
    }

  implicit def optionReads[A](implicit r: PhpReads[A]): PhpReads[Option[A]] =
    new PhpReads[Option[A]] {
      def reads(in: PhpValue) = in match {
        case PhpNull => PhpSuccess(None)
        case value   => r.reads(value).map(Some(_))
      }
    }

  // This code adapted directly from Play JSON's default mapReads:
  implicit def mapReads[A, B](implicit ra: PhpReads[A], rb: PhpReads[B]): PhpReads[Map[A, B]] =
    new PhpReads[Map[A, B]] {
      def reads(in: PhpValue) = in match {
        case value: PhpArrayLike =>
          type Errors = Seq[(PhpPath, Seq[ValidationError])]

          def replacePath(errors: Errors, field: PhpValue) = errors map {
            case (path, errors) => (PhpPath \ field) -> errors
          }

          def prependPath(errors: Errors, field: PhpValue) = errors map {
            case (path, errors) => (PhpPath \ field) ++ path -> errors
          }

          value.fields.foldLeft(Right(Map.empty): Either[Errors, Map[A, B]]) {
            case (acc, (field, value)) =>
              (acc, ra.reads(field), rb.reads(value)) match {
                case ( Right(values) , PhpSuccess(field, _), PhpSuccess(value, _) ) => Right(values + (field -> value))
                case ( Right(_)      , PhpError(errors)    , _                    ) => Left(replacePath(errors, field))
                case ( Right(_)      , _                   , PhpError(errors)     ) => Left(prependPath(errors, field))
                case ( Left(errors1) , PhpError(errors2)   , PhpError(errors3)    ) => Left(errors1 ++ replacePath(errors2, field) ++ prependPath(errors3, field))
                case ( Left(errors1) , PhpError(errors2)   , _                    ) => Left(errors1 ++ replacePath(errors2, field))
                case ( Left(errors1) , _                   , PhpError(errors2)    ) => Left(errors1 ++ prependPath(errors2, field))
                case ( Left(errors)  , _                   , _                    ) => Left(errors)
              }
          }.fold(
            errors => PhpError(errors),
            values => PhpSuccess(values)
          )

        case _ => PhpError(PhpPath, "error.expected.phparray")
      }
    }

  // This code adapted directly from Play JSON's default traversableReads:
  implicit def traversableReads[F[_], A](implicit bf: generic.CanBuildFrom[Nothing, A, F[A]], ra: PhpReads[A]): PhpReads[F[A]] =
    new PhpReads[F[A]] {
      def reads(in: PhpValue) = in match {
        case value: PhpArrayLike =>
          type Errors = Seq[(PhpPath, Seq[ValidationError])]

          def prependPath(errors: Errors, field: PhpValue) = errors map {
            case (path, errors) => (PhpPath \ field) ++ path -> errors
          }

          value.fields.foldLeft(Right(Vector.empty): Either[Errors, Vector[A]]) {
            case (acc, (field, value)) =>
              (acc, ra.reads(value)) match {
                case ( Right(values), PhpSuccess(value, _)  ) => Right(values :+ value)
                case ( Right(_),      PhpError(errors)      ) => Left(prependPath(errors, field))
                case ( Left(errors),  PhpSuccess(_, _)      ) => Left(errors)
                case ( Left(errors1), PhpError(errors2)     ) => Left(errors1 ++ prependPath(errors2, field))
              }
          }.fold(
            errors => PhpError(errors),
            values => {
              val builder = bf()
              values.foreach(builder += _)
              PhpSuccess(builder.result())
            }
          )

        case _ => PhpError(PhpPath, "error.expected.phparray")
      }
    }
}
