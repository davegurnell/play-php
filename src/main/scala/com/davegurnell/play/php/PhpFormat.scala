package com.davegurnell.play.php

/**
 * Object that can read and write `PhpValues` as type `A`.
 */
trait PhpFormat[A] extends PhpReads[A] with PhpWrites[A]

object PhpFormat {
  /** Construct a `PhpFormat` from a pair of functions. */
  def apply[A](r: PhpValue => PhpResult[A], w: A => PhpValue): PhpFormat[A] =
    new PhpFormat[A] {
      def reads(value: PhpValue) = r(value)
      def writes(value: A) = w(value)
    }

  /** Construct a `PhpFormat` from a `PhpReads` and a `PhpWrites`. */
  def apply[A](r: PhpReads[A], w: PhpWrites[A]): PhpFormat[A] =
    new PhpFormat[A] {
      def reads(value: PhpValue) = r.reads(value)
      def writes(value: A) = w.writes(value)
    }
}
