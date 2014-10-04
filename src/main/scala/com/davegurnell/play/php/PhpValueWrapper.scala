package com.davegurnell.play.php

import scala.language.implicitConversions

/**
 * Helper class for implementing methods that take `PhpValues` or
 * equivalent primitives as arguments.
 */
case class PhpValueWrapper(value: PhpValue)

trait PhpValueWrapperImplicits {
  /**
   * Helper method for implementing methods that take `PhpValues` or
   * equivalent primitives as arguments.
   */
  implicit def createPhpValueWrapper[A](value: A)(implicit writes: PhpWrites[A]): PhpValueWrapper =
    PhpValueWrapper(writes.writes(value))

  /**
   * Helper method for implementing methods that take pairs of `PhpValues` or
   * equivalent primitives as arguments.
   */
  implicit def createPhpValueWrapperPair[A, B](pair: (A, B))(implicit fieldWrites: PhpWrites[A], valueWrites: PhpWrites[B]): (PhpValueWrapper, PhpValueWrapper) =
    (PhpValueWrapper(fieldWrites.writes(pair._1)), PhpValueWrapper(valueWrites.writes(pair._2)))
}
