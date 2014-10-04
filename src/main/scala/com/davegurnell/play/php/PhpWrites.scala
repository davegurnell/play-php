package com.davegurnell.play.php

/**
 * Object that can write values of type `A` as `PhpValues`.
 */
trait PhpWrites[-A] {
  def writes(value: A): PhpValue
}

object PhpWrites extends DefaultPhpWrites {
  /** Construct a `PhpWrites` from a function. */
  def apply[A](w: A => PhpValue): PhpWrites[A] =
    new PhpWrites[A] { def writes(value: A) = w(value) }
}

/**
 * Implementations of `PhpWrites` for common data types,
 * all of which are provided by default via implicit scope.
 */
trait DefaultPhpWrites {
  implicit val phpValueWrites = new PhpWrites[PhpValue] {
    def writes(in: PhpValue) = in
  }

  implicit val intWrites = new PhpWrites[Int] {
    def writes(in: Int) = PhpInt(in)
  }

  implicit val doubleWrites = new PhpWrites[Double] {
    def writes(in: Double) = PhpDouble(in)
  }

  implicit val booleanWrites = new PhpWrites[Boolean] {
    def writes(in: Boolean) = PhpBoolean(in)
  }

  implicit val stringWrites = new PhpWrites[String] {
    def writes(in: String) = PhpString(in.getBytes)
  }

  implicit def optionWrites[A](implicit w: PhpWrites[A]) = new PhpWrites[Option[A]] {
    def writes(in: Option[A]) = in match {
      case Some(value) => w.writes(value)
      case None        => PhpNull
    }
  }

  implicit def mapWrites[A, B](implicit aw: PhpWrites[A], bw: PhpWrites[B]) = new PhpWrites[Map[A, B]] {
    def writes(in: Map[A, B]) = PhpArray(in.toSeq.map {
      case (field, value) =>
        aw.writes(field) -> bw.writes(value)
    })
  }

  implicit def traversableWrites[A](implicit w: PhpWrites[A]) = new PhpWrites[Traversable[A]] {
    def writes(in: Traversable[A]) = PhpArray(in.toSeq.zipWithIndex.map {
      case (value, index) =>
        PhpInt(index) -> w.writes(value)
    })
  }
}
