package com.htmlism.lexorank

/**
 * Behavior for manipulating ranks.
 *
 * Usually backed by `String` or `Int`.
 *
 * @tparam A
 */
trait Rankable[A] {
  def min: A

  def max: A

  def increment(a: A): A Or BoundsError

  def decrement(a: A): A Or BoundsError

  def eq(x: A, y: A): Boolean

  def between(x: A, y: A): A
}

sealed trait BoundsError

case object MaxOverflow extends BoundsError

case object MinUnderflow extends BoundsError

object Rankable {
  implicit val int: Rankable[Int] =
    new Rankable[Int] {
      def min: Int =
        Int.MinValue

      def max: Int =
        Int.MaxValue

      def increment(a: Int): Int Or BoundsError = {
        val ret = a + 1

        if (ret < a)
          Left(MaxOverflow)
        else
          Right(a)
      }

      def decrement(a: Int): Int Or BoundsError = {
        val ret = a - 1

        if (ret > a)
          Left(MinUnderflow)
        else
          Right(a)
      }

      def eq(x: Int, y: Int): Boolean = ???

      def between(x: Int, y: Int): Int = ???
    }
}