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

  def increment(a: A): A Or OverflowError

  def decrement(a: A): A Or OverflowError

  def eq(x: A, y: A): Boolean

  def between(x: A, y: A): A Or BetweenFailed
}

sealed trait OverflowError

case object MaxOverflow extends OverflowError

case object MinUnderflow extends OverflowError

sealed trait BetweenFailed

object Rankable {
  implicit val int: Rankable[Int] =
    new Rankable[Int] {
      def min: Int =
        Int.MinValue

      def max: Int =
        Int.MaxValue

      def increment(a: Int): Int Or OverflowError = {
        val ret = a + 1

        if (ret < a)
          Left(MaxOverflow)
        else
          Right(a)
      }

      def decrement(a: Int): Int Or OverflowError = {
        val ret = a - 1

        if (ret > a)
          Left(MinUnderflow)
        else
          Right(a)
      }

      def eq(x: Int, y: Int): Boolean = ???

      def between(x: Int, y: Int): Int Or BetweenFailed = {
        val min = Math.min(x, y)
        val max = Math.max(x, y)

        val ret = util.Random.nextInt(max - min) + 1 + min

        if (ret > min && ret < max)
          Right(ret)
        else
          Left(new BetweenFailed {})
      }
    }
}