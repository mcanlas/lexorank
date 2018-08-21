package com.htmlism.lexorank

/**
 * Behavior for manipulating ranks.
 *
 * Usually backed by `String` or `Int`.
 *
 * @tparam A
 */
trait Rankable[A] {
  /**
   * Using unsigned math. Should be "zero".
   */
  def min: A

  def max: A

  def increment(a: A): A Or MaxOverflow

  def decrement(a: A): A Or MinUnderflow

  def eq(x: A, y: A): Boolean

  /**
   * Generates a value between `x` and `y`, `x` inclusive.
   */
  def between(x: A, y: A): A

  /**
   * Generates a value between `min` and `max`.
   *
   * Derived.
   */
  def anywhere: A =
    between(min, max)
}

sealed trait OverflowError

sealed trait MaxOverflow
case object MaxOverflow extends MaxOverflow

sealed trait MinUnderflow
case object MinUnderflow extends MinUnderflow

object Rankable {
  /**
   * The summon pattern!
   */
  def apply[A : Rankable]: Rankable[A] =
    implicitly[Rankable[A]]

  implicit val int: Rankable[Int] =
    new Rankable[Int] {
      def min: Int =
        0

      def max: Int =
        Int.MaxValue

      def increment(a: Int): Int Or MaxOverflow = {
        val ret = a + 1

        if (ret < a)
          Left(MaxOverflow)
        else
          Right(a)
      }

      def decrement(a: Int): Int Or MinUnderflow = {
        val ret = a - 1

        if (ret > a)
          Left(MinUnderflow)
        else
          Right(a)
      }

      def eq(x: Int, y: Int): Boolean = ???

      def between(x: Int, y: Int): Int = {
        val min = Math.min(x, y)
        val max = Math.max(x, y)

        util.Random.nextInt(max - min) + min
      }
    }
}