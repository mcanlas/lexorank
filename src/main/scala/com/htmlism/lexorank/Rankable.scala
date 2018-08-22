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
  protected def min: A

  protected def max: A

  def increment(a: A): A Or MaxOverflow

  def decrement(a: A): A Or MinUnderflow

  /**
   * Used to determine if increment or decrement is used to resolve collisions.
   */
  def collisionStrategy(a: A): CollisionStrategy

  def eq(x: A, y: A): Boolean

  /**
   * Generates a value between `x` and `y`, `x` inclusive.
   *
   * `x` and `y` are unordered.
   *
   * `x` and `y` must not be equal.
   *
   * Even between two adjacent values, e.g. `3` and `4`, this function should continue to suggest one of them as a new
   * candidate rank (probably `3`). Key space exhaustion will be detected when a wrap-around is caused during an
   * increment/decrement cascade. Therefore, this should emit no errors.
   */
  def between(x: A, y: A): A

  /**
   * Generates a value between `min` and `max`.
   *
   * Derived.
   */
  def anywhere: A =
    between(min, max)

  def after(x: A): A =
    between(x, max)

  def before(x: A): A =
    between(min, x)
}

sealed trait OverflowError

sealed trait MaxOverflow
case object MaxOverflow extends MaxOverflow

sealed trait MinUnderflow
case object MinUnderflow extends MinUnderflow

object Rankable {
  implicit val posInt: Rankable[PosInt] =
    new Rankable[PosInt] {
      protected def min: PosInt =
        PosInt(0)

      protected def max: PosInt =
        PosInt(Int.MaxValue)

      def increment(a: PosInt): PosInt Or MaxOverflow = {
        val ret = a.n + 1

        if (ret < a.n)
          Left(MaxOverflow)
        else
          Right(a)
      }

      def decrement(a: PosInt): PosInt Or MinUnderflow = {
        val ret = a.n - 1

        if (ret > a.n)
          Left(MinUnderflow)
        else
          Right(a)
      }

      def eq(x: PosInt, y: PosInt): Boolean =
        x.n == y.n

      def between(x: PosInt, y: PosInt): PosInt = {
        val min = Math.min(x.n, y.n)
        val max = Math.max(x.n, y.n)

        PosInt {
          util.Random.nextInt(max - min) + min
        }
      }

      def collisionStrategy(a: PosInt): CollisionStrategy =
        if (a.n < max.n / 2)
          MoveUp
        else
          MoveDown
    }
}

sealed trait CollisionStrategy
case object MoveDown extends CollisionStrategy
case object MoveUp   extends CollisionStrategy