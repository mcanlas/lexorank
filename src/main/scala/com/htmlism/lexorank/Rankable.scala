package com.htmlism.lexorank

/**
 * Behavior for manipulating ranks.
 *
 * In this current design, this trait leaks to the caller how new ranks are assigned. If it is determined that existing
 * ranks must be adjusted (when an initial collision is detected after a new rank is generated), a strategy must be
 * chosen for the entire run of the adjustment cascade (which is implemented recursively).
 *
 * If the new rank is higher than the midpoint of the key space, the preferred strategy is to adjust everything
 * downwards. This has a lower likelihood of key exhaustion for values below the new rank when compared to the smaller
 * space above the rank. Conversely, when the new rank is lower than the midpoint, the strategy is to adjust upwards.
 *
 * It is possible that in infinitely precise spaces this idea of one strategy ahead of time may not apply. There could
 * be multiple strategies to make space for keys in a more dynamic fashion.
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

/**
 * This error happens when attempting to adjust existing rankable values and then ending up in a spot where they can
 * no longer be adjusted. This does not necessarily mean that the key space is entirely exhausted, but it could be a
 * sign that the key space is very crowded and only supports safe inserts/changes in specific places. Redistributing
 * keys and/or using a larger key space can help alleviate this error.
 */
sealed trait OverflowError

sealed trait MaxOverflow extends OverflowError
case object MaxOverflow extends MaxOverflow

sealed trait MinUnderflow extends OverflowError
case object MinUnderflow extends MinUnderflow

object Rankable {
  def apply[A : Rankable]: Rankable[A] =
    implicitly[Rankable[A]]

  implicit def rankablePosInt(implicit RG: RangedGenerator[PosInt]): Rankable[PosInt] =
    new Rankable[PosInt] {
      protected def min: PosInt =
        PosInt(0)

      protected def max: PosInt =
        PosInt(Int.MaxValue)

      def increment(a: PosInt): PosInt Or MaxOverflow = {
        val inc = a.n + 1

        if (inc < a.n)
          Left(MaxOverflow)
        else
          Right(PosInt(inc))
      }

      def decrement(a: PosInt): PosInt Or MinUnderflow = {
        val dec = a.n - 1

        if (dec > a.n)
          Left(MinUnderflow)
        else
          Right(PosInt(dec))
      }

      def eq(x: PosInt, y: PosInt): Boolean =
        x.n == y.n

      def between(x: PosInt, y: PosInt): PosInt =
        RG.between(x, y)

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