package com.htmlism.lexorank

/**
 * A newtype for integers between 1 and 10 inclusive. Used to simulate an easily-crowded key space.
 */
case class UpToTen(n: Int)

object UpToTen {
  implicit val rankable: Rankable[UpToTen] =
    new Rankable[UpToTen] {
      protected def min: UpToTen =
        UpToTen(0)

      protected def max: UpToTen =
        UpToTen(10)

      def increment(a: UpToTen): UpToTen Or MaxOverflow = {
        val ret = a.n + 1

        if (ret < a.n)
          Left(MaxOverflow)
        else
          Right(a)
      }

      def decrement(a: UpToTen): UpToTen Or MinUnderflow = {
        val ret = a.n - 1

        if (ret > a.n)
          Left(MinUnderflow)
        else
          Right(a)
      }

      def eq(x: UpToTen, y: UpToTen): Boolean =
        x.n == y.n

      def between(x: UpToTen, y: UpToTen): UpToTen = {
        val min = Math.min(x.n, y.n)
        val max = Math.max(x.n, y.n)

        UpToTen {
          util.Random.nextInt(max - min) + min
        }
      }

      def collisionStrategy(a: UpToTen): CollisionStrategy =
        if (a.n < max.n / 2)
          MoveUp
        else
          MoveDown
    }
}