package com.htmlism.lexorank

/**
  * A newtype for integers between 1 and 10 inclusive. Used to simulate an easily-crowded key space.
  */
case class UpToTen(n: Int) {
  override def toString: String =
    "T(" + n.toString + ")"
}

object UpToTen {
  implicit val upToTenOrdering: Ordering[UpToTen] =
    Ordering.by(_.n)

  implicit val rankableUpToTen: Rankable[UpToTen] =
    new Rankable[UpToTen] {
      protected def max: UpToTen =
        UpToTen(10)

      def increment(a: UpToTen): UpToTen Or MaxOverflow = {
        val inc = a.n + 1

        if (inc > 10)
          Left(MaxOverflow)
        else
          Right(UpToTen(inc))
      }

      def decrement(a: UpToTen): UpToTen Or MinUnderflow = {
        val dec = a.n - 1

        if (dec < 0)
          Left(MinUnderflow)
        else
          Right(UpToTen(dec))
      }

      def eq(x: UpToTen, y: UpToTen): Boolean =
        x.n == y.n

      def collisionStrategy(a: UpToTen): CollisionStrategy =
        if (a.n < max.n / 2)
          MoveUp
        else
          MoveDown
    }

  implicit val AlwaysSayMin: RankGenerator[UpToTen] =
    new RankGenerator[UpToTen] {
      override protected def min: UpToTen =
        UpToTen(1)

      protected def max: UpToTen =
        UpToTen(10)

      def between(x: UpToTen, y: UpToTen): UpToTen =
        min
    }
}
