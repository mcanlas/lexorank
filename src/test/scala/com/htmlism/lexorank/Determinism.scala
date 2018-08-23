package com.htmlism.lexorank

trait Determinism {
  implicit val rgPosInt: RankGenerator[PosInt] =
    new RankGenerator[PosInt] {
      def between(x: PosInt, y: PosInt): PosInt =
        PosInt {
          val min = Math.min(x.n, y.n)
          val max = Math.max(x.n, y.n)

          min + 1
        }

      protected def min: PosInt =
        PosInt(1)

      protected def max: PosInt =
        PosInt(Int.MaxValue)
    }
}
