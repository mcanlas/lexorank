package com.htmlism.lexorank

package object production {
  implicit val rgPosInt: RankGenerator[PosInt] =
    new RankGenerator[PosInt] {
      protected def min: PosInt =
        PosInt(1)

      protected def max: PosInt =
        PosInt(Int.MaxValue)

      def between(x: PosInt, y: PosInt): PosInt =
        PosInt {
          val min = Math.min(x.n, y.n)
          val max = Math.max(x.n, y.n)

          util.Random.nextInt(max - min) + min
        }
    }
}
