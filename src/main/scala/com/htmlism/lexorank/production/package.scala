package com.htmlism.lexorank

package object production {
  implicit val rgPosInt: RangedGenerator[PosInt] =
    new RangedGenerator[PosInt] {
      def between(x: PosInt, y: PosInt): PosInt =
        PosInt {
          val min = Math.min(x.n, y.n)
          val max = Math.max(x.n, y.n)

          util.Random.nextInt(max - min) + min
        }
    }
}
