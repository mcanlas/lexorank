package com.htmlism.lexorank

trait Determinism {
  implicit val rgPosInt: RangedGenerator[PosInt] =
    new RangedGenerator[PosInt] {
      def between(x: PosInt, y: PosInt): PosInt =
        PosInt {
          val min = Math.min(x.n, y.n)
          val max = Math.max(x.n, y.n)

          min + 1
        }
    }
}
