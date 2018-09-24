package com.htmlism.lexorank

trait KeyLike[A] {
  def first: A

  def increment(a: A): A
}

object KeyLike {
  implicit val keyLikePosInt: KeyLike[PosInt] =
    new KeyLike[PosInt] {
      def first: PosInt = PosInt(1)

      def increment(a: PosInt): PosInt = PosInt(a.n + 1)
    }
}
