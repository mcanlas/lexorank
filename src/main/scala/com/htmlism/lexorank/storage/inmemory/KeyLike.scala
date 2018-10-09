package com.htmlism.lexorank
package storage.inmemory

/**
  * Used only for in-memory. SQL-based storage will implement this internally (obviating the need for this typeclass).
  */
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
