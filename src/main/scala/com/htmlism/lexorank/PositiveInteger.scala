package com.htmlism.lexorank

import cats._

object PositiveInteger {
  implicit val posIntOrdering: Ordering[PosInt] =
    Ordering.by(_.n)

  implicit val postIntEq: Eq[PosInt] =
    Eq.fromUniversalEquals
}

/**
  * A newtype for integers that are greater than zero.
  */
case class PositiveInteger(n: Int) {
  override def toString: String =
    n.toString
}
