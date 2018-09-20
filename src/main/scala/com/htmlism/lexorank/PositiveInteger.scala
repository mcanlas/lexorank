package com.htmlism.lexorank

object PositiveInteger {
  implicit val posIntOrdering: Ordering[PosInt] =
    Ordering.by(_.n)
}

/**
 * A newtype for integers that are greater than zero.
 */
case class PositiveInteger(n: Int) {
  override def toString: String =
    n.toString
}