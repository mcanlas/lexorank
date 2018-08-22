package com.htmlism.lexorank

/**
 * A newtype for integers that are greater than zero.
 */
case class PositiveInteger(n: Int) {
  override def toString: String =
    n.toString
}