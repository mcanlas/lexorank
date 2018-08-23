package com.htmlism.lexorank

/**
 * A strategy for generating values between `min` and `max`. It could be something like a random number or the
 * midpoint.
 *
 * Isolates non-determinism.
 */
trait RangedGenerator[A] {
  def between(min: A, max: A): A
}