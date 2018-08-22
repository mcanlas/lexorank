package com.htmlism.lexorank

/**
 * Isolates non-determinism.
 */
trait RangedGenerator[A] {
  def between(min: A, max: A): A
}