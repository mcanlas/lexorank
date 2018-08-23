package com.htmlism.lexorank

/**
 * A strategy for generating values between `min` and `max`. It could be something like a random number or the
 * midpoint.
 *
 * Isolates non-determinism.
 */
trait RankGenerator[A] {
  /**
   * Using unsigned math. Should be "zero".
   */
  protected def min: A

  protected def max: A

  /**
   * Generates a value between `x` and `y`, `x` inclusive.
   *
   * `x` and `y` are unordered.
   *
   * `x` and `y` must not be equal.
   *
   * Even between two adjacent values, e.g. `3` and `4`, this function should continue to suggest one of them as a new
   * candidate rank (probably `3`). Key space exhaustion will be detected when a wrap-around is caused during an
   * increment/decrement cascade. Therefore, this should emit no errors.
   */
  def between(x: A, y: A): A

  /**
   * Generates a value between `min` and `max`.
   *
   * Derived.
   */
  def anywhere: A =
    between(min, max)

  def after(x: A): A =
    between(x, max)

  def before(x: A): A =
    between(min, x)
}