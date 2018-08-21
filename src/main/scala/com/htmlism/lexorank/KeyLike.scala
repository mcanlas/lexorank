package com.htmlism.lexorank

trait KeyLike[A] {
  def first: A

  def increment(a: A): A
}

object KeyLike {
  implicit val int: KeyLike[Int] =
    new KeyLike[Int] {
      def first: Int = 1

      def increment(a: Int): Int = a + 1
    }
}