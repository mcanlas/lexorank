package com.htmlism.lexorank

import cats._

sealed trait PositionRequest[+A] {
  def keys: List[A]
}

/**
  * Not used often. Could be used when creating the first row.
  */
case object Anywhere extends PositionRequest[Nothing] {
  def keys: List[Nothing] = Nil
}

case class Before[A](k: A) extends PositionRequest[A] {
  def keys: List[A] = List(k)
}

case class After[A](k: A) extends PositionRequest[A] {
  def keys: List[A] = List(k)
}

object Between {
  def apply[A](a: A, b: A)(implicit A: Eq[A]): Between[A] Or LexorankError =
    Either.cond(A.neqv(a, b), new Between[A](a, b), errors.DuplicateBetweenKeys)

  def unapply[A](x: Between[A]): Option[(A, A)] =
    Some(x.a -> x.b)
}

/**
  * The keys are unordered. The "first" key need not come first in the constructor.
  */
class Between[A] private (val a: A, val b: A) extends PositionRequest[A] {
  def keys: List[A] = List(a, b)

  override def toString: String = s"Between($a, $b)"
}
