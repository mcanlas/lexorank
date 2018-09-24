package com.htmlism.lexorank

import cats._

// TODO this probably doesn't need to be a whole ADT
sealed trait PositionRequest[+A] {
  def before: Option[A]

  def after: Option[A]
}

/**
  * Not used often. Could be used when creating the first row.
  */
case object Anywhere extends PositionRequest[Nothing] {
  def after: Option[Nothing] =
    None

  def before: Option[Nothing] =
    None
}

case class Before[A](k: A) extends PositionRequest[A] {
  def after: Option[A] =
    None

  def before: Option[A] =
    Some(k)
}

case class After[A](k: A) extends PositionRequest[A] {
  def after: Option[A] =
    Some(k)

  def before: Option[A] =
    None
}

object Between {
  def apply[A](a: A, b: A)(implicit A: Eq[A]): Option[Between[A]] =
    if (A.eqv(a, b))
      None
    else
      Some(new Between[A](a, b))
}

class Between[A] private (min: A, max: A) extends PositionRequest[A] {
  def after: Option[A] =
    Some(min)

  def before: Option[A] =
    Some(max)
}
