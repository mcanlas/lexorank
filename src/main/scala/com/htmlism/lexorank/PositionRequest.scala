package com.htmlism.lexorank

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

case class Before[A](x: A) extends PositionRequest[A] {
  def after: Option[A] =
    None

  def before: Option[A] =
    Some(x)
}

case class After[A](x: A) extends PositionRequest[A] {
  def after: Option[A] =
    Some(x)

  def before: Option[A] =
    None
}

case class Between[A](min: A, max: A) extends PositionRequest[A] {
  def after: Option[A] =
    Some(min)

  def before: Option[A] =
    Some(max)
}