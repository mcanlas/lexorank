package com.htmlism.lexorank

import cats.data._

object AfterBefore {
  def after[A](x: A) =
    AfterBefore(Ior.left(x))

  def before[A](x: A) =
    AfterBefore(Ior.right(x))

  def afterAndBefore[A](x: A, y: A) =
    AfterBefore(Ior.both(x, y))
}

case class AfterBefore[A](x: A Ior A) {
  def after: Option[A] =
    x.left

  def before: Option[A] =
    x.right
}
