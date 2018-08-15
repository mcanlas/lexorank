package com.htmlism.lexorank

import cats.effect._

object Storage {
  def apply(xs: (Pk, Rank)*): Storage =
    Storage(xs.map((Row.apply _).tupled).toList)
}

case class Storage(xs: List[Row]) {
  assert(xs.map(_.id).toSet.size == xs.size, "primary keys are unique")
  assert(xs.map(_.rank).toSet.size == xs.size, "ranks are unique")

  def getAllRanks: IO[List[Rank]] =
    IO {
      xs.map(_.rank)
    }
}