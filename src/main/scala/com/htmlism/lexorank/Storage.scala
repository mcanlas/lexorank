package com.htmlism.lexorank

import cats.effect._

class Storage[A] {
  case class Row(id: Pk, rank: A)

  private val xs = collection.mutable.Buffer.empty[Row]

  /**
   * ID cannot be equal either of the provided `before` or `after`.
   *
   * @param id
   * @param after
   * @param before
   * @return
   */
  def changePosition(id: Pk, after: Option[Pk], before: Option[Pk]): IO[Either[LexorankError, Row]] =
    IO {
      if (after.contains(id))
        Left(IdWasInAfter)
      else if (before.contains(id))
        Left(IdWasInBefore)
      else
        Right(???)
    }

  def getAllRanks: IO[List[A]] =
    IO {
      xs.map(_.rank).toList
    }

  def withRow(id: Pk, rank: A): IO[Row] =
    IO {
      val ret = Row(id, rank)

      xs += ret

      assert(xs.map(_.id).toSet.size == xs.size, "primary keys are unique")
      assert(xs.map(_.rank).toSet.size == xs.size, "ranks are unique")

      ret
    }
}