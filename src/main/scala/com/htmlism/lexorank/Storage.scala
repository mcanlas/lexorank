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
  def changePosition(id: Pk, after: Option[Pk], before: Option[Pk]): AnnotatedIO[Either[LexorankError, Row]] =
      if (after.contains(id))
        AnnotatedIO(Left(IdWasInAfter))

      else if (before.contains(id))
        AnnotatedIO(Left(IdWasInBefore))
      else {
        // spent one connection
        exists(id)
          .map { t =>
            if (t)
              Right(???)
            else
              Left(IdDoesNotExistInStorage)
          }
      }

  def getAllRanks: AnnotatedIO[List[A]] =
    AnnotatedIO {
      xs.map(_.rank).toList
    }

  def getAllIds: AnnotatedIO[List[Pk]] =
    AnnotatedIO {
      xs.map(_.id).toList
    }

  def exists(id: Pk): AnnotatedIO[Boolean] =
    getAllIds
      .map(_.contains(id))

  def withRow(id: Pk, rank: A): IO[Row] =
    IO {
      val ret = Row(id, rank)

      xs += ret

      assert(xs.map(_.id).toSet.size == xs.size, "primary keys are unique")
      assert(xs.map(_.rank).toSet.size == xs.size, "ranks are unique")

      ret
    }
}