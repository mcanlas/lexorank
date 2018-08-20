package com.htmlism.lexorank

import cats.effect._

/**
 * We can consciously choose not to support the use case of inserting new records in storage that currently has no
 * records in it. The existing `changePosition` method assumes that there is something that exists prior that needs
 * changing. Admin users can seed the database with at least one row to facilitate this.
 */
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
  def changePosition(id: Pk, afterBefore: AfterBefore[Pk]): AnnotatedIO[Either[ChangeError, Row]] =
    if (afterBefore.after.contains(id))
      AnnotatedIO(Left(IdWasInAfter))

    else if (afterBefore.before.contains(id))
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