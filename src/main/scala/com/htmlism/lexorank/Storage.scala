package com.htmlism.lexorank

import cats.effect._
import mouse.all._

/**
 * We can consciously choose not to support the use case of inserting new records in storage that currently has no
 * records in it. The existing `changePosition` method assumes that there is something that exists prior that needs
 * changing. Admin users can seed the database with at least one row to facilitate this.
 */
class Storage[K, A] {
  type Row = Entity[K, A]

  private val xs = collection.mutable.Buffer.empty[Row]

  /**
   * ID cannot be equal either of the provided `before` or `after`.
   */
  def changePosition(id: K, afterBefore: AfterBefore[K]): AnnotatedIO[Either[ChangeError, Row]] =
    if (afterBefore.after.contains(id))
      AnnotatedIO(Left(IdWasInAfter))

    else if (afterBefore.before.contains(id))
      AnnotatedIO(Left(IdWasInBefore))
    else
      getSnapshot
        .map(doIt(id))

  def doIt(id: K)(xs: List[Row]): Either[ChangeError, Row] =
    xs
      .map(_.id)
      .contains(id) |> { t =>
        if (t)
          Right(???)
        else
          Left(IdDoesNotExistInStorage)
      }

  def getSnapshot: AnnotatedIO[List[Row]] =
    AnnotatedIO(xs.toList)

  def withRow(id: K, rank: A): IO[Row] =
    IO {
      val ret = Entity(id, rank)

      xs += ret

      assert(xs.map(_.id).toSet.size == xs.size, "primary keys are unique")
      assert(xs.map(_.x).toSet.size == xs.size, "ranks are unique")

      ret
    }
}