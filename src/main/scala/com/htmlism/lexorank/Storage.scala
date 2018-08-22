package com.htmlism.lexorank

import scala.annotation.tailrec

import cats.implicits._
import mouse.all._

/**
 * We can consciously choose not to support the use case of inserting new records in storage that currently has no
 * records in it. The existing `changePosition` method assumes that there is something that exists prior that needs
 * changing. Admin users can seed the database with at least one row to facilitate this.
 */
class Storage[K, R](implicit K: KeyLike[K], R: Rankable[R]) {
  type Row = Entity[K, Record[R]]
  type Snapshot = Map[K, R]

  private var pkSeed: K =
    K.first

  /**
   * `from` is not logically necessary but does make for safer, more-specific SQL statements.
   */
  case class Update(pk: K, from: R, to: R)

  /**
   * This is bi-directional map between PKs and ranks.
   */
  private val xs = collection.mutable.Buffer.empty[Row]

  def insertAt(payload: String, pos: PositionRequest[K]): AnnotatedIO[Row] =
    getSnapshot >>= insertAtReally(payload, pos)

  private def insertAtReally(payload: String, pos: PositionRequest[K])(ctx: Snapshot) =
    AnnotatedIO {
      val pk = pkSeed
      pkSeed = K.increment(pkSeed)

      // TODO currently may generate an unchecked collision
      val rank = generateNewRank(ctx)(pos)
      val rec = Record(payload, rank)

      withRow(pk, rec)

      Entity(pk, rec)
    }

  /**
   * ID cannot be equal either of the provided `before` or `after`.
   */
  def changePosition(id: K, req: PositionRequest[K]): AnnotatedIO[Row Or ChangeError] =
    if (req.after.contains(id))
      AnnotatedIO(Left(IdWasInAfter))

    else if (req.before.contains(id))
      AnnotatedIO(Left(IdWasInBefore))
    else
      getSnapshot
        .map(doIt(id))

  private def doIt(id: K)(ctx: Snapshot): Row Or ChangeError =
    ctx
      .get(id)
      .fold[Row Or ChangeError](Left(IdDoesNotExistInStorage)) { _ =>
        Right(???)
      }

  private def getSnapshot: AnnotatedIO[Snapshot] =
    AnnotatedIO(xs.map(r => r.id -> r.x.rank).toMap)

  private def generateUpdateSequence(id: K, pos: PositionRequest[K])(ctx: Snapshot): List[Update] = {
    @tailrec
    def makeSpaceFor(updates: List[Update])(rank: R): List[Update] =
      rankCollidesAt(ctx)(rank) match {
        case Some((k, a)) =>
          val newRankForCollision = R.decrement(a).toOption.get // TODO safety
          val butFirstDo = Update(k, a, newRankForCollision)

          makeSpaceFor(butFirstDo :: updates)(newRankForCollision)

        case None =>
          updates
      }

    generateNewRank(ctx)(pos) |> makeSpaceFor(Nil)
  }

  private def rankCollidesAt(ctx: Snapshot)(rank: R): Option[(K, R)] =
    ctx.find { case (_, r) => R.eq(r, rank) }

  /**
   * This will be the new rank, regardless. Collided onto values will be pushed out.
   */
  private def generateNewRank(ctx: Snapshot)(req: PositionRequest[K]): R = {
    val afterRank  = req.after.flatMap(ctx.get)
    val beforeRank = req.before.flatMap(ctx.get)

    (afterRank, beforeRank) match {
      case (Some(min), Some(max)) =>
        R.between(min, max)

      case (Some(min), None) =>
        R.after(min)

      case (None, Some(max)) =>
        R.before(max)

      case (None, None) =>
        R.anywhere
    }
  }

  def withRow(id: K, record: Record[R]): this.type =
    {
      val row = Entity(id, record)

      xs += row

      assert(xs.map(_.id).toSet.size == xs.size, "primary keys are unique")
      assert(xs.map(_.x.rank).toSet.size == xs.size, "ranks are unique")

      this
    }

  def size: Int =
    xs.size

  override def toString: String =
    (pkSeed :: xs.map(_.toString).toList).mkString("\n")
}

case class ChangeRequest[A](id: A, pos: PositionRequest[A])