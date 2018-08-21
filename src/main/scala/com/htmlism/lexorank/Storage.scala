package com.htmlism.lexorank

import scala.annotation.tailrec

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
    AnnotatedIO {
      val pk = pkSeed
      pkSeed = K.increment(pkSeed)

      val rank = R.anywhere
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

  def doIt(id: K)(xs: Snapshot): Row Or ChangeError =
    xs
      .get(id)
      .fold[Row Or ChangeError](Left(IdDoesNotExistInStorage)) { _ =>
        Right(???)
      }

  def getSnapshot: AnnotatedIO[Snapshot] =
    AnnotatedIO(xs.map(r => r.id -> r.x.rank).toMap)

  // TODO when a collision is detected, use the relation to the midpoint to decide. below = increment, above = decrement
  def generateUpdateSequence(id: K, req: PositionRequest[K])(snap: Snapshot): List[Update] = {
    val ev = Rankable[R]

    @tailrec
    def tryToApply(up: Update, updates: List[Update]): List[Update] =
      rankCollidesAt(snap)(up.to) match {
        case Some((k, a)) =>
          val newRankForCollision = ev.decrement(a).toOption.get // TODO safety
          val evadeCollision = Update(k, a, newRankForCollision)

          tryToApply(evadeCollision, up :: updates)

        case None =>
          up :: updates
      }

    val newRank = generateNewRank(snap)(req)
    val update = Update(id, snap(id), newRank)

    tryToApply(update, Nil)
  }

  def rankCollidesAt(snap: Snapshot)(rank: R): Option[(K, R)] =
    snap.find { case (_, r) => R.eq(r, rank) }

  /**
   * This will be the new rank, regardless. Collided onto values will be pushed out.
   */
  def generateNewRank(snap: Snapshot)(req: PositionRequest[K]): R = {
    val rk = Rankable[R]

    val afterRank  = req.after.flatMap(snap.get)
    val beforeRank = req.before.flatMap(snap.get)

    (afterRank, beforeRank) match {
      case (Some(min), Some(max)) =>
        rk.between(min, max)

      case (Some(min), None) =>
        rk.after(min)

      case (None, Some(max)) =>
        rk.before(max)

      case (None, None) =>
        rk.anywhere
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
}