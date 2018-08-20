package com.htmlism.lexorank

import scala.annotation.tailrec

/**
 * We can consciously choose not to support the use case of inserting new records in storage that currently has no
 * records in it. The existing `changePosition` method assumes that there is something that exists prior that needs
 * changing. Admin users can seed the database with at least one row to facilitate this.
 */
class Storage[K, A : Rankable] {
  type Row = Entity[K, A]
  type Snapshot = Map[K, A]

  /**
   * `from` is not logically necessary but does make for safer, more-specific SQL statements.
   */
  case class Update(pk: K, from: A, to: A)

  private val xs = collection.mutable.Buffer.empty[Row]

  /**
   * ID cannot be equal either of the provided `before` or `after`.
   */
  def changePosition(id: K, afterBefore: AfterBefore[K]): AnnotatedIO[Row Or ChangeError] =
    if (afterBefore.after.contains(id))
      AnnotatedIO(Left(IdWasInAfter))

    else if (afterBefore.before.contains(id))
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
    AnnotatedIO(xs.map(r => r.id -> r.x).toMap)

  def generateUpdateSequence(id: K, afterBefore: AfterBefore[K])(snap: Snapshot): List[Update] = {
    val ev = Rankable[A]

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

    val newRank = generateNewRank(snap)(id, afterBefore)
    val update = Update(id, snap(id), newRank)

    tryToApply(update, Nil)
  }

  def rankCollidesAt(snap: Snapshot)(rank: A): Option[(K, A)] =
    snap.find { case (_, a) => a == rank }

  /**
   * This will be the new rank, regardless. Collided onto values will be pushed out.
   */
  def generateNewRank(snap: Snapshot)(id: K, afterBefore: AfterBefore[K]): A = {
    val ev = Rankable[A]

    val afterRank  = afterBefore.after.flatMap(snap.get).getOrElse(ev.min)
    val beforeRank = afterBefore.before.flatMap(snap.get).getOrElse(ev.max)

    ev.between(afterRank, beforeRank)
  }

  def withRow(id: K, rank: A): this.type =
    {
      val row = Entity(id, rank)

      xs += row

      assert(xs.map(_.id).toSet.size == xs.size, "primary keys are unique")
      assert(xs.map(_.x).toSet.size == xs.size, "ranks are unique")

      this
    }
}