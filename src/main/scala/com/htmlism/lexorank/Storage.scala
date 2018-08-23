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
  type Row = (K, Record[R])
  type Snapshot = Map[K, R]
  type Update = RankUpdate[K, R]

  private var pkSeed: K =
    K.first

  /**
   * This is bi-directional map between PKs and ranks.
   */
  private val xs =
    collection.mutable.Map.empty[K, Record[R]]

  def insertAt(payload: String, pos: PositionRequest[K]): AnnotatedIO[Row] =
    getSnapshot >>= insertAtReally(payload, pos)

  private def insertAtReally(payload: String, pos: PositionRequest[K])(ctx: Snapshot) =
    {
      val pk = pkSeed
      pkSeed = K.increment(pkSeed)

      val rank = generateNewRank(ctx)(pos)
      val rec = Record(payload, rank)

      val preReqUpdates = rank |> makeSpaceFor(ctx)

      val updatesIO = preReqUpdates.traverse(applyUpdate)

      val appendIO = {
        AnnotatedIO {
          withRow(pk, rec)

          (pk, rec)
        }
      }

      updatesIO *> appendIO
    }

  private def applyUpdate(up: Update): AnnotatedIO[Unit] =
    AnnotatedIO {
      xs(up.pk) = Record("", up.to)
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
        // TODO nonsensical
        Right(id -> Record("", R.anywhere))
      }

  private def getSnapshot: AnnotatedIO[Snapshot] =
    AnnotatedIO(xs.map(r => r._1 -> r._2.rank).toMap)

  private def generateUpdateSequence(id: K, pos: PositionRequest[K])(ctx: Snapshot): List[Update] =
    generateNewRank(ctx)(pos) |> makeSpaceFor(ctx)

  private def makeSpaceFor(ctx: Snapshot)(rank: R): List[Update] = {
    println("\n\n\n\nentered this space")
    makeSpaceForReally(ctx, Nil, rank, None)
  }

  /**
   * If you keep dynamically recomputing your collision strategy, it's possible that you will keep suggesting keys that
   * were already "taken".
   */
  @tailrec
  private def makeSpaceForReally(ctx: Snapshot, updates: List[Update], rank: R, oStrat: Option[CollisionStrategy]): List[Update] =
    Lexorank.rankCollidesAt(rank)(ctx) match {
      case Some(k) =>
        val strat = oStrat.getOrElse(R.collisionStrategy(rank))

        // TODO encapsulate this algorithm
        // TODO still non-deterministic errors here given key exhaustion

        val newRankForCollision =
          strat match {
            case MoveUp =>
              R.increment(rank).toOption.get

            case MoveDown =>
              R.decrement(rank).toOption.get
          }

        println(ctx)
        println(s"via $strat $rank became $newRankForCollision")

        val butFirstDo = RankUpdate(k, rank, newRankForCollision)

        makeSpaceForReally(ctx, butFirstDo :: updates, newRankForCollision, Some(strat))

      case None =>
        updates
    }

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
      val row = (id, record)

      xs += row

      assert(xs.values.map(_.rank).toSet.size == xs.size, "ranks are unique")

      this
    }

  def size: Int =
    xs.size

  override def toString: String =
    (pkSeed :: xs.map(_.toString).toList).mkString("\n")
}

object Lexorank {
  def rankCollidesAt[K, R](rank: R)(ctx: Map[K, R])(implicit R: Rankable[R]): Option[K] =
    ctx.find { case (_, r) => R.eq(r, rank) }.map(_._1)
}

case class ChangeRequest[A](id: A, pos: PositionRequest[A])

/**
 * `from` is not logically necessary but does make for safer, more-specific SQL statements.
 */
case class RankUpdate[K, R](pk: K, from: R, to: R)