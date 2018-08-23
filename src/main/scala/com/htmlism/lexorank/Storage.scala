package com.htmlism.lexorank

import scala.annotation.tailrec

import cats.implicits._
import mouse.all._

/**
 * We can consciously choose not to support the use case of inserting new records in storage that currently has no
 * records in it. The existing `changePosition` method assumes that there is something that exists prior that needs
 * changing. Admin users can seed the database with at least one row to facilitate this.
 *
 * @param RG A strategy for generating values in `R`
 * @param K Evidence for key behaviors over `K`
 * @param R Evidence for rank behaviors over `R`
 * @tparam K The type for primary keys in this storage. Usually `Int`
 * @tparam R The type for ranking items relative to one another. Usually `Int` but could be something like `String`
 */
class Storage[K, R](RG: RankGenerator[R])(implicit K: KeyLike[K], R: Rankable[R]) {
  /**
   * Conceptually a row in a relational database, containing a primary, a payload, and a rank.
   */
  type Row = (K, Record[R])

  /**
   * Easy access for relating unique keys to unique ranks. This is conceptually a bidirectional map.
   *
   * In SQL, this could be backed by a composite index over PK and ranks and enable index scans.
   */
  type Snapshot = Map[K, R]

  /**
   * This represents the desire to update a given row (identified by primary key) to some new rank. At runtime this
   * would be backed by a SQL UPDATE statement.
   */
  type Update = RankUpdate[K, R]

  private var pkSeed: K =
    K.first

  /**
   * This is bi-directional map between PKs and ranks.
   */
  private val xs =
    collection.mutable.Map.empty[K, Record[R]]

  /**
   * A public method for attempting to insert an anonymous payload at some position.
   */
  def insertAt(payload: String, pos: PositionRequest[K]): AnnotatedIO[Row Or String] =
    getSnapshot >>= attemptInsert(payload, pos)

  private def attemptInsert(payload: String, pos: PositionRequest[K])(ctx: Snapshot) =
    canWeCreateANewRank(pos)(ctx)
      .map(makeSpaceAndInsert(payload))
      .fold(handleKeySpaceError, _.map(_.asRight[String]))

  /**
   * We reached this area because we determined in memory that finding a new rank key was not possible. The
   * end of this IO should be the end of the transaction also (to relax the select for update locks).
   */
  private def handleKeySpaceError(err: OverflowError): AnnotatedIO[Row Or String] =
    AnnotatedIO {
      Left("could not make space for you, sorry bud")
    }

  private def makeSpaceAndInsert(payload: String)(e: (R, List[Update])) = {
    val (rank, preReqUpdates) = e

    val updatesIO = preReqUpdates.traverse(applyUpdate)

    val appendIO = {
      AnnotatedIO {
        val rec = Record(payload, rank)

        val pk = pkSeed
        pkSeed = K.increment(pkSeed)

        withRow(pk, rec)

        (pk, rec)
      }
    }

    updatesIO *> appendIO
  }

  private def canWeCreateANewRank(pos: PositionRequest[K])(ctx: Snapshot) =
    generateNewRank(ctx)(pos) |> makeSpaceFor(ctx)

  private def applyUpdate(up: Update): AnnotatedIO[Unit] =
    AnnotatedIO {
      xs(up.pk) = Record("", up.to)
      assertUniqueRanks()
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
        Right(id -> Record("", RG.anywhere))
      }

  /**
   * This, in your mind types, opens a connection to the database. It uses a "select for update" lock to hold to
   * hold on to the entire key space, since the worst case scenario is that many of them get updated.
   *
   * Remember that your key space may also be a composite of two columns. If your rank column is `Int` and there is
   * an "organization" column that you also key by, this only needs to lock on one organization's ranks (if your
   * operations are always per-organization and never enforce global ranking across all organizations).
   */
  private def getSnapshot: AnnotatedIO[Snapshot] =
    AnnotatedIO(xs.map(r => r._1 -> r._2.rank).toMap)

  private def generateUpdateSequence(id: K, pos: PositionRequest[K])(ctx: Snapshot) =
    generateNewRank(ctx)(pos) |> makeSpaceFor(ctx)

  private def makeSpaceFor(ctx: Snapshot)(rank: R) = {
    println("\n\n\n\nentered this space")
    makeSpaceForReally(ctx, Nil, rank, None)
      .map(ups => rank -> ups)
  }

  private def getStrat(rank: R, oStrat: Option[CollisionStrategy]) =
    oStrat.getOrElse(R.collisionStrategy(rank))

  private def tryMakeNewRank(rank: R)(strat: CollisionStrategy) =
    strat match {
      case MoveUp =>
        R.increment(rank)

      case MoveDown =>
        R.decrement(rank)
    }

  /**
   * Recursive runs of this function are referred to as "the cascade". In a worst case scenario, the rank you want
   * may be occupied by another row already. Additionally, attempting to move this tenant away to another rank may
   * "collide" into yet another tenant and the process repeats itself.
   *
   * If you keep dynamically recomputing your collision strategy, it's possible that you will keep suggesting keys that
   * were already "taken".
   */
  @tailrec
  private def makeSpaceForReally(ctx: Snapshot, updates: List[Update], rank: R, oStrat: Option[CollisionStrategy]): List[Update] Or OverflowError =
    Lexorank.rankCollidesAt(rank)(ctx) match {
      case Some(k) =>
        val strat =
          getStrat(rank, oStrat)

        val newRankMaybe =
          strat |> tryMakeNewRank(rank)

        // looks like a flatmap, but if we attempt to refactor, we will lose tail position
        newRankMaybe match {
          case Left(e) =>
            Left(e)

          case Right(newRankForCollision) =>
            println(ctx)
            println(s"via $strat $rank became $newRankForCollision")

            val butFirstDo = RankUpdate(k, rank, newRankForCollision)

            makeSpaceForReally(ctx, butFirstDo :: updates, newRankForCollision, Some(strat))
        }

      case None =>
        Right(updates)
    }

  /**
   * This will be the new rank, regardless. Collided onto values will be pushed out.
   */
  private def generateNewRank(ctx: Snapshot)(req: PositionRequest[K]): R = {
    val afterRank  = req.after.flatMap(ctx.get)
    val beforeRank = req.before.flatMap(ctx.get)

    (afterRank, beforeRank) match {
      case (Some(min), Some(max)) =>
        RG.between(min, max)

      case (Some(min), None) =>
        RG.after(min)

      case (None, Some(max)) =>
        RG.before(max)

      case (None, None) =>
        RG.anywhere
    }
  }

  def withRow(id: K, record: Record[R]): this.type =
    {
      val row = (id, record)

      xs += row

      assertUniqueRanks()

      this
    }

  private def assertUniqueRanks(): Unit =
    assert(xs.values.map(_.rank).toSet.size == xs.size, "ranks are unique")

  /**
   * Not a part of the public API. For testing only.
   */
  def dump: Map[K, Record[R]] =
    xs.toMap

  /**
   * Not a part of the public API. For testing only.
   */
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