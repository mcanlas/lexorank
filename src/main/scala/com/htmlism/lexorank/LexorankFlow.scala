package com.htmlism.lexorank

import scala.annotation.tailrec

import cats._
import cats.implicits._
import mouse.all._

import com.htmlism.lexorank.errors.KeyNotInContext

/**
  * We can consciously choose not to support the use case of inserting new records in storage that currently has no
  * records in it. The existing `changePosition` method assumes that there is something that exists prior that needs
  * changing. Admin users can seed the database with at least one row to facilitate this.
  *
  * @param store Persistent storage
  * @param RG A strategy for generating values in `R`
  *
  * @param F Evidence for IO
  * @param R Evidence for rank behaviors over `R`
  *
  * @tparam F An effect type
  * @tparam K The type for primary keys in this storage. Usually `Int`
  * @tparam R The type for ranking items relative to one another. Usually `Int` but could be something like `String`
  */
// TODO differentiate between outer effect type F and database IO type G
class LexorankFlow[F[_], K, R](store: Storage[F, K, R], RG: RankGenerator[R])(
    implicit F: Monad[F],
    R: Rankable[R],
    O: Ordering[R]) {

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

  /**
    * Scala `ListSet` is buggy in 2.11 so don't bother.
    */
  def getRows: F[List[K]] =
    store.getSnapshot
      .map(_.toList.sortBy(_._2).map(_._1))

  /**
    * A public method for attempting to insert an anonymous payload at some position.
    */
  // TODO request keys must be validated as being in the context
  def insertAt(payload: String, pos: PositionRequest[K]): F[Row Or LexorankError] =
    store.lockSnapshot.map(inContext(pos))
    .flatMap {
      case Left(err) =>
        err.asLeft[Nothing].pure

      case Right(x) =>
        attemptInsert(payload, pos)(x) match {
          case Left(err) =>
            err.asLeft[Nothing].pure

          case Right(x2) =>
            x2.map(_.asRight[LexorankError])
        }
  }

  private def inContext(req: PositionRequest[K])(ctx: Snapshot) = {
    val toTest = req.before.toList ::: req.after.toList

    val maybeKeys = toTest.map(ctx.get)

    if (maybeKeys.exists(_.isEmpty))
      Left(KeyNotInContext : LexorankError)
    else
      Right(ctx)
  }

  private def attemptInsert(payload: String, pos: PositionRequest[K])(
      ctx: Snapshot) =
    canWeCreateANewRank(pos)(ctx)
      .map {
        case (xs, r) => store.makeSpace(xs) *> store.insertNewRecord(payload, r)
      }

  /**
    * We reached this area because we determined in memory that finding a new rank key was not possible. The
    * end of this IO should be the end of the transaction also (to relax the select for update locks).
    */
  private def handleKeySpaceError(err: errors.OverflowError): F[Row Or String] =
    F.pure {
      Left("could not make space for you, sorry bud")
    }

  private def canWeCreateANewRank(pos: PositionRequest[K])(ctx: Snapshot) =
    generateNewRank(ctx)(pos) |> makeSpaceFor(ctx)

  /**
    * ID cannot be equal either of the provided `before` or `after`.
    */
  // TODO is there a pathological case here where you might request a change that is already true?
  def changePosition(id: K, req: PositionRequest[K]): F[Row Or ChangeError] =
    if (req.after.contains(id))
      F.pure(Left(IdWasInAfter))
    else if (req.before.contains(id))
      F.pure(Left(IdWasInBefore))
    else
      store.lockSnapshot
        .map(doIt(id))

  // TODO these guts are totally wrong
  private def doIt(id: K)(ctx: Snapshot): Row Or ChangeError =
    ctx
      .get(id)
      .fold[Row Or ChangeError](Left(IdDoesNotExistInStorage)) { _ =>
        // TODO nonsensical
        Right(id -> Record("", RG.anywhere))
      }

  private def makeSpaceFor(ctx: Snapshot)(rank: R) = {
    println("\n\n\n\nentered this space")
    makeSpaceForReally(ctx, Nil, rank, None)
      .map(ups => ups -> rank)
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
  private def makeSpaceForReally(
      ctx: Snapshot,
      updates: List[Update],
      rank: R,
      oStrat: Option[CollisionStrategy]): List[Update] Or errors.OverflowError =
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

            makeSpaceForReally(ctx,
                               butFirstDo :: updates,
                               newRankForCollision,
                               Some(strat))
        }

      case None =>
        Right(updates)
    }

  /**
    * This will be the new rank, regardless. Collided onto values will be pushed out.
    */
  // TODO it is possible that the key did not exist in the database
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
}

object Lexorank {
  def rankCollidesAt[K, R](rank: R)(ctx: Map[K, R])(
      implicit R: Rankable[R]): Option[K] =
    ctx.find { case (_, r) => R.eq(r, rank) }.map(_._1)
}

case class ChangeRequest[A](id: A, pos: PositionRequest[A])

/**
  * `from` is not logically necessary but does make for safer, more-specific SQL statements.
  */
case class RankUpdate[K, R](pk: K, from: R, to: R)
