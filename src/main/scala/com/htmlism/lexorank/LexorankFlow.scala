package com.htmlism.lexorank

import scala.annotation.tailrec

import cats._
import cats.implicits._
import mouse.all._

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
class LexorankFlow[F[_], K, R](store: Storage[F, K, R], RG: RankGenerator[R])(implicit F: Monad[F],
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
    * Partially unified alias for IntelliJ's benefit.
    */
  type OrLexorankError[A] = A Or LexorankError

  /**
    * Scala `ListSet` is buggy in 2.11 so don't bother.
    */
  def getRows: F[List[K]] =
    store.getSnapshot
      .map(_.toList.sortBy(_._2).map(_._1))

  /**
    * A public method for attempting to insert an anonymous payload at some position.
    */
  def insertAt(payload: String, pos: PositionRequest[K]): F[Row Or LexorankError] =
    store.lockSnapshot >>= attemptWriteWorkflow(pos, store.insertNewRecord(payload, _))

  private def attemptWriteWorkflow(pos: PositionRequest[K], lastMile: R => F[Row])(ctx: Snapshot) =
    (isKeyInContext(pos, ctx) >>= canWeCreateANewRank(pos))
      .traverse((attemptWritesToStorage(lastMile) _).tupled)

  /**
    * Partially unified type annotation for IntelliJ's benefit.
    */
  private def isKeyInContext(req: PositionRequest[K], ctx: Snapshot): OrLexorankError[Snapshot] = {
    val maybeKeys = req.keys.map(ctx.get)

    Either.cond(maybeKeys.forall(_.nonEmpty), ctx, errors.KeyNotInContext)
  }

  private def attemptWritesToStorage(lastMile: R => F[Row])(xs: List[Update], newRank: R) =
    store.makeSpace(xs) *> lastMile(newRank)

  private def canWeCreateANewRank(pos: PositionRequest[K])(ctx: Snapshot) =
    generateNewRank(ctx)(pos) >>= maybeMakeSpaceFor(ctx)

  // TODO is there a pathological case here where you might request a change that is already true?
  def changePosition(req: ChangeRequest[K]): F[Row Or LexorankError] =
    store.lockSnapshot >>= attemptWriteWorkflow(req.req, store.changeRankTo(req.id, _))

  private def maybeMakeSpaceFor(ctx: Snapshot)(rank: R) = {
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
      previousStrategy: Option[CollisionStrategy]): List[Update] Or errors.OverflowError =
    Lexorank.rankCollidesAt(rank)(ctx) match {
      case Some(k) =>
        val strat =
          getStrat(rank, previousStrategy)

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
    *
    * At this point after `isKeyInContext` we assume that the keys in the position request exist in the context.
    */
  private def generateNewRank(ctx: Snapshot)(req: PositionRequest[K]): OrLexorankError[R] = {
    val afterRank  = req.after.flatMap(ctx.get)
    val beforeRank = req.before.flatMap(ctx.get)

    (afterRank, beforeRank) match {
      case (Some(min), Some(max)) =>
        Either.cond(O.compare(min, max) < 0, RG.between(min, max), errors.ImpossibleBetweenRequest)

      case (Some(min), None) =>
        Right(RG.after(min))

      case (None, Some(max)) =>
        Right(RG.before(max))

      case (None, None) =>
        Right(RG.anywhere)
    }
  }
}

object Lexorank {
  def rankCollidesAt[K, R](rank: R)(ctx: Map[K, R])(implicit R: Rankable[R]): Option[K] =
    ctx.find { case (_, r) => R.eq(r, rank) }.map(_._1)
}

/**
  * `from` is not logically necessary but does make for safer, more-specific SQL statements.
  */
case class RankUpdate[K, R](pk: K, from: R, to: R)
