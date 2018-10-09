package com.htmlism.lexorank

import scala.annotation.tailrec

import cats._
import cats.implicits._
import mouse.all._

import com.htmlism.lexorank.ranking._
import com.htmlism.lexorank.request._

/**
  * We can consciously choose not to support the use case of inserting new records in storage that currently has no
  * records in it. The existing `changePosition` method assumes that there is something that exists prior that needs
  * changing. Admin users can seed the database with at least one row to facilitate this.
  *
  * @param tx Like a Doobie transactor; a natural transformation from storage effect `G` to effect type `F`
  * @param store Persistent storage for rows, keys, and ranks
  * @param RG A strategy for generating values in `R`
  *
  * @tparam F The outer, transactional effect type (like `IO`) used after interpreting programs written in `G`
  * @tparam G A composable effect type for storage
  * @tparam K The type for primary keys in this storage. Usually `Int`
  * @tparam R The type for ranking items relative to one another. Usually `Int` but could be something like `String`
  */
class LexorankFlow[F[_], G[_]: Monad, K, R](tx: G ~> F, store: Storage[G, K, R], RG: RankGenerator[R])(
    implicit R: Rankable[R]) {

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
  def getRows(implicit ord: Ordering[R]): F[List[K]] =
    tx {
      store.getSnapshot
        .map(_.toList.sortBy(_._2).map(_._1))
    }

  /**
    * A public method for attempting to insert an anonymous payload at some position.
    */
  def insertAt(payload: String, req: PositionRequest[K]): F[Row Or LexorankError] =
    tx {
      store.lockSnapshot >>= attemptWriteWorkflow(req, store.insertNewRecord(payload, _))
    }

  // TODO is there a pathological case here where you might request a change that is already true?
  def changePosition(chReq: ChangeRequest[K]): F[Row Or LexorankError] =
    tx {
      store.lockSnapshot >>= attemptWriteWorkflow(chReq.req, store.changeRankTo(chReq.id, _))
    }

  private def attemptWriteWorkflow(req: PositionRequest[K], consumeRank: R => G[Row])(ctx: Snapshot) =
    (areRequestKeysValid(req, ctx) >>= canWeCreateANewRank(req))
      .traverse((attemptWritesToStorage(consumeRank) _).tupled)

  // partially applied either bifunctor for error-free flatmap in intellij
  private def areRequestKeysValid(req: PositionRequest[K], ctx: Snapshot): OrLexorankError[Snapshot] =
    Either.cond(req.keys.forall(ctx.contains), ctx, errors.KeyNotInContext)

  private def attemptWritesToStorage(consumeRank: R => G[Row])(xs: List[Update], newRank: R) =
    xs.traverse_(store.applyUpdateInCascade) *> consumeRank(newRank)

  private def canWeCreateANewRank(req: PositionRequest[K])(ctx: Snapshot) =
    generateNewRank(ctx)(req) |> maybeMakeSpaceForNewRank(ctx)

  private def maybeMakeSpaceForNewRank(ctx: Snapshot)(rank: R) = {
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
  private def makeSpaceForReally(ctx: Snapshot,
                                 updates: List[Update],
                                 rank: R,
                                 previousStrategy: Option[CollisionStrategy]): List[Update] Or errors.OverflowError =
    rankCollidesAt(rank)(ctx) match {
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
  private def generateNewRank(ctx: Snapshot)(req: PositionRequest[K]): R =
    req match {
      case Before(x) =>
        RG.before(ctx(x))

      case After(x) =>
        RG.after(ctx(x))

      case Between(x, y) =>
        RG.between(ctx(x), ctx(y))

      case Anywhere =>
        RG.anywhere
    }

  private def rankCollidesAt(rank: R)(ctx: Map[K, R]) =
    ctx.find { case (_, r) => R.eq(r, rank) }.map(_._1)
}

/**
  * `from` is not logically necessary but does make for safer, more-specific SQL statements.
  */
case class RankUpdate[K, R](pk: K, from: R, to: R)
