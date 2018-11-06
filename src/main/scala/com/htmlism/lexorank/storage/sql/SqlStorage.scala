package com.htmlism.lexorank
package storage
package sql

import cats.effect._
import cats.implicits._

import doobie._
import doobie.implicits._

/**
  * @tparam K The type for primary keys in this storage
  * @tparam R The type for ranking items relative to one another
  */
class SqlStorage[K: Get: Put, R: Get: Put] extends Storage[ConnectionIO, K, R] {
  private[this] val q =
    new SqlQueries[K, R]

  def getSnapshot: ConnectionIO[Snapshot] =
    q.selectAllRows.to[List].map(_.toMap)

  def lockSnapshot: ConnectionIO[Snapshot] =
    q.selectAllRowsForUpdate.to[List].map(_.toMap)

  def insertNewRecord(payload: String, rank: R): ConnectionIO[(K, Record[R])] =
    SqlQueries.insert(payload, rank).withUniqueGeneratedKeys[K]("id") >>= (q.selectOne(_).unique)

  def changeRankTo(id: K, rank: R): ConnectionIO[(K, Record[R])] =
    SqlQueries
      .updateRankAfterCascade(id, rank)
      .run
      .flatTap(assertOneAffected("last mile")) >> q.selectOne(id).unique

  def applyUpdateInCascade(up: Update): ConnectionIO[Unit] =
    SqlQueries
      .updateRankInsideCascade(up.pk, up.from, up.to)
      .run
      .flatTap(assertOneAffected("cascade"))
      .void

  /**
    * This might not be a good idea if MySQL reports zero rows updated for rows that don't physically need updating.
    */
  private[this] def assertOneAffected(in: String)(n: Int) =
    Async[ConnectionIO].delay {
      assert(n == 1, s"one row was updated in $in")
    }
}
