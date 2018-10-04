package com.htmlism.lexorank
package storage.sql

import cats.effect.IO

import doobie._
import doobie.implicits._
import doobie.scalatest._
import org.scalatest.FreeSpec

class QueriesSpec[K, R] extends FreeSpec
  with IOChecker {
  def transactor: Transactor[IO] = ???

  /**
    * Conceptually a row in a relational database, containing a primary, a payload, and a rank.
    */
  type Row = (K, Record[R])

  def selectOne(id: K): Query0[Row] =
    sql"""
          SELECT
            id,
            payload,
            rank
          FROM
            rankable_entities
          WHERE
            id = $id
       """.query[Row]

  def selectAllRows: Query0[Row] =
    sql"""
          SELECT
            id,
            payload,
            rank
          FROM
            rankable_entities
       """.query[Row]

  def selectAllRowsForUpdate: Query0[Row] =
    sql"""
          SELECT FOR UPDATE
            id,
            payload,
            rank
          FROM
            rankable_entities
       """.query[Row]

  def insert(payload: String, rank: R): Update0 =
    sql"""
          UPDATE
            rankable_entities
       """.update

  def updateRankForLastMile(id: K, rank: R): Update0 =
    sql"""
          UPDATE
            rankable_entities
          SET
            rank = $rank
          WHERE
            rank = $id
       """.update

  def updateRankInCascade(id: K, from: R, to: R): Update0 =
    sql"""
          UPDATE
            rankable_entities
          SET
            rank = $to
          WHERE
            rank = $from
       """.update
}
