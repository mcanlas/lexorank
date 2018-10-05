package com.htmlism.lexorank
package storage.sql

import doobie._
import doobie.implicits._

/**
  * Static in principle. Constructed to bind key and rank types with evidence.
  */
class Queries[K: Meta, R: Meta] {

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
          SELECT
            id,
            payload,
            rank
          FROM
            rankable_entities
          FOR UPDATE
       """.query[Row]

  import shapeless._

  def insert(payload: String, rank: R)(implicit ev: Param[String :: R :: HNil]): Update0 =
    sql"""
          INSERT INTO
            rankable_entities
          VALUES (NULL, $payload, $rank)
       """.update

  def updateRankAfterCascade(id: K, rank: R)(implicit ev: Param[R :: K :: HNil]): Update0 =
    sql"""
          UPDATE
            rankable_entities
          SET
            rank = $rank
          WHERE
            rank = $id
       """.update

  def updateRankInsideCascade(id: K, from: R, to: R)(implicit ev: Param[R :: K :: R :: HNil]): Update0 =
    sql"""
          UPDATE
            rankable_entities
          SET
            rank = $to
          WHERE
            id = $id and
            rank = $from
       """.update

}

import shapeless._
class MinimalBad[A: Meta, B: Meta] {
  def bad(a: A, b: B)(implicit ev: Param[A :: B :: HNil]): Update0 =
    sql"""$a $b""".update
}
