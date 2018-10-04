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

//  def updateRankForLastMile(id: K, rank: R): Update0 =
//    sql"""
//          UPDATE
//            rankable_entities
//          SET
//            rank = $rank
//          WHERE
//            rank = $id
//       """.update
//
//  def updateRankInCascade(id: K, from: R, to: R): Update0 =
//    sql"""
//          UPDATE
//            rankable_entities
//          SET
//            rank = $to
//          WHERE
//            id = $id and
//            rank = $from
//       """.update

}
