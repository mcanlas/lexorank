package com.htmlism.lexorank
package storage.sql

import doobie._
import doobie.implicits._

/**
  * A bundle of Doobie SQL queries. Static in principle, but constructed to centralize key and rank types with evidence.
  *
  * @tparam K The primary key type
  * @tparam R The ranking type
  */
class SqlQueries[K: Meta, R: Meta] {

  /**
    * Conceptually a row in a relational database, containing a primary, a payload, and a rank.
    */
  type Row = (K, Record[R])

  /**
    * Used to echo back a row that was either newly inserted or recently updated.
    */
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

  /**
    * The primary query used when powering a listing UI.
    *
    * If the entities had some subdivision like organization, this method would accept an organization as a filter.
    */
  def selectAllRows: Query0[(K, R)] =
    sql"""
          SELECT
            id,
            rank
          FROM
            rankable_entities
       """.query[(K, R)]

  /**
    * Used to obtain a snapshot of the current rankings during a Lexorank update. The intention is to compose this
    * query in a transaction so that the entire keyspace is locked until the cascade is complete. At worst, a cascade
    * could update much of the keyspace.
    *
    * If the entities had some subdivision like organization, this method would accept an organization as a filter.
    */
  def selectAllRowsForUpdate: Query0[(K, R)] =
    sql"""
          SELECT
            id,
            rank
          FROM
            rankable_entities
          FOR UPDATE
       """.query[(K, R)]
}

object SqlQueries {

  /**
    * Used to insert a new record at a given rank.
    */
  def insert[R: Meta](payload: String, rank: R): Update0 =
    sql"""
          INSERT INTO
            rankable_entities
          VALUES (NULL, $payload, $rank)
       """.update

  /**
    * Used to update the rank of a record after space has been made for that rank.
    */
  def updateRankAfterCascade[K: Meta, R: Meta](id: K, rank: R): Update0 =
    sql"""
          UPDATE
            rankable_entities
          SET
            rank = $rank
          WHERE
            id = $id
       """.update

  /**
    * Used as the fundamental unit for a cascade. Many instances of this query will be composed together within one
    * cascade.
    */
  def updateRankInsideCascade[K: Meta, R: Meta](id: K, from: R, to: R): Update0 =
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
