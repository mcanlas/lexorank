package com.htmlism.lexorank

/**
  * Storage interface, backed by a Scala collection or SQL engine (like H2).
  *
  * @tparam F An effect type
  * @tparam K The type for primary keys in this storage. Usually `Int`
  * @tparam R The type for ranking items relative to one another. Usually `Int` but could be something like `String`
  */
trait Storage[F[_], K, R] {

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
    * Similar to `lockSnapshot` but does not lock the rows. Used as the initial query for a UI or to check state in
    * tests.
    */
  def getSnapshot: F[Snapshot]

  /**
    * This, in your mind types, opens a connection to the database. It uses a "select for update" lock to hold to
    * hold on to the entire key space, since the worst case scenario is that many of them get updated.
    *
    * Remember that your key space may also be a composite of two columns. If your rank column is `Int` and there is
    * an "organization" column that you also key by, this only needs to lock on one organization's ranks (if your
    * operations are always per-organization and never enforce global ranking across all organizations).
    */
  def lockSnapshot: F[Snapshot]

  /**
    * A "last mile" storage update that differentiates insert requests from change requests.
    *
    * @param payload Arbitrary data
    * @param rank The rank to insert this record at
    *
    * @return The newly inserted record with its new primary key
    */
  def insertNewRecord(payload: String, rank: R): F[Row]

  /**
    * A "last mile" storage update that differentiates insert requests from change requests.
    *
    * @param id The primary key of the record that is being updated
    * @param rank The rank to change this record to
    *
    * @return The updated record with its new rank
    */
  def changeRankTo(id: K, rank: R): F[Row]

  /**
    * This can be used in two places. It's first use is the granular element of a cascade; a cascade is a series of
    * these updates.
    *
    * It's second use is in a change request. After the "make space" cascade is complete, this will be used to update
    * the row that originated the request.
    */
  def applyUpdateInCascade(up: Update): F[Unit]
}
