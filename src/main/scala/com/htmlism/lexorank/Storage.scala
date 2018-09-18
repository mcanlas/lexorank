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
   * This, in your mind types, opens a connection to the database. It uses a "select for update" lock to hold to
   * hold on to the entire key space, since the worst case scenario is that many of them get updated.
   *
   * Remember that your key space may also be a composite of two columns. If your rank column is `Int` and there is
   * an "organization" column that you also key by, this only needs to lock on one organization's ranks (if your
   * operations are always per-organization and never enforce global ranking across all organizations).
   */
  def lockSnapshot: F[Snapshot]

  def makeSpace(xs: List[Update]): F[Unit]

  def insertNewRecord(payload: String, rank: R): F[Row]

  def applyUpdate(up: Update): F[Unit]
}