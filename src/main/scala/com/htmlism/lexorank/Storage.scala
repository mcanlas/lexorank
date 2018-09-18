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
}