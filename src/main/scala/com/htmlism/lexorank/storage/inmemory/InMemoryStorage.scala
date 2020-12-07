package com.htmlism.lexorank
package storage.inmemory

import cats.effect._

object InMemoryStorage {

  /**
    * Factory from nothing.
    *
    * @tparam F An effect type
    * @tparam K A key type
    * @tparam R A rank type
    */
  def empty[F[_]: Sync, K: KeyLike, R]: InMemoryStorage[F, K, R] =
    from(Map.empty)

  /**
    * Factory method for seeding state
    *
    * @param xs A map of ranks to anonymous payloads
    *
    * @tparam F An effect type
    * @tparam K A key type
    * @tparam R A rank type
    */
  def from[F[_]: Sync, K: KeyLike, R](xs: Map[R, String]): InMemoryStorage[F, K, R] = {
    val store = new InMemoryStorage[F, K, R]

    xs.foreach { case (r, s) =>
      store.addRecord(s, r)
    }

    store
  }
}

/**
  * @param xs A bi-directional map between PKs and ranks.
  * @param K Evidence for key behaviors over `K`
  *
  * @tparam F An effect type
  * @tparam K The type for primary keys in this storage. Usually `Int`
  * @tparam R The type for ranking items relative to one another. Usually `Int` but could be something like `String`
  */
class InMemoryStorage[F[_], K, R](implicit F: Sync[F], K: KeyLike[K]) extends Storage[F, K, R] {
  private var pkSeed: K =
    K.first

  private[this] val xs =
    collection.mutable.Map.empty[K, Record[R]]

  def getSnapshot: F[Snapshot] =
    F.delay {
      xs.map(r => r._1 -> r._2.rank).toMap
    }

  def lockSnapshot: F[Snapshot] =
    F.delay {
      xs.map(r => r._1 -> r._2.rank).toMap
    }

  def insertNewRecord(payload: String, rank: R): F[Row] =
    F.delay {
      addRecord(payload, rank)
    }

  def changeRankTo(id: K, rank: R): F[Row] =
    F.delay {
      val withNewRank = xs(id).copy(rank = rank)

      xs(id) = withNewRank
      assertUniqueRanks()

      id -> withNewRank
    }

  def applyUpdateInCascade(up: Update): F[Unit] =
    F.delay {
      xs(up.pk) = Record("", up.to)
      assertUniqueRanks()
    }

  private[this] def assertUniqueRanks(): Unit =
    assert(xs.values.map(_.rank).toSet.size == xs.size, "ranks are unique")

  /**
    * Not a part of the public API. For testing only.
    */
  def addRecord(payload: String, rank: R): Row = {
    val rec = Record(payload, rank)

    val pk = pkSeed
    pkSeed = K.increment(pkSeed)

    xs += (pk -> rec)
    assertUniqueRanks()

    (pk, rec)
  }

  /**
    * Not a part of the public API. For testing only.
    */
  def dump: Map[K, Record[R]] =
    xs.toMap

  /**
    * Not a part of the public API. For testing only.
    */
  def size: Int =
    xs.size

  override def toString: String =
    (pkSeed :: xs.map(_.toString).toList).mkString("\n")
}
