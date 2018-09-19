package com.htmlism.lexorank
package storage

import cats.implicits._
import cats.effect._

object ScalaCollectionStorage {
  /**
   * Factory from nothing.
   *
   * @tparam F An effect type
   * @tparam K A key type
   * @tparam R A rank type
   */
  def empty[F[_] : Sync, K : KeyLike, R]: ScalaCollectionStorage[F, K, R] =
    from(Map.empty)

  /**
   * Factory method for seeding state
   *
   * @param xs An immutable bi-directional map. Construction will bomb if the map is not bidirectionally unique
   *
   * @tparam F An effect type
   * @tparam K A key type
   * @tparam R A rank type
   */
  def from[F[_] : Sync, K : KeyLike, R](xs: Map[K, Record[R]]): ScalaCollectionStorage[F, K, R] =
    new ScalaCollectionStorage(collection.mutable.Map(xs.toList: _*))
}

/**
 * @param xs A bi-directional map between PKs and ranks.
 * @param K Evidence for key behaviors over `K`
 *
 * @tparam F An effect type
 * @tparam K The type for primary keys in this storage. Usually `Int`
 * @tparam R The type for ranking items relative to one another. Usually `Int` but could be something like `String`
 */
class ScalaCollectionStorage[F[_], K, R](xs: collection.mutable.Map[K, Record[R]])(implicit F: Sync[F], K: KeyLike[K]) extends Storage[F, K, R] {
  assertUniqueRanks()

  private var pkSeed: K =
    K.first

  def lockSnapshot: F[Snapshot] =
    F.delay {
      xs
        .map(r => r._1 -> r._2.rank)
        .toMap
    }

  def makeSpace(xs: List[Update]): F[Unit] =
    xs.traverse_(applyUpdate)

  def insertNewRecord(payload: String, rank: R): F[Row] =
    F.delay {
      val rec = Record(payload, rank)

      val pk = pkSeed
      pkSeed = K.increment(pkSeed)

      withRow(pk, rec)

      (pk, rec)
    }

  def applyUpdate(up: Update): F[Unit] =
    F.delay {
      xs(up.pk) = Record("", up.to)
      assertUniqueRanks()
    }

  /**
   * Not a part of the public API. Used to pre-seed storage with data. For testing only.
   */
  def withRow(id: K, record: Record[R]): this.type =
  {
    val row = (id, record)

    xs += row

    assertUniqueRanks()

    this
  }

  private def assertUniqueRanks(): Unit =
    assert(xs.values.map(_.rank).toSet.size == xs.size, "ranks are unique")

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