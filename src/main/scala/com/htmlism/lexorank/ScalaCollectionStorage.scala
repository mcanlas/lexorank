package com.htmlism.lexorank

import cats.implicits._
import cats.effect._

/**
 * @param K Evidence for key behaviors over `K`
 */
class ScalaCollectionStorage[F[_], K, R](implicit F: Sync[F], K: KeyLike[K]) extends Storage[F, K, R] {
  private var pkSeed: K =
    K.first

  /**
   * This is bi-directional map between PKs and ranks.
   */
  private val xs =
    collection.mutable.Map.empty[K, Record[R]]

  def lockSnapshot: F[Snapshot] =
    F.delay {
      xs
        .map(r => r._1 -> r._2.rank)
        .toMap
    }

  def makeSpaceAndInsert(payload: String)(e: (R, List[Update])): F[Row] = {
    val (rank, preReqUpdates) = e

    val updatesIO = preReqUpdates.traverse(applyUpdate)

    val appendIO =
      F.delay {
        val rec = Record(payload, rank)

        val pk = pkSeed
        pkSeed = K.increment(pkSeed)

        withRow(pk, rec)

        (pk, rec)
      }

    updatesIO *> appendIO
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