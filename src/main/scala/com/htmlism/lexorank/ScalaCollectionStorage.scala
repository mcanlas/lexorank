package com.htmlism.lexorank

import cats.effect._

/**
 * @param K Evidence for key behaviors over `K`
 * @param R Evidence for rank behaviors over `R`
 */
class ScalaCollectionStorage[F[_], K, R](implicit F: Sync[F], K: KeyLike[K], R: Rankable[R]) extends Storage[F, K, R] {
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
}