package com.htmlism.lexorank

/**
 * @param K Evidence for key behaviors over `K`
 * @param R Evidence for rank behaviors over `R`
 */
class ScalaCollectionStorage[K, R](implicit K: KeyLike[K], R: Rankable[R]) extends Storage[K, R] {
  private var pkSeed: K =
    K.first

  /**
   * This is bi-directional map between PKs and ranks.
   */
  private val xs =
    collection.mutable.Map.empty[K, Record[R]]
}