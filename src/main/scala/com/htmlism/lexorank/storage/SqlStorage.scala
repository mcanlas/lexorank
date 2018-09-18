package com.htmlism.lexorank
package storage

/**
 * @tparam F An effect type
 * @tparam K The type for primary keys in this storage. Usually `Int`
 * @tparam R The type for ranking items relative to one another. Usually `Int` but could be something like `String`
 */
trait SqlStorage[F[_], K, R] extends Storage[F, K, R]