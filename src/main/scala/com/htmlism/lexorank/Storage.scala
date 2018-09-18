package com.htmlism.lexorank

/**
 * Storage interface, backed by a Scala collection or SQL engine (like H2).
 *
 * @tparam F An effect type
 * @tparam K The type for primary keys in this storage. Usually `Int`
 * @tparam R The type for ranking items relative to one another. Usually `Int` but could be something like `String`
 */
trait Storage[F[_], K, R]