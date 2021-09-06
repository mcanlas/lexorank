package com.htmlism.lexorank

import com.htmlism.lexorank.request._
import com.htmlism.lexorank.storage.inmemory.InMemoryStorage

/**
  * This represents a flow instance and valid position request for that instance. The expectation is that the storage
  * can satisfy the request without error.
  *
  * This is a newtype to differentiate between two completely arbitrary/random instances.
  *
  * @param store
  *   Lexorank storage
  * @param req
  *   A position request for that instance
  *
  * @tparam F
  *   A monadic effect type for storage
  * @tparam K
  *   A key type
  * @tparam R
  *   A rank type
  */
case class InMemStoreAndInsertRequest[F[_], K, R](store: InMemoryStorage[F, K, R], req: PositionRequest[K])

/**
  * This represents a flow instance and valid position request for that instance. The expectation is that the storage
  * can satisfy the request without error.
  *
  * This is a newtype to differentiate between two completely arbitrary/random instances.
  *
  * @param store
  *   Lexorank storage
  * @param pk
  *   The identifier for the row being changed
  * @param req
  *   A position request for that instance
  *
  * @tparam F
  *   A monadic effect type for storage
  * @tparam K
  *   A key type
  * @tparam R
  *   A rank type
  */
case class InMemStoreAndChangeRequest[F[_], K, R](store: InMemoryStorage[F, K, R], req: ChangeRequest[K])
