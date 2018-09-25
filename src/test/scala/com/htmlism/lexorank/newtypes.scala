package com.htmlism.lexorank

/**
  * This represents a flow instance and valid position request for that instance. The expectation is that the storage
  * can satisfy the request without error.
  *
  * This is a newtype to differentiate between two completely arbitrary/random instances.
  *
  * @param store Lexorank storage
  * @param req A position request for that instance
  *
  * @tparam F A monadic effect type for storage
  * @tparam K A key type
  * @tparam R A rank type
  */
case class StorageAndValidInsertRequest[F[_], K, R, PR[_] <: PositionRequest[_]](
    store: storage.ScalaCollectionStorage[F, K, R],
    req: PR[K])

/**
  * This represents a flow instance and valid position request for that instance. The expectation is that the storage
  * can satisfy the request without error.
  *
  * This is a newtype to differentiate between two completely arbitrary/random instances.
  *
  * @param store Lexorank storage
  * @param pk The identifier for the row being changed
  * @param req A position request for that instance
  *
  * @tparam F A monadic effect type for storage
  * @tparam K A key type
  * @tparam R A rank type
  */
case class StorageAndValidChangeRequest[F[_], K, R, PR[_] <: PositionRequest[_]](
    store: storage.ScalaCollectionStorage[F, K, R],
    pk: K,
    req: PR[K])
