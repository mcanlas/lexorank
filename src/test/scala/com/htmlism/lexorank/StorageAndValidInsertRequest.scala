package com.htmlism.lexorank

/**
  * This represents a flow instance and valid position request for that instance. The expectation is that the storage
  * can satisfy the request without error.
  *
  * This is a newtype to differentiate between two completely arbitrary/random instances.
  *
  * @param flow A Lexorank instance
  * @param req A position request for that instance
  * @tparam F A monadic effect type
  * @tparam K A key type
  * @tparam R A rank type
  */
case class StorageAndValidInsertRequest[F[_], K, R, PR[_] <: PositionRequest[_]](
    flow: storage.ScalaCollectionStorage[F, K, R],
    req: PR[K])
