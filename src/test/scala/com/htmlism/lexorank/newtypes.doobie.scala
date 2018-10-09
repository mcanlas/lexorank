package com.htmlism.lexorank

import cats._
import cats.effect._

import doobie._

import com.htmlism.lexorank.request._
import com.htmlism.lexorank.storage.sql.SqlStorage

case class H2StoreAndInsertRequest[K, R](tx: ConnectionIO ~> IO, store: SqlStorage[K, R], req: PositionRequest[K])

case class H2StoreAndChangeRequest[K, R](tx: ConnectionIO ~> IO, store: SqlStorage[K, R], req: ChangeRequest[K])
