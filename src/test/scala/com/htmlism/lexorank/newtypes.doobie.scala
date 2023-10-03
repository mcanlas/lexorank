package com.htmlism.lexorank

import cats.*
import cats.effect.*
import doobie.*

import com.htmlism.lexorank.request.*
import com.htmlism.lexorank.storage.sql.SqlStorage

case class H2StoreAndInsertRequest[K, R](tx: ConnectionIO ~> IO, store: SqlStorage[K, R], req: PositionRequest[K])

case class H2StoreAndChangeRequest[K, R](tx: ConnectionIO ~> IO, store: SqlStorage[K, R], req: ChangeRequest[K])
