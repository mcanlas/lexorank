package com.htmlism.lexorank
package storage
package sql

import cats.effect.IO

import doobie._
import doobie.implicits._
import doobie.scalatest._
import org.scalatest.FreeSpec

class QueriesSpec[K, R] extends FreeSpec with IOChecker {
  def transactor: Transactor[IO] =
    Preload
      .within[IO]
      .unsafeRunSync()
}
