package com.htmlism.lexorank
package storage
package sql

import cats.effect.IO

import doobie._
import doobie.implicits._
import doobie.scalatest._
import org.scalatest._

class QueriesSpec[K, R] extends FreeSpec with IOChecker {
  def transactor: Transactor[IO] =
    Preload
      .within[IO]
      .unsafeRunSync()

  private val q =
    new Queries[PosInt, PosInt]

  "type safe queries in h2" - {
    "select one" in {
      check(q.selectOne(PosInt(4)))
    }

    "select all rows" in {
      check(q.selectAllRows)
    }

    "select all rows for update" in {
      check(q.selectAllRowsForUpdate)
    }

    "insert" in {
      check(q.insert("", PosInt(4)))
    }

    "update rank for last mile" in {
      check(q.updateRankAfterCascade(PosInt(4), PosInt(5)))
    }

    "update rank in cascade" in {
      check(q.updateRankInsideCascade(PosInt(5), PosInt(4), PosInt(5)))
    }
  }
}
