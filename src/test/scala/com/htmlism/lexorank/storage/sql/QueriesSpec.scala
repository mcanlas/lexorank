package com.htmlism.lexorank
package storage
package sql

import cats.effect.IO

import doobie._
import doobie.scalatest._
import org.scalatest._

class QueriesSpec[K, R] extends FreeSpec with IOChecker {
  def transactor: Transactor[IO] =
    Preload
      .within[IO]
      .unsafeRunSync()

  private val q =
    new SqlQueries[PosInt, PosInt]

  private val pk = PosInt(1)
  private val r  = PosInt(1)

  "type safe queries in h2" - {
    "select one" in {
      check(q.selectOne(pk))
    }

    "select all rows" in {
      check(q.selectAllRows)
    }

    "select all rows for update" in {
      check(q.selectAllRowsForUpdate)
    }

    "insert" in {
      check(q.insert("", pk))
    }

    "update rank for last mile" in {
      check(q.updateRankAfterCascade(pk, r))
    }

    "update rank in cascade" in {
      check(q.updateRankInsideCascade(pk, r, r))
    }
  }
}
