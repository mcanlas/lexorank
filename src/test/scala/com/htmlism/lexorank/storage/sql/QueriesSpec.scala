package com.htmlism.lexorank
package storage
package sql

import cats.effect.IO

import doobie._
import doobie.scalatest._
import org.scalatest.freespec.AnyFreeSpec

class QueriesSpec[K, R] extends AnyFreeSpec with IOChecker {
  lazy val transactor: Transactor[IO] =
    Preload.unsafeBuildTxSync

  private[this] val q =
    new SqlQueries[PosInt, PosInt]

  private[this] val pk = PosInt(1)
  private[this] val r  = PosInt(1)

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
      check(SqlQueries.insert("", pk))
    }

    "update rank for last mile" in {
      check(SqlQueries.updateRankAfterCascade(pk, r))
    }

    "update rank in cascade" in {
      check(SqlQueries.updateRankInsideCascade(pk, r, r))
    }
  }
}
