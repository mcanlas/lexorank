package com.htmlism.lexorank.storage

import cats._
import cats.implicits._
import cats.effect._

import doobie._
import doobie.implicits._
import doobie.h2._
import doobie.h2.implicits._
import doobie.util.fragment.Fragment

object Preload {
  private val jdbcUrl = "jdbc:h2:mem:"

  private val startUpSql = scala.io.Source
    .fromResource("schema.sql")
    .getLines()
    .mkString("\n")

  private def runStartUpSql[F[_]: Monad](tx: Transactor[F]): F[Transactor[F]] =
    Fragment.const0(startUpSql).update.run.transact(tx).as(tx)

  def within[F[_]: Async]: F[Transactor[F]] =
    H2Transactor.newH2Transactor[F](jdbcUrl, "", "") >>= runStartUpSql[F]

  def asStream[F[_]: Async]: fs2.Stream[F, doobie.Transactor[F]] =
    H2Transactor
      .stream[F](jdbcUrl, "", "")
      .evalMap(runStartUpSql[F])
}

object Scratch extends App {
  Preload
    .within[IO]
    .flatMap { x =>
      sql"SELECT * from rankable_entities".query[(Int, String, Int)].to[List].transact(x)
    }
    .map(println)
    .unsafeRunSync()

  Preload
    .asStream[IO]
    .evalMap { x =>
      sql"SELECT * from rankable_entities".query[(Int, String, Int)].to[List].transact(x)
    }
    .map(println)
    .compile
    .drain
    .unsafeRunSync()
}
