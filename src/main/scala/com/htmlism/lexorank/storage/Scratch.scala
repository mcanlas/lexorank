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
  private val startUpSql = scala.io.Source
    .fromResource("schema.sql")
    .getLines()
    .mkString("\n")

  private def runStartUpSql[F[_]: Monad](tx: Transactor[F]): F[Transactor[F]] =
    Fragment.const0(startUpSql).update.run.transact(tx).as(tx)

  def fresh[F[_]: Async]: F[Transactor[F]] =
    H2Transactor.newH2Transactor[F]("jdbc:h2:mem:", "", "") >>= runStartUpSql

  def withStream[F[_]: Async] =
    for {
      xa <- H2Transactor.stream[F]("jdbc:h2:mem:", "", "")
    } yield xa

  withStream[IO]
    .evalMap(runStartUpSql)

//  def withIo[A](f: doobie.util.transactor.Transactor[IO] => IO[A]) =
//    withStream[IO, A] { tx =>
//      f(tx)
//    }.compile.drain.unsafeRunSync
}

object Scratch extends App {
  val create = scala.io.Source
    .fromResource("schema.sql")
    .getLines()
    .mkString("\n")

  H2Transactor
    .stream[IO]("jdbc:h2:mem:", "", "")
    .compile
    .drain

  def partial[A[_]: Async, B] =
    for {
      xa <- H2Transactor.newH2Transactor[A]("jdbc:h2:mem:", "", "")
      a  <- sql"SELECT 3".query[Int].unique.transact(xa)
      _  <- xa.dispose
      n  <- Fragment.const0(create).update.run.transact(xa)
      xs <- sql"SELECT * from rankable_entities".query[(Int, String, Int)].to[List].transact(xa)
    } yield n

  partial[IO, Nothing]
    .map(println)
    .unsafeRunSync()
}
