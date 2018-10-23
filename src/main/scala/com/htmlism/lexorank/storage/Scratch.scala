package com.htmlism.lexorank.storage

import cats._
import cats.implicits._
import cats.effect._
import mouse.any._

import doobie._
import doobie.implicits._
import doobie.util.fragment.Fragment

object Preload {
  private[this] val jdbcUrl     = "jdbc:h2:mem:"
  private[this] val sqlResource = "/schema.sql"

  private[this] val resource211 =
    scala.io.Source.fromInputStream(getClass.getResourceAsStream(sqlResource))

  // `Source.fromResource` only in scala 2.12
  private[this] val startUpSql = resource211.getLines.mkString("\n")

  private[this] def runStartUpSql[F[_]: Monad](tx: Transactor[F]): F[Transactor[F]] =
    Fragment.const0(startUpSql).update.run.transact(tx).as(tx)

  def unsafeBuildTxSync: doobie.Transactor[IO] = {
    val g = scala.concurrent.ExecutionContext.Implicits.global

    implicit val cs: ContextShift[IO] = IO.contextShift(g)

    org.h2.jdbcx.JdbcConnectionPool.create(jdbcUrl, "", "") |>
      (Transactor.fromDataSource[IO](_, g, g)) |>
      (runStartUpSql[IO](_)) |>
      (_.unsafeRunSync())
  }
}

object Scratch extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    sql"SELECT * from rankable_entities"
      .query[(Int, String, Int)]
      .to[List]
      .transact(Preload.unsafeBuildTxSync)
      .map(println)
      .as(ExitCode.Success)
}
