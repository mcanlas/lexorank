package com.htmlism.lexorank.storage

import cats.effect._
import cats.implicits._
import doobie._
import doobie.implicits._
import doobie.util.fragment.Fragment
import mouse.any._

object Preload {
  type BracketThrowable[F[_]] = Bracket[F, Throwable]

  private[this] val sqlResource = "schema.sql"

  private[this] def dynamicJdbcUrl = {
    val randomName = scala.util.Random.alphanumeric.take(10).mkString

    s"jdbc:h2:mem:$randomName;DB_CLOSE_DELAY=-1"
  }

  private[this] val startUpSql = ResourceLoader.load(sqlResource).getLines().mkString("\n")

  private[this] def runStartUpSql[F[_]: BracketThrowable](tx: Transactor[F]): F[Transactor[F]] =
    Fragment
      .const0(startUpSql)
      .update
      .run
      .transact(tx)
      .as(tx)

  private[this] def connect = {
    implicit val cs: ContextShift[IO] =
      IO.contextShift(scala.concurrent.ExecutionContext.Implicits.global)

    Transactor
      .fromDriverManager[IO]("org.h2.Driver", dynamicJdbcUrl)
  }

  def unsafeBuildTxSync: doobie.Transactor[IO] =
    connect |>
      runStartUpSql[IO] |>
      (_.unsafeRunSync())
}
