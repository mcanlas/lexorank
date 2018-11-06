package com.htmlism.lexorank.storage

import cats._
import cats.implicits._
import cats.effect._
import mouse.any._

import doobie._
import doobie.implicits._
import doobie.util.fragment.Fragment

object Preload {
  private[this] val sqlResource = "/schema.sql"

  private[this] def dynamicJdbcUrl = {
    val randomName = scala.util.Random.alphanumeric.take(10).mkString

    s"jdbc:h2:mem:$randomName;DB_CLOSE_DELAY=-1"
  }

  private[this] val resource211 =
    scala.io.Source.fromInputStream(getClass.getResourceAsStream(sqlResource))

  // `Source.fromResource` only in scala 2.12
  private[this] val startUpSql = resource211.getLines.mkString("\n")

  private[this] def runStartUpSql[F[_]: Monad](tx: Transactor[F]): F[Transactor[F]] =
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

object Scratch2 extends IOApp {
  private[this] val tx =
    Transactor
      .fromDriverManager[IO]("org.h2.Driver", "jdbc:h2:mem:")

  private[this] def create =
    sql"create table foo (a int)".update.run

  private[this] def select =
    sql"SELECT * from foo"
      .query[Int]
      .to[List]

  def run(args: List[String]): IO[ExitCode] =
    (create *> select)
      .transact(tx)
      .map { x =>
        println(x); x
      }
      .as(ExitCode.Success)
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
