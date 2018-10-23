package com.htmlism.lexorank

import cats.effect._

import doobie._
import doobie.implicits._

import com.htmlism.lexorank.request._
import com.htmlism.lexorank.storage.Preload
import com.htmlism.lexorank.storage.sql._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck._

trait H2Arbitraries {
  implicit val posInt: Arbitrary[PosInt] =
    Arbitrary {
      Gen
        .choose(1, Int.MaxValue)
        .map(PosInt.apply)
    }

  private[this] def genInsertBefore[R: Arbitrary: Meta] =
    for {
      (tx, s) <- genStorageAtLeast[R](1)
      k       <- Gen.oneOf(keys(tx, s).toVector)
    } yield H2StoreAndInsertRequest(tx.trans, s, Before(k))

  private[this] def genInsertAfter[R: Arbitrary: Meta] =
    for {
      (tx, s) <- genStorageAtLeast[R](1)
      k       <- Gen.oneOf(keys(tx, s).toVector)
    } yield H2StoreAndInsertRequest(tx.trans, s, After(k))

  private[this] def genInsertBetween[R: Arbitrary: Meta] =
    for {
      (tx, s) <- genStorageAtLeast[R](2)
      k1      <- Gen.oneOf(keys(tx, s).toVector)
      k2      <- Gen.oneOf((keys(tx, s) - k1).toVector)
    } yield H2StoreAndInsertRequest(tx.trans, s, Between(k1, k2).right.get)

  implicit def arbInsertPair[R: Arbitrary: Meta]: Arbitrary[H2StoreAndInsertRequest[PosInt, R]] =
    Arbitrary {
      Gen.oneOf(genInsertBefore[R], genInsertAfter[R], genInsertBetween[R])
    }

  implicit def arbChangePair[R: Arbitrary: Meta]: Arbitrary[H2StoreAndChangeRequest[PosInt, R]] =
    Arbitrary {
      Gen.oneOf(genChangeBefore[R], genChangeAfter[R], genChangeBetween[R])
    }

  private[this] def genChangeBefore[R: Arbitrary: Meta] =
    for {
      (tx, s) <- genStorageAtLeast[R](2)
      k1      <- Gen.oneOf(keys(tx, s).toVector)
      k2      <- Gen.oneOf((keys(tx, s) - k1).toVector)
    } yield {
      H2StoreAndChangeRequest(tx.trans, s, ChangeRequest(k1, Before(k2)).right.get)
    }

  private[this] def genChangeAfter[R: Arbitrary: Meta] =
    for {
      (tx, s) <- genStorageAtLeast[R](2)
      k1      <- Gen.oneOf(keys(tx, s).toVector)
      k2      <- Gen.oneOf((keys(tx, s) - k1).toVector)
    } yield {
      H2StoreAndChangeRequest(tx.trans, s, ChangeRequest(k1, After(k2)).right.get)
    }

  private[this] def genChangeBetween[R: Arbitrary: Meta] =
    for {
      (tx, s) <- genStorageAtLeast[R](3)
      k1      <- Gen.oneOf(keys(tx, s).toVector)
      k2      <- Gen.oneOf((keys(tx, s) - k1).toVector)
      k3      <- Gen.oneOf((keys(tx, s) - k1 - k2).toVector)
    } yield {
      H2StoreAndChangeRequest(tx.trans, s, ChangeRequest(k1, Between(k2, k3).right.get).right.get)
    }

  private[this] def genStorageAtLeast[R: Arbitrary: Meta](n: Int) =
    Gen
      .nonEmptyMap(arbitrary[(R, String)])
      .filter(_.size >= n)
      .map { xs =>
        val tx = Preload.within[IO].unsafeRunSync()

        val store = new SqlStorage[PosInt, R]

        xs.foreach {
          case (r, s) =>
            store.insertNewRecord(s, r).transact(tx).unsafeRunSync()
        }

        tx -> store
      }

  private[this] def keys[R](tx: Transactor[IO], store: SqlStorage[PosInt, R]) =
    store.getSnapshot.transact(tx).unsafeRunSync().keySet
}
