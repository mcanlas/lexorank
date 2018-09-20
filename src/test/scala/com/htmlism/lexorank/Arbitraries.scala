package com.htmlism.lexorank

import cats.effect._
import mouse.all._

import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

trait Arbitraries {
  implicit def upToTen: Arbitrary[UpToTen] =
    Arbitrary {
      Gen.choose(1, 10)
        .map(UpToTen.apply)
    }

  implicit def posInt: Arbitrary[PosInt] =
    Arbitrary {
      Gen.choose(1, Int.MaxValue)
        .map(PosInt.apply)
    }

  implicit def bimap[K : Arbitrary, V : Arbitrary]: Arbitrary[Bimap[K, V]] =
    Arbitrary {
      arbitrary[List[(K, V)]]
        .map(Bimap.fromList)
    }

  implicit def arbStorage[K : Arbitrary : KeyLike, V : Arbitrary : Rankable]: Arbitrary[storage.ScalaCollectionStorage[IO, K, V]] =
    Arbitrary {
      arbitrary[Bimap[K, V]]
        .map(xs => buildStorage(xs)) // xs because scala?
    }

  private def buildStorage[K : KeyLike, V : Rankable](xs: Bimap[K, V]): storage.ScalaCollectionStorage[IO, K, V] =
    storage.ScalaCollectionStorage.from {
      xs
        .xs
        .map { case (k, v) => k -> Record("", v) }
    }

  private def genNonEmptyStorage[F[_] : Sync, K : Arbitrary : KeyLike, V : Arbitrary]: Gen[storage.ScalaCollectionStorage[F, K, V]] =
    Gen
      .nonEmptyListOf(arbitrary[(K, V)])
      .map(Bimap.fromList)
      .map(_.xs.map { case (k, v) => k -> Record("", v) })
      .map(storage.ScalaCollectionStorage.from[F, K, V])

  implicit def arbInsertBefore[F[_] : Sync, K : KeyLike : Arbitrary, R : Arbitrary]: Arbitrary[StorageAndRequest[F, K, R, Before]] =
    Arbitrary {
      for {
        s <- genNonEmptyStorage[F, K, R]
        k <- Gen.oneOf(s.dump.keys.toVector)
      } yield {
        StorageAndRequest(s, Before(k))
      }
    }

  implicit def arbInsertAfter[F[_] : Sync, K : KeyLike : Arbitrary, R : Arbitrary]: Arbitrary[StorageAndRequest[F, K, R, After]] =
    Arbitrary {
      for {
        s <- genNonEmptyStorage[F, K, R]
        k <- Gen.oneOf(s.dump.keys.toVector)
      } yield {
        StorageAndRequest(s, After(k))
      }
    }
//
//  protected def genInsertAfter[F, K, R](store: Storage[F, K, R]): Arbitrary[FlowAndRequest[F, K, R, After]] =
//
//
//  protected def genInsertBetween[F, K, R](store: Storage[F, K, R]): Arbitrary[FlowAndRequest[F, K, R, Between]] =
//
//
//  protected def genChangeReq
//
//  implicit def arbFlowAndRequest[F[_], K, R, PR[_]]: Arbitrary[FlowAndRequest[F, K, R, PR]] =
//    ???
}
