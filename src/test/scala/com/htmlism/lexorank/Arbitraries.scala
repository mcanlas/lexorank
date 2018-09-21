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

  implicit def arbStorage[K : Arbitrary : KeyLike, V : Arbitrary : Rankable]: Arbitrary[storage.ScalaCollectionStorage[IO, K, V]] =
    Arbitrary {
      genNonEmptyStorage[IO, K, V]
    }

  private def genNonEmptyStorage[F[_] : Sync, K : Arbitrary : KeyLike, V : Arbitrary]: Gen[storage.ScalaCollectionStorage[F, K, V]] =
    Gen
      .nonEmptyListOf(arbitrary[(V, String)])
      .map(_.toMap)
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
}
