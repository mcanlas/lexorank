package com.htmlism.lexorank

import cats.effect._

import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

trait Arbitraries {
  implicit def upToTen: Arbitrary[UpToTen] =
    Arbitrary {
      Gen
        .choose(1, 10)
        .map(UpToTen.apply)
    }

  implicit def posInt: Arbitrary[PosInt] =
    Arbitrary {
      Gen
        .choose(1, Int.MaxValue)
        .map(PosInt.apply)
    }

  implicit def arbStorage[K: Arbitrary: KeyLike, V: Arbitrary: Rankable]
    : Arbitrary[storage.ScalaCollectionStorage[IO, K, V]] =
    Arbitrary {
      genNonEmptyStorage[IO, K, V]
    }

  private def genNonEmptyStorage[F[_]: Sync,
                                 K: Arbitrary: KeyLike,
                                 V: Arbitrary]
    : Gen[storage.ScalaCollectionStorage[F, K, V]] =
    Gen
      .nonEmptyMap(arbitrary[(V, String)])
      .map(storage.ScalaCollectionStorage.from[F, K, V])

  implicit def arbInsertBefore[F[_]: Sync, K: KeyLike: Arbitrary, R: Arbitrary]
    : Arbitrary[StorageAndInsertRequest[F, K, R, Before]] =
    Arbitrary {
      for {
        s <- genNonEmptyStorage[F, K, R]
        k <- Gen.oneOf(s.dump.keys.toVector)
      } yield {
        StorageAndInsertRequest(s, Before(k))
      }
    }

  implicit def arbInsertAfter[F[_]: Sync, K: KeyLike: Arbitrary, R: Arbitrary]
    : Arbitrary[StorageAndInsertRequest[F, K, R, After]] =
    Arbitrary {
      for {
        s <- genNonEmptyStorage[F, K, R]
        k <- Gen.oneOf(s.dump.keys.toVector)
      } yield {
        StorageAndInsertRequest(s, After(k))
      }
    }

  implicit def arbChangeBefore[F[_]: Sync, K: KeyLike: Arbitrary, R: Arbitrary]
    : Arbitrary[StorageAndChangeRequest[F, K, R, Before]] =
    Arbitrary {
      val storageWithAtLeastTwo =
        Gen
          .nonEmptyMap(arbitrary[(R, String)])
          .filter(_.size > 1)
          .map(storage.ScalaCollectionStorage.from[F, K, R])

      for {
        s <- storageWithAtLeastTwo
        k1 <- Gen.oneOf(s.dump.keys.toVector)
        k2 <- Gen.oneOf((s.dump.keys.toSet - k1).toVector)
      } yield {
        StorageAndChangeRequest(s, k1, Before(k2))
      }
    }

  implicit def arbChangeAfter[F[_]: Sync, K: KeyLike: Arbitrary, R: Arbitrary]
    : Arbitrary[StorageAndChangeRequest[F, K, R, After]] =
    Arbitrary {
      val storageWithAtLeastTwo =
        Gen
          .nonEmptyMap(arbitrary[(R, String)])
          .filter(_.size > 1)
          .map(storage.ScalaCollectionStorage.from[F, K, R])

      for {
        s <- storageWithAtLeastTwo
        k1 <- Gen.oneOf(s.dump.keys.toVector)
        k2 <- Gen.oneOf((s.dump.keys.toSet - k1).toVector)
      } yield {
        StorageAndChangeRequest(s, k1, After(k2))
      }
    }
}
