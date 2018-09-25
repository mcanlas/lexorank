package com.htmlism.lexorank

import cats.effect._

import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

trait LexorankArbitraries {
  implicit val upToTen: Arbitrary[UpToTen] =
    Arbitrary {
      Gen
        .choose(1, 10)
        .map(UpToTen.apply)
    }

  implicit val posInt: Arbitrary[PosInt] =
    Arbitrary {
      Gen
        .choose(1, Int.MaxValue)
        .map(PosInt.apply)
    }

  implicit def arbBefore[A: Arbitrary]: Arbitrary[Before[A]] =
    Arbitrary { arbitrary[A].map(Before.apply) }

  implicit def arbAfter[A: Arbitrary]: Arbitrary[After[A]] =
    Arbitrary { arbitrary[A].map(After.apply) }

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

  private def genInvalidInsert[F[_]: Sync,
                               K: KeyLike: Arbitrary,
                               R: Arbitrary] =
    for {
      s <- genNonEmptyStorage[F, K, R]
      k <- Gen.oneOf(s.dump.keys.toVector)
    } yield (s, k)

  implicit def arbInsertBefore[F[_]: Sync, K: KeyLike: Arbitrary, R: Arbitrary]
    : Arbitrary[StorageAndValidInsertRequest[F, K, R, Before]] =
    Arbitrary {
      genInvalidInsert[F, K, R]
        .map { case (s, k) => StorageAndValidInsertRequest(s, Before(k)) }
    }

  implicit def arbInsertAfter[F[_]: Sync, K: KeyLike: Arbitrary, R: Arbitrary]
    : Arbitrary[StorageAndValidInsertRequest[F, K, R, After]] =
    Arbitrary {
      genInvalidInsert[F, K, R]
        .map { case (s, k) => StorageAndValidInsertRequest(s, After(k)) }
    }

  implicit def arbChangeBefore[F[_]: Sync, K: KeyLike: Arbitrary, R: Arbitrary]
    : Arbitrary[StorageAndValidChangeRequest[F, K, R, Before]] =
    Arbitrary {
      val storageWithAtLeastTwo =
        Gen
          .nonEmptyMap(arbitrary[(R, String)])
          .filter(_.size > 1)
          .map(storage.ScalaCollectionStorage.from[F, K, R])

      for {
        s  <- storageWithAtLeastTwo
        k1 <- Gen.oneOf(s.dump.keys.toVector)
        k2 <- Gen.oneOf((s.dump.keys.toSet - k1).toVector)
      } yield {
        StorageAndValidChangeRequest(s, k1, Before(k2))
      }
    }

  implicit def arbChangeAfter[F[_]: Sync, K: KeyLike: Arbitrary, R: Arbitrary]
    : Arbitrary[StorageAndValidChangeRequest[F, K, R, After]] =
    Arbitrary {
      val storageWithAtLeastTwo =
        Gen
          .nonEmptyMap(arbitrary[(R, String)])
          .filter(_.size > 1)
          .map(storage.ScalaCollectionStorage.from[F, K, R])

      for {
        s  <- storageWithAtLeastTwo
        k1 <- Gen.oneOf(s.dump.keys.toVector)
        k2 <- Gen.oneOf((s.dump.keys.toSet - k1).toVector)
      } yield {
        StorageAndValidChangeRequest(s, k1, After(k2))
      }
    }
}
