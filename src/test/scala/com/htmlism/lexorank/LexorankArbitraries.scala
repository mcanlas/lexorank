package com.htmlism.lexorank

import cats._
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

  implicit def arbPositionRequest[A: Eq: Arbitrary]
    : Arbitrary[PositionRequest[A]] =
    Arbitrary {
      Gen.oneOf(arbitrary[Between[A]],
                arbitrary[Before[A]],
                arbitrary[After[A]])
    }

  implicit def arbChange[A: Eq: Arbitrary]: Arbitrary[ChangeRequest[A]] =
    Arbitrary {
      val xy =
        for {
          x   <- arbitrary[A]
          req <- arbitrary[PositionRequest[A]]
        } yield (x, req)

      xy.filter { case (x, req) => !req.keys.contains(x) }
        .map { case (x, req) => ChangeRequest(x, req).right.get }

    }

  implicit def arbBetween[A: Eq: Arbitrary]: Arbitrary[Between[A]] =
    Arbitrary {
      val xy =
        for {
          x <- arbitrary[A]
          y <- arbitrary[A]
        } yield (x, y)

      xy.filter { case (x, y) => x != y }
        .map((Between.apply[A] _).tupled)
        .map(_.right.get)
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

  private def genValidInsert[F[_]: Sync, K: KeyLike: Arbitrary, R: Arbitrary] =
    for {
      s <- genNonEmptyStorage[F, K, R]
      k <- Gen.oneOf(s.dump.keys.toVector)
    } yield (s, k)

  private def genInsertBefore[F[_]: Sync, K: KeyLike: Arbitrary, R: Arbitrary] =
    genValidInsert[F, K, R]
      .map { case (s, k) => StorageAndValidInsertRequest(s, Before(k)) }

  private def genInsertAfter[F[_]: Sync, K: KeyLike: Arbitrary, R: Arbitrary] =
    genValidInsert[F, K, R]
      .map { case (s, k) => StorageAndValidInsertRequest(s, After(k)) }

  implicit def arbInsertPair[F[_]: Sync, K: KeyLike: Arbitrary, R: Arbitrary]
    : Arbitrary[StorageAndValidInsertRequest[F, K, R]] =
    Arbitrary {
      Gen.oneOf(genInsertBefore[F, K, R], genInsertAfter[F, K, R]) // TODO insert between
    }

  implicit def arbChangePair[F[_]: Sync,
                             K: Eq: KeyLike: Arbitrary,
                             R: Arbitrary]
    : Arbitrary[StorageAndValidChangeRequest[F, K, R]] =
    Arbitrary {
      Gen.oneOf(genChangeBefore[F, K, R],
                genChangeAfter[F, K, R],
                genChangeBetween[F, K, R])
    }

  private def genChangeBefore[F[_]: Sync,
                              K: Eq: KeyLike: Arbitrary,
                              R: Arbitrary] =
    for {
      s  <- genStorageAtLeast[F, K, R](2)
      k1 <- Gen.oneOf(s.dump.keys.toVector)
      k2 <- Gen.oneOf((s.dump.keys.toSet - k1).toVector)
    } yield {
      StorageAndValidChangeRequest(s, ChangeRequest(k1, Before(k2)).right.get)
    }

  private def genChangeAfter[F[_]: Sync,
                             K: Eq: KeyLike: Arbitrary,
                             R: Arbitrary] =
    for {
      s  <- genStorageAtLeast[F, K, R](2)
      k1 <- Gen.oneOf(s.dump.keys.toVector)
      k2 <- Gen.oneOf((s.dump.keys.toSet - k1).toVector)
    } yield {
      StorageAndValidChangeRequest(s, ChangeRequest(k1, After(k2)).right.get)
    }

  private def genChangeBetween[F[_]: Sync,
                               K: Eq: KeyLike: Arbitrary,
                               R: Arbitrary] =
    for {
      s  <- genStorageAtLeast[F, K, R](3)
      k1 <- Gen.oneOf(s.dump.keys.toVector)
      k2 <- Gen.oneOf((s.dump.keys.toSet - k1).toVector)
      k3 <- Gen.oneOf((s.dump.keys.toSet - k1 - k2).toVector)
    } yield {
      StorageAndValidChangeRequest(
        s,
        ChangeRequest(k1, Between(k2, k3).right.get).right.get)
    }

  private def genStorageAtLeast[F[_]: Sync,
                                K: KeyLike: Arbitrary,
                                R: Arbitrary](n: Int) =
    Gen
      .nonEmptyMap(arbitrary[(R, String)])
      .filter(_.size >= n)
      .map(storage.ScalaCollectionStorage.from[F, K, R])
}
