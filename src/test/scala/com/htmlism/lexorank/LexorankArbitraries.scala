package com.htmlism.lexorank

import cats._
import cats.effect._

import com.htmlism.lexorank.ranking.Rankable
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

import com.htmlism.lexorank.request._
import com.htmlism.lexorank.storage.inmemory._

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

  implicit def arbPositionRequest[A: Eq: Arbitrary]: Arbitrary[PositionRequest[A]] =
    Arbitrary {
      Gen.oneOf(arbitrary[Between[A]], arbitrary[Before[A]], arbitrary[After[A]])
    }

  implicit def arbChange[A: Eq: Arbitrary]: Arbitrary[ChangeRequest[A]] =
    Arbitrary {
      val xy =
        for {
          x   <- arbitrary[A]
          req <- arbitrary[PositionRequest[A]]
        } yield (x, req)

      xy.filter { case (x, req) => !req.keys.contains(x) }
        .map {
          case (x, req) =>
            ChangeRequest(x, req).getOrElse(throw new UnsupportedOperationException("expeced valid Between"))
        }

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
        .map(_.getOrElse(throw new UnsupportedOperationException("expeced valid Between")))
    }

  implicit def arbBefore[A: Arbitrary]: Arbitrary[Before[A]] =
    Arbitrary { arbitrary[A].map(Before.apply) }

  implicit def arbAfter[A: Arbitrary]: Arbitrary[After[A]] =
    Arbitrary { arbitrary[A].map(After.apply) }

  implicit def arbStorage[K: Arbitrary: KeyLike, V: Arbitrary: Rankable]: Arbitrary[InMemoryStorage[IO, K, V]] =
    Arbitrary {
      genNonEmptyStorage[IO, K, V]
    }

  private[this] def genNonEmptyStorage[F[_]: Sync, K: KeyLike, V: Arbitrary]: Gen[InMemoryStorage[F, K, V]] =
    Gen
      .nonEmptyMap(arbitrary[(V, String)])
      .map(InMemoryStorage.from[F, K, V])

  private[this] def genInsertBefore[F[_]: Sync, K: KeyLike, R: Arbitrary] =
    for {
      s <- genNonEmptyStorage[F, K, R]
      k <- Gen.oneOf(s.dump.keys.toVector)
    } yield InMemStoreAndInsertRequest(s, Before(k))

  private[this] def genInsertAfter[F[_]: Sync, K: KeyLike, R: Arbitrary] =
    for {
      s <- genNonEmptyStorage[F, K, R]
      k <- Gen.oneOf(s.dump.keys.toVector)
    } yield InMemStoreAndInsertRequest(s, After(k))

  private[this] def genInsertBetween[F[_]: Sync, K: Eq: KeyLike, R: Arbitrary] =
    for {
      s  <- genStorageAtLeast[F, K, R](2)
      k1 <- Gen.oneOf(s.dump.keys.toVector)
      k2 <- Gen.oneOf((s.dump.keys.toSet - k1).toVector)
    } yield InMemStoreAndInsertRequest(
      s,
      Between(k1, k2).getOrElse(throw new UnsupportedOperationException("expeced valid Between"))
    )

  implicit def arbInsertPair[F[_]: Sync, K: Eq: KeyLike, R: Arbitrary]: Arbitrary[InMemStoreAndInsertRequest[F, K, R]] =
    Arbitrary {
      Gen.oneOf(genInsertBefore[F, K, R], genInsertAfter[F, K, R], genInsertBetween[F, K, R])
    }

  implicit def arbChangePair[F[_]: Sync, K: Eq: KeyLike, R: Arbitrary]: Arbitrary[InMemStoreAndChangeRequest[F, K, R]] =
    Arbitrary {
      Gen.oneOf(genChangeBefore[F, K, R], genChangeAfter[F, K, R], genChangeBetween[F, K, R])
    }

  private[this] def genChangeBefore[F[_]: Sync, K: Eq: KeyLike, R: Arbitrary] =
    for {
      s  <- genStorageAtLeast[F, K, R](2)
      k1 <- Gen.oneOf(s.dump.keys.toVector)
      k2 <- Gen.oneOf((s.dump.keys.toSet - k1).toVector)
    } yield {
      InMemStoreAndChangeRequest(
        s,
        ChangeRequest(k1, Before(k2)).getOrElse(throw new UnsupportedOperationException("expeced valid Between"))
      )
    }

  private[this] def genChangeAfter[F[_]: Sync, K: Eq: KeyLike, R: Arbitrary] =
    for {
      s  <- genStorageAtLeast[F, K, R](2)
      k1 <- Gen.oneOf(s.dump.keys.toVector)
      k2 <- Gen.oneOf((s.dump.keys.toSet - k1).toVector)
    } yield {
      InMemStoreAndChangeRequest(
        s,
        ChangeRequest(k1, After(k2)).getOrElse(throw new UnsupportedOperationException("expeced valid Between"))
      )
    }

  private[this] def genChangeBetween[F[_]: Sync, K: Eq: KeyLike, R: Arbitrary] =
    for {
      s  <- genStorageAtLeast[F, K, R](3)
      k1 <- Gen.oneOf(s.dump.keys.toVector)
      k2 <- Gen.oneOf((s.dump.keys.toSet - k1).toVector)
      k3 <- Gen.oneOf((s.dump.keys.toSet - k1 - k2).toVector)
    } yield {
      InMemStoreAndChangeRequest(
        s,
        ChangeRequest(k1, Between(k2, k3).getOrElse(throw new UnsupportedOperationException("expeced valid Between")))
          .getOrElse(throw new UnsupportedOperationException("expeced valid Between"))
      )
    }

  private[this] def genStorageAtLeast[F[_]: Sync, K: KeyLike, R: Arbitrary](n: Int) =
    Gen
      .nonEmptyMap(arbitrary[(R, String)])
      .filter(_.size >= n)
      .map(InMemoryStorage.from[F, K, R])
}
