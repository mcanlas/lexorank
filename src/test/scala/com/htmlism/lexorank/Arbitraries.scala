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
}
