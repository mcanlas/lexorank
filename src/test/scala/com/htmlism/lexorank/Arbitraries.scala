package com.htmlism.lexorank

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

  implicit def storage[K : Arbitrary : KeyLike, V : Arbitrary : Rankable](implicit RG: RankGenerator[V]): Arbitrary[LexorankFlow[K, V]] =
    Arbitrary {
      arbitrary[Bimap[K, V]]
        .map(xs => buildStorage(xs)) // xs because scala?
    }

  private def buildStorage[K : KeyLike, V : Rankable](xs: Bimap[K, V])(implicit RG: RankGenerator[V]): LexorankFlow[K, V] =
    xs.xs.foldLeft(new LexorankFlow[K, V](RG))((st, kv) => st.withRow(kv._1, Record("", kv._2)))
}
