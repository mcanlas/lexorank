package com.htmlism.lexorank

import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

trait Arbitraries {
  implicit def posInt: Arbitrary[PosInt] =
    Arbitrary {
      PosInt {
        scala.util.Random.nextInt(Int.MaxValue - 1) + 1
      }
    }

  implicit def bimap[K : Arbitrary, V : Arbitrary]: Arbitrary[Bimap[K, V]] =
    Arbitrary {
      arbitrary[List[(K, V)]]
        .map(Bimap.fromList)
    }
}
