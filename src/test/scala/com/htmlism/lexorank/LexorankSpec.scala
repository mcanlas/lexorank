package com.htmlism.lexorank

import org.scalatest._
import org.scalatest.prop._

class LexorankSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks with Arbitraries with Determinism {
  it should "domain error if pk exists in `after`" in {
    forAll { key: PosInt =>
      val ret =
        new Storage[PosInt, PosInt]
          .changePosition(key, After(key))
          .value
          .unsafeRunSync()

      ret shouldBe Left(IdWasInAfter)
    }
  }

  it should "domain error if pk exists in `before`" in {
    forAll { key: PosInt =>
      val ret =
        new Storage[PosInt, PosInt]
          .changePosition(key, Before(key))
          .value
          .unsafeRunSync()

      ret shouldBe Left(IdWasInBefore)
    }
  }

  it should "domain error if pk does not exist" in {
    forAll { (keyA: PosInt, keyB: PosInt) =>
      whenever (keyA != keyB) {
        val ret =
          new Storage[PosInt, PosInt]
            .changePosition(keyA, After(keyB))
            .value
            .unsafeRunSync()

        ret shouldBe Left(IdDoesNotExistInStorage)
      }
    }
  }

  "insertion anywhere" should "always be successful given an int-sized store" in {
    forAll { store: Storage[PosInt, PosInt] =>
      val previousSize = store.size

      store
        .insertAt("", Anywhere)
        .value
        .unsafeRunSync()

      store.size shouldBe previousSize + 1
    }
  }

  "insertion anywhere" should "work up to the key space limit" ignore {
    val limit = 10

    val store = new Storage[PosInt, UpToTen]

    for (_ <- 1 to limit) {
      store
        .insertAt("", Anywhere)
        .value
        .unsafeRunSync()

      println
      println
      println
    }

    store.size shouldBe limit
  }

  "insertion anywhere" should "error given a crowded key space" ignore {
    forAll { store: Storage[PosInt, UpToTen] =>
      val previousSize = store.size

      store
        .insertAt("", Anywhere)
        .value
        .unsafeRunSync()

      store.size shouldBe previousSize + 1
    }
  }
}
