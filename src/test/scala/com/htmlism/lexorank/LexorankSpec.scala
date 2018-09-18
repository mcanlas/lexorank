package com.htmlism.lexorank

import org.scalatest._
import org.scalatest.prop._

class LexorankSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks with Arbitraries with Determinism {
  it should "domain error if pk exists in `after`" in {
    forAll { key: PosInt =>
      val ret =
        new Storage[PosInt, PosInt](rgPosInt)
          .changePosition(key, After(key))
          .value
          .unsafeRunSync()

      ret shouldBe Left(IdWasInAfter)
    }
  }

  it should "domain error if pk exists in `before`" in {
    forAll { key: PosInt =>
      val ret =
        new Storage[PosInt, PosInt](rgPosInt)
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
          new Storage[PosInt, PosInt](rgPosInt)
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

  "insertion anywhere given always say min" should "always work up to the key space limit" in {
    val limit = 10

    val store = new Storage[PosInt, UpToTen](UpToTen.AlwaysSayMin)

    for (n <- 1 to limit) {
      println(n + ":")
      store
        .insertAt("", Anywhere)
        .value
        .unsafeRunSync()

      val sortedDump =
        store.dump.values.map(_.rank.n).toList.sorted

      val expected =
        (1 to n).toList

      sortedDump should contain theSameElementsInOrderAs expected

      println
      println
      println
    }

    store.size shouldBe limit
  }

  /**
   * Given the way the current strategy is implemented (using the midpoint to inform the strategy once at the top),
   * it is possible that there will be a crowded key space error even if the total key space isn't literally full.
   * Only a smarter strategy would be able to take advantage of the empty keys that the midpoint strategy is leaving
   * behind.
   */
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
