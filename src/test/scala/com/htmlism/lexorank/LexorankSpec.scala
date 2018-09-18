package com.htmlism.lexorank

import cats.effect._

import org.scalatest._
import org.scalatest.prop._

class LexorankSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks with Arbitraries with Determinism {
  it should "domain error if pk exists in `after`" in {
    forAll { key: PosInt =>
      val ret =
        new LexorankFlow[IO, PosInt, PosInt](new ScalaCollectionStorage, rgPosInt)
          .changePosition(key, After(key))
          .unsafeRunSync()

      ret shouldBe Left(IdWasInAfter)
    }
  }

  it should "domain error if pk exists in `before`" in {
    forAll { key: PosInt =>
      val ret =
        new LexorankFlow[IO, PosInt, PosInt](new ScalaCollectionStorage, rgPosInt)
          .changePosition(key, Before(key))
          .unsafeRunSync()

      ret shouldBe Left(IdWasInBefore)
    }
  }

  it should "domain error if pk does not exist" in {
    forAll { (keyA: PosInt, keyB: PosInt) =>
      whenever (keyA != keyB) {
        val ret =
          new LexorankFlow[IO, PosInt, PosInt](new ScalaCollectionStorage, rgPosInt)
            .changePosition(keyA, After(keyB))
            .unsafeRunSync()

        ret shouldBe Left(IdDoesNotExistInStorage)
      }
    }
  }

  "insertion anywhere" should "always be successful given an int-sized store" in {
    forAll { store: ScalaCollectionStorage[IO, PosInt, PosInt] =>
      val previousSize = store.size
      val flow = new LexorankFlow(store, rgPosInt)

      flow
        .insertAt("", Anywhere)
        .unsafeRunSync()

      store.size shouldBe previousSize + 1
    }
  }

  "insertion anywhere given always say min" should "always work up to the key space limit" in {
    val limit = 10

    val store = new ScalaCollectionStorage[IO, PosInt, UpToTen]
    val flow = new LexorankFlow(store, UpToTen.AlwaysSayMin)

    for (n <- 1 to limit) {
      println(n + ":")
      flow
        .insertAt("", Anywhere)
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
    forAll { store: ScalaCollectionStorage[IO, PosInt, UpToTen] =>
      val previousSize = store.size
      val flow = new LexorankFlow(store, UpToTen.AlwaysSayMin)

      flow
        .insertAt("", Anywhere)
        .unsafeRunSync()

      store.size shouldBe previousSize + 1
    }
  }
}
