package com.htmlism.lexorank

import cats.implicits._
import cats.effect._

import org.scalatest._
import org.scalatest.prop._

class LexorankSpec
  extends FlatSpec
    with Matchers
    with Inside
    with GeneratorDrivenPropertyChecks
    with Arbitraries
    with Determinism {
  it should "domain error if pk exists in `after`" in {
    forAll { key: PosInt =>
      val ret =
        new LexorankFlow[IO, PosInt, PosInt](storage.ScalaCollectionStorage.empty, rgPosInt)
          .changePosition(key, After(key))
          .unsafeRunSync()

      ret shouldBe Left(IdWasInAfter)
    }
  }

  it should "domain error if pk exists in `before`" in {
    forAll { key: PosInt =>
      val ret =
        new LexorankFlow[IO, PosInt, PosInt](storage.ScalaCollectionStorage.empty, rgPosInt)
          .changePosition(key, Before(key))
          .unsafeRunSync()

      ret shouldBe Left(IdWasInBefore)
    }
  }

  it should "domain error if pk does not exist" in {
    forAll { (keyA: PosInt, keyB: PosInt) =>
      whenever (keyA != keyB) {
        val ret =
          new LexorankFlow[IO, PosInt, PosInt](storage.ScalaCollectionStorage.empty, rgPosInt)
            .changePosition(keyA, After(keyB))
            .unsafeRunSync()

        ret shouldBe Left(IdDoesNotExistInStorage)
      }
    }
  }

  "insertion anywhere" should "always be successful given an int-sized store" in {
    forAll { store: storage.ScalaCollectionStorage[IO, PosInt, PosInt] =>
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

    val store = storage.ScalaCollectionStorage.empty[IO, PosInt, UpToTen]
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
    forAll { store: storage.ScalaCollectionStorage[IO, PosInt, UpToTen] =>
      val previousSize = store.size
      val flow = new LexorankFlow(store, UpToTen.AlwaysSayMin)

      flow
        .insertAt("", Anywhere)
        .unsafeRunSync()

      store.size shouldBe previousSize + 1
    }
  }

  // TODO for any given state, property test that INSERT and CHANGE requests retain their properties
  // i.e. previous sort was maintained and requested sort is also satisified

  // TODO before behavior needs to be tested
  "a valid Insert Before request" should "increment size and retain order" in {
    forAll { pair: StorageAndRequest[IO, PosInt, PosInt, Before] =>
      val StorageAndRequest(a, b) = pair

      val flow = new LexorankFlow[IO, PosInt, PosInt](a, rgPosInt)

      val io =
        for {
          xs1 <- flow.getRows
           or <- flow.insertAt("", b)
          xs2 <- flow.getRows
        } yield {
          inside(or) {
            case Right((pk, rec)) =>
              (xs2 - pk).toList should contain theSameElementsInOrderAs xs1.toList
          }
        }

      io
        .unsafeRunSync()
    }
  }

  // TODO after behavior needs to be tested
  "a valid Insert After request" should "increment size and retain order" in {
    forAll { pair: StorageAndRequest[IO, PosInt, PosInt, After] =>
      val StorageAndRequest(a, b) = pair

      val flow = new LexorankFlow[IO, PosInt, PosInt](a, rgPosInt)

      val io =
        for {
          xs1 <- flow.getRows
          or <- flow.insertAt("", b)
          xs2 <- flow.getRows
        } yield {
          inside(or) {
            case Right((pk, rec)) =>
              (xs2 - pk).toList should contain theSameElementsInOrderAs xs1.toList
          }
        }

      io
        .unsafeRunSync()
    }
  }
}
