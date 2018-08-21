package com.htmlism.lexorank

import org.scalatest._
import org.scalatest.prop._

class LexorankSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks with Arbitraries {
  it should "domain error if pk exists in `after`" in {
    forAll { key: Int =>
      val ret =
        new Storage[Int, Int]
          .changePosition(key, AfterBefore.after(key))
          .value
          .unsafeRunSync()

      ret shouldBe Left(IdWasInAfter)
    }
  }

  it should "domain error if pk exists in `before`" in {
    forAll { key: Int =>
      val ret =
        new Storage[Int, Int]
          .changePosition(key, AfterBefore.before(key))
          .value
          .unsafeRunSync()

      ret shouldBe Left(IdWasInBefore)
    }
  }

  it should "domain error if pk does not exist" in {
    forAll { (keyA: Int, keyB: Int) =>
      whenever (keyA != keyB) {
        val ret =
          new Storage[Int, Int]
            .changePosition(keyA, AfterBefore.after(keyB))
            .value
            .unsafeRunSync()

        ret shouldBe Left(IdDoesNotExistInStorage)
      }
    }
  }

  "insertion" should "always be successful given an empty store" in {
    val store = new Storage[Int, Int]

    store
      .insertAt(None, None)
      .value
      .unsafeRunSync()

    store.size shouldBe 1
  }
}
