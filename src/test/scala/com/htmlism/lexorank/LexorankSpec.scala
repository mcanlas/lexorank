package com.htmlism.lexorank

import org.scalatest._
import org.scalatest.prop._

class LexorankSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  it should "domain error if pk exists in `after`" in {
    forAll { key: Int =>
      val ret =
        new Storage[String]
          .changePosition(key, AfterBefore.after(key))
          .value
          .unsafeRunSync()

      ret shouldBe Left(IdWasInAfter)
    }
  }

  it should "domain error if pk exists in `before`" in {
    forAll { key: Int =>
      val ret =
        new Storage[String]
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
          new Storage[String]
            .changePosition(keyA, AfterBefore.after(keyB))
            .value
            .unsafeRunSync()

        ret shouldBe Left(IdDoesNotExistInStorage)
      }
    }
  }
}
