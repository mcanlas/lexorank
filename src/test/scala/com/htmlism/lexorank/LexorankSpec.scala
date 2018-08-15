package com.htmlism.lexorank

import org.scalatest._

class LexorankSpec extends FlatSpec with Matchers {
  it should "domain error if pk exists in `after`" in {
    val ret =
      new Storage[String]
        .changePosition(1, after = Some(1), before = None)
        .unsafeRunSync()

    ret shouldBe Left(IdWasInAfter)
  }

  it should "domain error if pk exists in `before`" in {
    val ret =
      new Storage[String]
        .changePosition(1, after = None, before = Some(1))
        .unsafeRunSync()

    ret shouldBe Left(IdWasInBefore)
  }

  it should "domain error if pk does not exist" in {
    val ret =
      new Storage[String]
        .changePosition(1, after = None, before = None)
        .unsafeRunSync()

    ret shouldBe Left(IdDoesNotExistInStorage)
  }
}
