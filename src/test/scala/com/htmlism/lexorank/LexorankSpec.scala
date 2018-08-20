package com.htmlism.lexorank

import org.scalatest._

class LexorankSpec extends FlatSpec with Matchers {
  it should "domain error if pk exists in `after`" in {
    val key = 1

    val ret =
      new Storage[String]
        .changePosition(key, AfterBefore.after(key))
        .value
        .unsafeRunSync()

    ret shouldBe Left(IdWasInAfter)
  }

  it should "domain error if pk exists in `before`" in {
    val key = 1

    val ret =
      new Storage[String]
        .changePosition(key, AfterBefore.before(key))
        .value
        .unsafeRunSync()

    ret shouldBe Left(IdWasInBefore)
  }

  it should "domain error if pk does not exist" in {
    val pk = 1
    val pos = 2

    val ret =
      new Storage[String]
        .changePosition(pk, AfterBefore.after(pos))
        .value
        .unsafeRunSync()

    ret shouldBe Left(IdDoesNotExistInStorage)
  }
}
