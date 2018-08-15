package com.htmlism.lexorank

import org.scalatest._

class LexorankSpec extends FlatSpec with Matchers {
  it should "error if pk exists in `after`" in {
    Lexorank.changePosition(1, after = Some(1), before = None) shouldBe None
  }

  it should "error if pk exists in `before`" in {
    Lexorank.changePosition(1, after = None, before = Some(1)) shouldBe None
  }
}
