package com.htmlism.lexorank

import cats.instances.int._

import org.scalatest._
import org.scalatest.prop._

class ChangeRequestSpec
    extends FlatSpec
    with Matchers
    with Inside
    with OptionValues
    with EitherValues
    with GeneratorDrivenPropertyChecks
    with LexorankArbitraries {
  "valid pairs" should "construct properly" in {
    forAll { (x: Int, req: PositionRequest[Int]) =>
      whenever(!req.keys.contains(x)) {
        inside(ChangeRequest(x, req)) {
          case Right(ch) =>
            ch.id shouldBe x
            ch.req shouldBe req
        }
      }
    }
  }

  "invalid pairs" should "fail properly" in {
    forAll { req: PositionRequest[Int] =>
      ChangeRequest(req.keys.head, req).left.value shouldBe errors.DuplicateChangeKeys
    }
  }
}
