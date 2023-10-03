package com.htmlism.lexorank
package request

import cats.instances.int.*
import org.scalatest.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ChangeRequestSpec
    extends AnyFlatSpec
    with Matchers
    with Inside
    with OptionValues
    with EitherValues
    with ScalaCheckDrivenPropertyChecks
    with LexorankArbitraries {
  "valid pairs" should "construct properly" in {
    forAll { (x: Int, req: PositionRequest[Int]) =>
      whenever(!req.keys.contains(x)) {
        inside(ChangeRequest(x, req)) { case Right(ch) =>
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
