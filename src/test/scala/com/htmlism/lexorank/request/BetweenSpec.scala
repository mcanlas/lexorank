package com.htmlism.lexorank
package request

import cats.instances.int._

import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class BetweenSpec
    extends AnyFlatSpec
    with Matchers
    with Inside
    with OptionValues
    with EitherValues
    with ScalaCheckDrivenPropertyChecks {
  "valid pairs" should "construct properly" in {
    forAll { (x: Int, y: Int) =>
      whenever(x != y) {
        inside(Between(x, y)) {
          case Right(ab) =>
            ab.a shouldBe x
            ab.b shouldBe y
        }
      }
    }
  }

  "invalid pairs" should "fail properly" in {
    forAll { x: Int =>
      Between(x, x).left.value shouldBe errors.DuplicateBetweenKeys
    }
  }
}
