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
    with GeneratorDrivenPropertyChecks {
  "valid pairs" should "construct properly" ignore {
    forAll { (x: Int, y: Int) =>
      whenever(x != y) {
        inside(Between(x, y)) {
          case Right(ab) =>
            ab.after.value shouldBe x
            ab.before.value shouldBe y
        }
      }
    }
  }

  "invalid pairs" should "fail properly" ignore {
    forAll { x: Int =>
      Between(x, x).left.value shouldBe errors.DuplicateBetweenKeys
    }
  }
}
