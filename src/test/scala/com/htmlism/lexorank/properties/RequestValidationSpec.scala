package com.htmlism.lexorank
package properties

import cats.effect._

import org.scalatest._
import org.scalatest.prop._

class RequestValidationSpec
    extends FlatSpec
    with Matchers
    with GeneratorDrivenPropertyChecks
    with LexorankArbitraries
    with Inside
    with Determinism {
  private val emptyFlow =
    new LexorankFlow[IO, PosInt, PosInt](new storage.ScalaCollectionStorage[IO, PosInt, PosInt],
                                         rgPosInt)

  "invalid insert before requests" should "return a key error" in {
    forAll { req: Before[PosInt] =>
      val io =
        emptyFlow
          .insertAt("", req)
          .map { either =>
            inside(either) {
              case Left(err) =>
                err shouldBe errors.KeyNotInContext
            }
          }

      io.unsafeRunSync()
    }
  }

  "invalid insert after requests" should "return a key error" in {
    forAll { req: After[PosInt] =>
      val io =
        emptyFlow
          .insertAt("", req)
          .map { either =>
            inside(either) {
              case Left(err) =>
                err shouldBe errors.KeyNotInContext
            }
          }

      io.unsafeRunSync()
    }
  }
}
