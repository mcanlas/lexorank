package com.htmlism.lexorank
package properties

import cats.effect._

import org.scalatest._
import org.scalatest.prop._

import com.htmlism.lexorank.storage.InMemoryStorage

class RequestValidationSpec
    extends FlatSpec
    with Matchers
    with GeneratorDrivenPropertyChecks
    with LexorankArbitraries
    with Inside
    with Determinism {
  private val tx = cats.arrow.FunctionK.id[IO]

  private val emptyFlow =
    new LexorankFlow(tx, new InMemoryStorage[IO, PosInt, PosInt], rgPosInt)

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
