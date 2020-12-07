package com.htmlism.lexorank
package properties

import cats.effect._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import com.htmlism.lexorank.request._
import com.htmlism.lexorank.storage.inmemory.InMemoryStorage

class RequestValidationSpec
    extends AnyFlatSpec
    with Matchers
    with ScalaCheckDrivenPropertyChecks
    with LexorankArbitraries
    with Inside
    with Determinism {
  private[this] val tx = cats.arrow.FunctionK.id[IO]

  private[this] val emptyFlow =
    new LexorankFlow(tx, new InMemoryStorage[IO, PosInt, PosInt], rgPosInt)

  "invalid insert before requests" should "return a key error" in {
    forAll { req: Before[PosInt] =>
      val io =
        emptyFlow
          .insertAt("", req)
          .map { either =>
            inside(either) { case Left(err) =>
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
            inside(either) { case Left(err) =>
              err shouldBe errors.KeyNotInContext
            }
          }

      io.unsafeRunSync()
    }
  }
}
