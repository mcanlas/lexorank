package com.htmlism.lexorank

import doobie.implicits._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import com.htmlism.lexorank.request._

class LexorankH2Spec
    extends AnyFlatSpec
    with Matchers
    with Inside
    with H2Arbitraries
    with ScalaCheckDrivenPropertyChecks
    with Determinism {

  "a valid Insert request" should "increment size; reflect requested order; retain old order" in {
    forAll { (trio: H2StoreAndInsertRequest[PosInt, PosInt], s: String) =>
      val H2StoreAndInsertRequest(tx, store, req) = trio

      val flow = new LexorankFlow(tx, store, rgPosInt)

      val io =
        for {
          xs1 <- flow.getRows
          or  <- flow.insertAt(s, req)
          xs2 <- flow.getRows
        } yield {
          inside(or) { case Right((newPk, rec)) =>
            xs2 diff List(newPk) should contain theSameElementsInOrderAs xs1

            req match {
              case Before(k) =>
                assert(xs2.indexOf(newPk) < xs2.indexOf(k), s"new pk $newPk comes before requested pk $k")

              case After(k) =>
                assert(xs2.indexOf(newPk) > xs2.indexOf(k), s"new pk $newPk comes after requested pk $k")

              case Between(x, y) =>
                val xFirst = xs2.indexOf(newPk) > xs2.indexOf(x) && xs2.indexOf(newPk) < xs2
                  .indexOf(y)
                val yFirst = xs2.indexOf(newPk) > xs2.indexOf(y) && xs2.indexOf(newPk) < xs2
                  .indexOf(x)

                assert(xFirst || yFirst, s"new pk $newPk is somewhere between $x and $y, unordered")

              case Anywhere =>
            }

            rec.name shouldBe s
          }
        }

      io.unsafeRunSync()
    }
  }

  "a valid Change request" should "maintain size; reflect requested order; retain old order" in {
    forAll { trio: H2StoreAndChangeRequest[PosInt, PosInt] =>
      val H2StoreAndChangeRequest(tx, store, chReq) = trio

      val flow = new LexorankFlow(tx, store, rgPosInt)

      val io =
        for {
          xs1 <- flow.getRows
          or  <- flow.changePosition(chReq)
          xs2 <- flow.getRows
        } yield {
          inside(or) { case Right((echoPk, _)) =>
            chReq.id shouldBe echoPk

            xs1 diff List(chReq.id) should contain theSameElementsInOrderAs (xs2 diff List(chReq.id))

            chReq.req match {
              case Before(k) =>
                assert(xs2.indexOf(echoPk) < xs2.indexOf(k), s"pk $echoPk comes before requested pk $k")

              case After(k) =>
                assert(xs2.indexOf(echoPk) > xs2.indexOf(k), s"pk $echoPk comes after requested pk $k")

              case Between(x, y) =>
                val xFirst = xs2.indexOf(echoPk) > xs2.indexOf(x) && xs2.indexOf(echoPk) < xs2
                  .indexOf(y)
                val yFirst = xs2.indexOf(echoPk) > xs2.indexOf(y) && xs2.indexOf(echoPk) < xs2
                  .indexOf(x)

                assert(xFirst || yFirst, s"pk $echoPk is somewhere between $x and $y, unordered")

              case Anywhere =>
            }
          }
        }

      io.unsafeRunSync()
    }
  }
}
