package com.htmlism.lexorank

import cats.implicits._
import cats.effect._

import org.scalatest._
import org.scalatest.prop._

import com.htmlism.lexorank.request._
import com.htmlism.lexorank.storage.InMemoryStorage

class LexorankSpec
    extends FlatSpec
    with Matchers
    with Inside
    with GeneratorDrivenPropertyChecks
    with LexorankArbitraries
    with Determinism {
  private val tx = cats.arrow.FunctionK.id[IO]

  "insertion anywhere" should "always be successful given an int-sized store" in {
    forAll { store: InMemoryStorage[IO, PosInt, PosInt] =>
      val previousSize = store.size
      val flow         = new LexorankFlow(tx, store, rgPosInt)

      flow
        .insertAt("", Anywhere)
        .unsafeRunSync()

      store.size shouldBe previousSize + 1
    }
  }

  "insertion anywhere given always say min" should "always work up to the key space limit" in {
    val limit = 10

    val store = InMemoryStorage.empty[IO, PosInt, UpToTen]
    val flow  = new LexorankFlow(tx, store, UpToTen.AlwaysSayMin)

    for (n <- 1 to limit) {
      println(n + ":")
      flow
        .insertAt("", Anywhere)
        .unsafeRunSync()

      val sortedDump =
        store.dump.values.map(_.rank.n).toList.sorted

      val expected =
        (1 to n).toList

      sortedDump should contain theSameElementsInOrderAs expected

      println
      println
      println
    }

    store.size shouldBe limit
  }

  /**
    * Given the way the current strategy is implemented (using the midpoint to inform the strategy once at the top),
    * it is possible that there will be a crowded key space error even if the total key space isn't literally full.
    * Only a smarter strategy would be able to take advantage of the empty keys that the midpoint strategy is leaving
    * behind.
    */
  "insertion anywhere" should "error given a crowded key space" ignore {
    forAll { store: InMemoryStorage[IO, PosInt, UpToTen] =>
      val previousSize = store.size
      val flow         = new LexorankFlow(tx, store, UpToTen.AlwaysSayMin)

      flow
        .insertAt("", Anywhere)
        .unsafeRunSync()

      store.size shouldBe previousSize + 1
    }
  }

  // TODO for any given state, property test that INSERT and CHANGE requests retain their properties
  // i.e. previous sort was maintained and requested sort is also satisified

  "a valid Insert request" should "increment size; reflect requested order; retain old order" in {
    forAll { (pair: StorageAndValidInsertRequest[IO, PosInt, PosInt], s: String) =>
      val StorageAndValidInsertRequest(store, req) = pair

      val flow = new LexorankFlow(tx, store, rgPosInt)

      val io =
        for {
          xs1 <- flow.getRows
          or  <- flow.insertAt(s, req)
          xs2 <- flow.getRows
        } yield {
          inside(or) {
            case Right((newPk, rec)) =>
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
    forAll { duo: StorageAndValidChangeRequest[IO, PosInt, PosInt] =>
      val StorageAndValidChangeRequest(store, chReq) = duo

      val flow = new LexorankFlow(tx, store, rgPosInt)

      val io =
        for {
          xs1 <- flow.getRows
          or  <- flow.changePosition(chReq)
          xs2 <- flow.getRows
        } yield {
          inside(or) {
            case Right((echoPk, _)) =>
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
