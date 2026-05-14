package com.htmlism.lexorank
package storage.inmemory

import cats.effect.*
import java.util.concurrent.atomic.AtomicReference
import scala.annotation.tailrec

object InMemoryStorage {

  /**
    * Factory from nothing.
    *
    * @tparam F
    *   An effect type
    * @tparam K
    *   A key type
    * @tparam R
    *   A rank type
    */
  def empty[F[_]: Sync, K: KeyLike, R]: InMemoryStorage[F, K, R] =
    from(Map.empty)

  /**
    * Factory method for seeding state
    *
    * @param xs
    *   A map of ranks to anonymous payloads
    *
    * @tparam F
    *   An effect type
    * @tparam K
    *   A key type
    * @tparam R
    *   A rank type
    */
  def from[F[_]: Sync, K: KeyLike, R](xs: Map[R, String]): InMemoryStorage[F, K, R] = {
    val store = new InMemoryStorage[F, K, R]

    xs.foreach { case (r, s) =>
      store.addRecord(s, r)
    }

    store
  }
}

/**
  * @param xs
  *   A bi-directional map between PKs and ranks.
  * @param K
  *   Evidence for key behaviors over `K`
  *
  * @tparam F
  *   An effect type
  * @tparam K
  *   The type for primary keys in this storage. Usually `Int`
  * @tparam R
  *   The type for ranking items relative to one another. Usually `Int` but could be something like `String`
  */
class InMemoryStorage[F[_], K, R](implicit F: Sync[F], K: KeyLike[K]) extends Storage[F, K, R] {
  private case class State(nextPk: K, rows: Map[K, Record[R]])

  private[this] val state =
    new AtomicReference(State(K.first, Map.empty[K, Record[R]]))

  def getSnapshot: F[Snapshot] =
    F.delay {
      snapshotOf(state.get)
    }

  def lockSnapshot: F[Snapshot] =
    F.delay {
      snapshotOf(state.get)
    }

  def insertNewRecord(payload: String, rank: R): F[Row] =
    F.delay {
      addRecord(payload, rank)
    }

  def changeRankTo(id: K, rank: R): F[Row] =
    F.delay {
      updateState { s =>
        val withNewRank =
          s.rows(id).copy(rank = rank)

        val next =
          s.copy(rows = s.rows.updated(id, withNewRank))

        assertUniqueRanks(next)

        next -> (id -> withNewRank)
      }
    }

  def applyUpdateInCascade(up: Update): F[Unit] =
    F.delay {
      updateState[Unit] { s =>
        val next =
          s.copy(rows = s.rows.updated(up.pk, Record("", up.to)))

        assertUniqueRanks(next)

        next -> ()
      }
    }

  private[this] def snapshotOf(s: State): Snapshot =
    s.rows.view.mapValues(_.rank).toMap

  private[this] def assertUniqueRanks(s: State): Unit =
    assert(s.rows.values.map(_.rank).toSet.size == s.rows.size, "ranks are unique")

  @tailrec
  private[this] def updateState[A](f: State => (State, A)): A = {
    val current =
      state.get

    val (next, out) =
      f(current)

    if (state.compareAndSet(current, next)) out
    else updateState(f)
  }

  /**
    * Not a part of the public API. For testing only.
    */
  def addRecord(payload: String, rank: R): Row =
    updateState { s =>
      val rec =
        Record(payload, rank)

      val pk =
        s.nextPk

      val next =
        State(K.increment(pk), s.rows.updated(pk, rec))

      assertUniqueRanks(next)

      next -> (pk -> rec)
    }

  /**
    * Not a part of the public API. For testing only.
    */
  def dump: Map[K, Record[R]] =
    state.get.rows

  /**
    * Not a part of the public API. For testing only.
    */
  def size: Int =
    state.get.rows.size

  // TODO reimplement toString as Show
  override def toString: String =
    (state.get.nextPk.toString :: state.get.rows.map(_.toString).toList).mkString("\n")
}
