package com.htmlism

import cats.data._
import cats.effect._
import cats.implicits._
import mouse.all._

package object lexorank {
  type Pk = Int
  type Or[A, B] = Either[B, A]
  type Log = List[String]
  type AnnotatedIO[A] = WriterT[IO, Log, A]

  object AnnotatedIO {
    def apply[A](body: => A): AnnotatedIO[A] =
      IO(body) |> WriterT.liftF[IO, Log, A]
  }
}
