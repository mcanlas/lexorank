package com.htmlism

import cats.data._
import cats.effect._
import cats.implicits._

package object lexorank {
  type Pk = Int
  type Or[A, B] = Either[B, A]
  type AnnotatedIO[A] = WriterT[IO, List[String], A]

  object AnnotatedIO {
    def apply[A](body: => A): AnnotatedIO[A] =
      WriterT.liftF[IO, List[String], A](IO(body))
  }
}
