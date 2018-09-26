package com.htmlism

package object lexorank {
  type Or[+A, +B] = Either[B, A]

  type PosInt = PositiveInteger
  val PosInt = PositiveInteger

  type LexorankError = errors.LexorankError
}
