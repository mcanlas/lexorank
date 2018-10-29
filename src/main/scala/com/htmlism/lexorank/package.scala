package com.htmlism

package object lexorank {

  /**
    * An infix alias for `Either`
    *
    * {{{
    *   val x = ???: Value Or Error
    * }}}
    *
    * @tparam A The payload type
    * @tparam B The error type
    */
  type Or[+A, +B] = Either[B, A]

  /**
    * @group Imported types
    */
  type PosInt = PositiveInteger

  /**
    * @group Imported types
    */
  val PosInt = PositiveInteger

  /**
    * @group Imported types
    */
  type LexorankError = errors.LexorankError
}
