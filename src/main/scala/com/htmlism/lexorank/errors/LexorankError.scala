package com.htmlism.lexorank.errors

/**
  * A logical, non-fatal error.
  */
sealed trait LexorankError

sealed trait OverflowError extends LexorankError

sealed trait MaxOverflow extends OverflowError
case object MaxOverflow  extends MaxOverflow

sealed trait MinUnderflow extends OverflowError
case object MinUnderflow  extends MinUnderflow
