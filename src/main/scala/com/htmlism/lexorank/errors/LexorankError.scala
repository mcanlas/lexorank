package com.htmlism.lexorank.errors

/**
  * A logical, non-fatal error.
  */
sealed trait LexorankError

sealed trait RequestError extends LexorankError
sealed trait AfterIoError extends LexorankError

/**
  * When a requested row key or position request key does not exist in the context snapshot.
  */
case object KeyNotInContext extends RequestError

sealed trait OverflowError extends AfterIoError

sealed trait MaxOverflow extends OverflowError
case object MaxOverflow  extends MaxOverflow

sealed trait MinUnderflow extends OverflowError
case object MinUnderflow  extends MinUnderflow
