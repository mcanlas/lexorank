package com.htmlism.lexorank.errors

/**
  * A logical, non-fatal error.
  */
sealed trait LexorankError

sealed trait RequestError extends LexorankError
sealed trait AfterIoError extends LexorankError

case object DuplicateChangeKeys  extends RequestError
case object DuplicateBetweenKeys extends RequestError

/**
  * When a requested row key or position request key does not exist in the context snapshot.
  */
case object KeyNotInContext          extends AfterIoError
case object ImpossibleBetweenRequest extends AfterIoError
sealed trait OverflowError           extends AfterIoError

sealed trait MaxOverflow extends OverflowError
case object MaxOverflow  extends MaxOverflow

sealed trait MinUnderflow extends OverflowError
case object MinUnderflow  extends MinUnderflow
