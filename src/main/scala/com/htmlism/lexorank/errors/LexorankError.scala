package com.htmlism.lexorank.errors

/**
  * A logical, non-fatal error.
  */
sealed trait LexorankError

/**
  * An input validation error that occurs before any IO occurs.
  */
sealed trait RequestError extends LexorankError

/**
  * An error that occurs after some IO occurs.
  */
sealed trait AfterIoError extends LexorankError

/**
  * The key of a change request cannot be the among the keys of the underlying position request.
  */
object DuplicateChangeKeys extends RequestError

/**
  * The two keys of a between request cannot be the same.
  */
object DuplicateBetweenKeys extends RequestError

/**
  * When a requested row key or position request key does not exist in the context snapshot.
  */
case object KeyNotInContext extends AfterIoError

/**
  * An attempt to adjust a rank wraps around an end of a finite key space, violating order.
  */
sealed trait OverflowError extends AfterIoError

/**
  * When incrementing a rank wraps around the maximum of a key space.
  */
sealed trait MaxOverflow extends OverflowError

/**
  * Singleton inhabitant.
  */
object MaxOverflow extends MaxOverflow

/**
  * When decrementing a rank wraps around the minimum of a key space.
  */
sealed trait MinUnderflow extends OverflowError

/**
  * Singleton inhabitant.
  */
object MinUnderflow extends MinUnderflow
