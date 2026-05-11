package com.htmlism.lexorank
package request

import cats.*

object ChangeRequest {
  def apply[A: Eq](id: A, req: PositionRequest[A]): Either[LexorankError, ChangeRequest[A]] =
    Either.cond(req.keys.forall(rk => Eq[A].neqv(id, rk)), new ChangeRequest[A](id, req), errors.DuplicateChangeKeys)
}

class ChangeRequest[A] private (val id: A, val req: PositionRequest[A]) {
  override def toString: String =
    s"ChangeRequest(id: $id, req: ${req.toString})"
}
