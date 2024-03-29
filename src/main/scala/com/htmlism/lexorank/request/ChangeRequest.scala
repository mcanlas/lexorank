package com.htmlism.lexorank
package request

import cats.*

object ChangeRequest {
  def apply[A](id: A, req: PositionRequest[A])(implicit A: Eq[A]): ChangeRequest[A] Or LexorankError =
    Either.cond(req.keys.forall(rk => A.neqv(id, rk)), new ChangeRequest[A](id, req), errors.DuplicateChangeKeys)
}

class ChangeRequest[A] private (val id: A, val req: PositionRequest[A]) {
  override def toString: String =
    s"ChangeRequest(id: $id, req: ${req.toString})"
}
