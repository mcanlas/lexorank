package com.htmlism.lexorank

sealed trait LexorankError

case object IdWasInBefore extends LexorankError

case object IdWasInAfter extends LexorankError