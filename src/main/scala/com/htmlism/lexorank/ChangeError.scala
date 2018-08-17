package com.htmlism.lexorank

sealed trait ChangeError

case object IdWasInBefore extends ChangeError

case object IdWasInAfter extends ChangeError

case object IdDoesNotExistInStorage extends ChangeError