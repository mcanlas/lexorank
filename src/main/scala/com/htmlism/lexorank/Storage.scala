package com.htmlism.lexorank

object Storage {
  def apply(xs: (Int, String)*): Storage =
    Storage(xs.map((Row.apply _).tupled).toList)
}

case class Storage(xs: List[Row])