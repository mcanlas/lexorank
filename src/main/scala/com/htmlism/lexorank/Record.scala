package com.htmlism.lexorank

/**
 * An example domain class, rankable over `A`.
 */
case class Record[A](name: String, rank: A)
