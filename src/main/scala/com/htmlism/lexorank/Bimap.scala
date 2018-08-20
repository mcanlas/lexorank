package com.htmlism.lexorank

object Bimap {
  def empty[K, V]: Bimap[K, V] =
    Bimap(Map.empty[K, V])
}

case class Bimap[K, V](xs: Map[K, V]) {
  def + (k: K, v: V): Bimap[K, V] =
    if (xs.keySet(k) || xs.values.toSet(v))
      this
    else
      Bimap(xs + (k -> v))
}
