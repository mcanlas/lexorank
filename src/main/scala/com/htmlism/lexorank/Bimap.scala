package com.htmlism.lexorank

object Bimap {
  def empty[K, V]: Bimap[K, V] =
    Bimap(Map.empty[K, V])

  def fromList[K, V](xs: List[(K, V)]): Bimap[K, V] =
    xs.foldLeft(Bimap.empty[K, V])((acc, e) => acc + e)

}

case class Bimap[K, V](xs: Map[K, V]) {
  def + (x: (K, V)): Bimap[K, V] =
    if (xs.keySet(x._1) || xs.values.toSet(x._2))
      this
    else
      Bimap(xs + x)
}
