package com.htmlism.lexorank.storage

import scala.io.Source

object ResourceLoader {
  def load(s: String): Source =
    Source.fromInputStream(getClass.getClassLoader.getResourceAsStream(s))
}
