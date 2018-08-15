package com.htmlism.lexorank

object Lexorank {
  /**
   * ID cannot be equal either of the provided `before` or `after`.
   *
   * @param id
   * @param after
   * @param before
   * @return
   */
  def changePosition(id: Pk, after: Option[Pk], before: Option[Pk]): Option[Row] =
    if (after.contains(id) || before.contains(id))
      None
    else
      ???
}
