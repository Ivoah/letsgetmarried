package net.ivoah.letsgetmarried

object IntExtractor {
  def unapply(s: String): Option[Int] = s.toIntOption
}
