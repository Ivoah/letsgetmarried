package net.ivoah.letsgetmarried

extension [T](i: Seq[T]) {
  def join(joiner: T): Seq[T] = i.flatMap(Seq(_, joiner)).dropRight(1)
}
