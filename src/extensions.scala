package net.ivoah.letsgetmarried

import play.api.libs.json.*

extension [T](i: Seq[T]) {
  def join(joiner: T): Seq[T] = i.flatMap(Seq(_, joiner)).dropRight(1)
}

extension (f: Format.type) {
	inline def derived[T]: Format[T] = Json.format[T]
}
