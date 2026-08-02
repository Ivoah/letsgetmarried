package net.ivoah.letsgetmarried
package view

import java.time.{LocalDate, LocalDateTime}
import java.io.File
import scalatags.Text.all.*

trait FormBuilder[T] {
  def createForm(v: T): Frag
}

object FormBuilder {
  def createForm[T](v: T)(using fb: FormBuilder[T]): Frag = fb.createForm(v)
}

given [T: FormBuilder] => FormBuilder[Seq[T]] {
	def createForm(seq: Seq[T]): Frag = ul(
		for (v <- seq) yield li(FormBuilder.createForm(v)),
		li(button("Add..."))
	)
}

given [T: FormBuilder] => FormBuilder[Option[T]] {
  def createForm(opt: Option[T]): Frag = FormBuilder.createForm(opt.toSeq)
}

given FormBuilder[String] {
  def createForm(s: String): Frag = input(value:=s)
}

given FormBuilder[Double] {
  def createForm(d: Double): Frag = input(`type`:="number", value:=d)
}

given FormBuilder[Boolean] {
	def createForm(b: Boolean): Frag = input(`type`:="checkbox", if (b) checked else frag())
}

given FormBuilder[LocalDate] {
	def createForm(ld: LocalDate): Frag = input(`type`:="date", value:=ld.toString)
}

given FormBuilder[LocalDateTime] {
	def createForm(ldt: LocalDateTime): Frag = input(`type`:="datetime-local", value:=ldt.toString)
}

given FormBuilder[File] {
	def createForm(f: File): Frag = input(`type`:="file")
}
