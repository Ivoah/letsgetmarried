package net.ivoah.letsgetmarried

import java.time.{LocalDate, LocalDateTime}
import java.io.File
import scalatags.Text.all.*

import scala.deriving.Mirror
import scala.compiletime.*

trait FormBuilder[T] {
  def createForm(v: Option[T], name: String): Frag
  extension(v: T) {
    def createForm(name: String): Frag = this.createForm(Some(v), name)
  }
}

object FormBuilder {
  def createForm[T](v: Option[T], name: String)(using fb: FormBuilder[T]): Frag = fb.createForm(v, name)

  given [T: FormBuilder] => FormBuilder[Seq[T]] = (seq, name) => ul(
    for ((v, i) <- seq.toSeq.flatten.zipWithIndex) yield li(v.createForm(s"$name[$i]")),
    li(button("Add..."))
  )

  given [T: FormBuilder] => FormBuilder[Option[T]] = (o, n)   => FormBuilder.createForm(o.flatten, n)
  given FormBuilder[String]                        = (s, n)   => input(name:=n, s.map(value:=_))
  given FormBuilder[Double]                        = (d, n)   => input(`type`:="number", name:=n, d.map(value:=_))
  given FormBuilder[Boolean]                       = (b, n)   => frag(
    input(`type`:="hidden", name:=n, value:="false"),
    input(`type`:="checkbox", name:=n, value:="true", if (b.exists(identity)) checked else frag())
  )
  given FormBuilder[LocalDate]                     = (ld, n)  => input(`type`:="date", name:=n, ld.map(value:=_.toString))
  given FormBuilder[LocalDateTime]                 = (ldt, n) => input(`type`:="datetime-local", name:=n, ldt.map(value:=_.toString))
  given FormBuilder[File]                          = (f, n)   => input(`type`:="file", name:=n)

  private inline def getFormBuilders[T <: Tuple]: List[FormBuilder[?]] = inline erasedValue[T] match {
    case _: (t *: ts)  => summonInline[FormBuilder[t]] :: getFormBuilders[ts]
    case _: EmptyTuple => Nil
  }

  private inline def getLabels[T <: Tuple]: List[String] = inline erasedValue[T] match {
    case _: (head *: tail) => constValue[head].toString :: getLabels[tail]
    case _: EmptyTuple     => Nil
  }

  private def prettyLabel(l: String): String = l.split("(?=[A-Z])").map(_.toLowerCase).mkString(" ").capitalize

  inline def derived[T](using p: Mirror.ProductOf[T]): FormBuilder[T] = new FormBuilder[T] {
    val formBuilders = getFormBuilders[p.MirroredElemTypes]
    val labels       = getLabels[p.MirroredElemLabels]

    override def createForm(v: Option[T], name: String): Frag = {
      val elements = v.map(_.asInstanceOf[Product].productIterator.toIndexedSeq)
      ul(labels.zip(formBuilders).zipWithIndex.map {
        case ((l, fb), i) =>
          li(s"${prettyLabel(l)}: ", fb.asInstanceOf[FormBuilder[Any]].createForm(elements.map(_(i)), s"$name.$l"))
      })
    }
  }
}
