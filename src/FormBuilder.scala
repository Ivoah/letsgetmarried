package net.ivoah.letsgetmarried

import java.time.{LocalDate, LocalDateTime}
import java.io.File
import scalatags.Text.all.*

import scala.deriving.Mirror
import scala.compiletime.*

trait FormBuilder[T] {
  def createForm(v: Option[T] = None): Frag
  def createForm(v: T): Frag = createForm(Some(v))
}

object FormBuilder {
  def createForm[T](v: Option[T] = None)(using fb: FormBuilder[T]): Frag = fb.createForm(v)
  def createForm[T](v: T)(using fb: FormBuilder[T]): Frag = fb.createForm(v)

  given [T: FormBuilder] => FormBuilder[Seq[T]] = seq => ul(
    for (v <- seq.toSeq.flatten) yield li(FormBuilder.createForm(Some(v))),
    li(button("Add..."))
  )

  given [T: FormBuilder] => FormBuilder[Option[T]] = o   => FormBuilder.createForm[T](o.flatten)
  given FormBuilder[String]                        = s   => input(s.map(value:=_))
  given FormBuilder[Double]                        = d   => input(`type`:="number", d.map(value:=_))
  given FormBuilder[Boolean]                       = b   => input(`type`:="checkbox", if (b.exists(identity)) checked else frag())
  given FormBuilder[LocalDate]                     = ld  => input(`type`:="date", ld.map(value:=_.toString))
  given FormBuilder[LocalDateTime]                 = ldt => input(`type`:="datetime-local", ldt.map(value:=_.toString))
  given FormBuilder[File]                          = f   => input(`type`:="file")

  private inline def getFormBuilders[T <: Tuple]: List[FormBuilder[?]] = inline erasedValue[T] match {
    case _: (t *: ts)  => summonInline[FormBuilder[t]] :: getFormBuilders[ts]
    case _: EmptyTuple => Nil
  }

  private inline def getLabels[T <: Tuple]: List[String] = inline erasedValue[T] match {
    case _: (head *: tail) => constValue[head].toString :: getLabels[tail]
    case _: EmptyTuple     => Nil
  }

  inline def derived[T](using p: Mirror.ProductOf[T]): FormBuilder[T] = new FormBuilder[T] {
    val formBuilders = getFormBuilders[p.MirroredElemTypes]
    val labels       = getLabels[p.MirroredElemLabels]

    override def createForm(v: Option[T]): Frag = {
      val elements = v.map(_.asInstanceOf[Product].productIterator.toSeq)
      ul(
        labels
          .zip(formBuilders)
          .zipWithIndex
          .map {
            case ((l, fb), i) => li(s"${l.split("(?=[A-Z])").map(_.toLowerCase).mkString(" ").capitalize}: ", fb.asInstanceOf[FormBuilder[Any]].createForm(elements.map(_(i))))
          }
      )
    }
  }
}
