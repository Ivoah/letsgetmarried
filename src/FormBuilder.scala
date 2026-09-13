package net.ivoah.letsgetmarried

import java.time.{LocalDate, LocalDateTime}
import java.io.File
import scalatags.Text.all.*

import scala.deriving.Mirror
import scala.compiletime.*
import scala.util.Try

type Form = Map[String, String]

trait FormBuilder[T] {
  def createForm(v: Option[T], name: String): Frag
  def parseForm(f: Form, name: String): T
  extension(v: T) {
    def createForm(name: String): Frag = this.createForm(Some(v), name)
  }
}

object FormBuilder {
  def createForm[T](v: Option[T], name: String)(using fb: FormBuilder[T]): Frag = fb.createForm(v, name)
  def parseForm[T](f: Form, name: String)(using fb: FormBuilder[T]): T = fb.parseForm(f, name)

  given [T: FormBuilder] => FormBuilder[Seq[T]] {
    def createForm(v: Option[Seq[T]], name: String): Frag = ul(
      for ((v, i) <- v.toSeq.flatten.zipWithIndex) yield li(v.createForm(s"$name[$i]")),
      li(button("Add..."))
    )
    def parseForm(f: Form, name: String): Seq[T] = {
      val pattern = s"^\\Q$name\\E(.*)$$".r
      val values = f.collect {
        case (pattern(rest), v) => rest -> v
      }
      for (i <- 0 to values.keys.map{case s"[$i]$rest" => i.toInt}.max) yield FormBuilder.parseForm[T](values, s"[$i]")
    }
  }

  given [T: FormBuilder] => FormBuilder[Option[T]] {
    def createForm(o: Option[Option[T]], n: String) = FormBuilder.createForm(o.flatten, n)
    def parseForm(f: Form, n: String): Option[T] = if (f.contains(n)) Try(FormBuilder.parseForm[T](f, n)).toOption else None
  }

  given FormBuilder[String] {
    def createForm(s: Option[String], n: String): Frag = input(name:=n, s.map(value:=_))
    def parseForm(f: Form, name: String): String = f(name)
  }

  given FormBuilder[Double] {
    def createForm(d: Option[Double], n: String): Frag = input(`type`:="number", name:=n, d.map(value:=_))
    def parseForm(f: Form, name: String): Double = f(name).toDouble
  }

  given FormBuilder[Boolean] {
    def createForm(b: Option[Boolean], n: String): Frag = frag(
      input(`type`:="hidden", name:=n, value:="false"),
      input(`type`:="checkbox", name:=n, value:="true", if (b.exists(identity)) checked else frag())
    )
    def parseForm(f: Form, name: String): Boolean = f(name) == "true"
  }

  given FormBuilder[LocalDate]    {
    def createForm(ld: Option[LocalDate], n: String) = input(`type`:="date", name:=n, ld.map(value:=_.toString))
    def parseForm(f: Form, name: String): LocalDate = LocalDate.parse(f(name))
  }

  given FormBuilder[LocalDateTime]{
    def createForm(ldt: Option[LocalDateTime], n: String) =input(`type`:="datetime-local", name:=n, ldt.map(value:=_.toString))
    def parseForm(f: Form, name: String): LocalDateTime = LocalDateTime.parse(f(name))
  }

  given FormBuilder[File] {
    def createForm(f: Option[File], n: String): Frag = input(`type`:="file", name:=n)
    def parseForm(f: Form, name: String): File = ???
  }

  private inline def getFormBuilders[T <: Tuple]: List[FormBuilder[?]] = inline erasedValue[T] match {
    case _: (t *: ts)  => summonInline[FormBuilder[t]] :: getFormBuilders[ts]
    case _: EmptyTuple => Nil
  }

  private inline def getLabels[T <: Tuple]: List[String] = inline erasedValue[T] match {
    case _: (head *: tail) => constValue[head].toString :: getLabels[tail]
    case _: EmptyTuple     => Nil
  }

  extension(l: List[?]) {
    private def toTuple: Tuple = l match {
      case head :: tail => head *: tail.toTuple
      case Nil => EmptyTuple
    }
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

    override def parseForm(f: Form, name: String): T = {
      val pattern = s"^\\Q$name.\\E(.*)$$".r
      val fields = labels.zip(formBuilders).map {
        case (l, fb) => fb.parseForm(f.collect {
          case (pattern(rest), v) => rest -> v
        }, l)
      }
      p.fromTuple(fields.toTuple.asInstanceOf[p.MirroredElemTypes])
    }
  }
}
