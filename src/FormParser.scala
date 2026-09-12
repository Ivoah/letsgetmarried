package net.ivoah.letsgetmarried

import java.time.{LocalDate, LocalDateTime}
import java.io.File
import scala.util.Try

import scala.deriving.Mirror
import scala.compiletime.*

type Form = Map[String, String]

trait FormParser[T] {
  def parseForm(f: Form, name: String): T
}

object FormParser {
  def parseForm[T](f: Form, name: String)(using fb: FormParser[T]): T = fb.parseForm(f, name)

  given [T: FormParser] => FormParser[Seq[T]] = (f, n) => {
    val pattern = s"^\\Q$n\\E(.*)$$".r
    val values = f.collect {
      case (pattern(rest), v) => rest -> v
    }
    for (i <- 0 to values.keys.map{case s"[$i]$rest" => i.toInt}.max) yield FormParser.parseForm[T](values, s"[$i]")
  }
  given [T: FormParser] => FormParser[Option[T]]  = (f, n) => if (f.contains(n)) Try(FormParser.parseForm[T](f, n)).toOption else None
  given FormParser[String]                        = (f, n) => f(n)
  given FormParser[Double]                        = (f, n) => f(n).toDouble
  given FormParser[Boolean]                       = (f, n) => f(n) == "true"
  given FormParser[LocalDate]                     = (f, n) => LocalDate.parse(f(n))
  given FormParser[LocalDateTime]                 = (f, n) => LocalDateTime.parse(f(n))
  given FormParser[File]                          = (f, n) => ???

  private inline def getFormParsers[T <: Tuple]: List[FormParser[?]] = inline erasedValue[T] match {
    case _: (t *: ts)  => summonInline[FormParser[t]] :: getFormParsers[ts]
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

  inline def derived[T](using p: Mirror.ProductOf[T]): FormParser[T] = new FormParser[T] {
    val formParsers = getFormParsers[p.MirroredElemTypes]
    val labels      = getLabels[p.MirroredElemLabels]

    override def parseForm(f: Form, name: String): T = {
      val pattern = s"^\\Q$name.\\E(.*)$$".r
      val fields = labels.zip(formParsers).map {
        case (l, fp) => fp.parseForm(f.collect {
          case (pattern(rest), v) => rest -> v
        }, l)
      }
      p.fromTuple(fields.toTuple.asInstanceOf[p.MirroredElemTypes])
    }
  }
}
