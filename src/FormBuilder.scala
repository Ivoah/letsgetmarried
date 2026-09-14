package net.ivoah.letsgetmarried

import java.time.{LocalDate, LocalDateTime}
import java.io.File
import scalatags.Text.all.*

import play.api.libs.json.*

import scala.deriving.Mirror
import scala.compiletime.*
import scala.util.Try
import java.text.SimpleDateFormat

type Form = Map[String, String]

trait FormBuilder[T] {
  def default: T
  def buildForm(v: Option[T], name: String): Frag
  def parseForm(f: Form, name: String, adding: Option[String], removing: Option[String]): T
  extension(v: T) {
    def buildForm(name: String): Frag = this.buildForm(Some(v), name)
  }
}

trait StringWrapper[T] extends Format[T] with FormBuilder[T] {
  def init(s: String): T
  def content(v: T): String
  def tag(v: Option[T]): Tag

  def reads(v: JsValue): JsResult[T] = v match {
    case JsString(str) => JsSuccess(init(str))
    case _ => JsError()
  }
  def writes(s: T): JsValue = JsString(content(s))

  def default: T = init("")
  def buildForm(v: Option[T], n: String): Frag = tag(v)(id:=n, name:=n)
  def parseForm(f: Form, name: String, adding: Option[String], removing: Option[String]): T = init(FormBuilder.parseForm[String](f, name, adding, removing))
}

object FormBuilder {
  def default[T: FormBuilder as fb]: T = fb.default
  def buildForm[T: FormBuilder as fb](v: Option[T], name: String): Frag = fb.buildForm(v, name)
  def parseForm[T: FormBuilder as fb](f: Form, name: String, adding: Option[String], removing: Option[String]): T = fb.parseForm(f, name, adding, removing)

  given [T: FormBuilder as fb] => FormBuilder[Seq[T]] {
    def default: Seq[T] = Seq[T]()
    def buildForm(v: Option[Seq[T]], n: String): Frag = ul(id:=n,
      for ((v, i) <- v.toSeq.flatten.zipWithIndex) yield li(button(raw("&times;"), name:="removing", value:=s"$n[$i]", formaction:=s"#$n"), v.buildForm(s"$n[$i]")),
      li(button("Add", name:="adding", value:=n, formaction:=s"#$n[${v.toSeq.flatten.length}]"))
    )
    def parseForm(f: Form, name: String, adding: Option[String], removing: Option[String]): Seq[T] = {
      val pattern = s"^\\Q$name\\E(.*)$$".r
      val values = f.collect {
        case (pattern(rest), v) => rest -> v
      }
      val seq = values.keys.map{case s"[$i]$rest" => i.toInt}.maxOption
        .toSeq.flatMap(0 to _)
        .collect {
          case i if !removing.exists(_ == s"$name[$i]") => fb.parseForm(values, s"[$i]", adding.map(_.stripPrefix(s"$name")), removing.map(_.stripPrefix(s"$name")))
        }

      if (adding.exists(_ == name)) seq :+ fb.default
      else seq
    }
  }

  given [T: FormBuilder as fb] => FormBuilder[Option[T]] {
    def default: Option[T] = None
    def buildForm(o: Option[Option[T]], n: String) = o.flatten match {
      case Some(v) => frag(button(raw("&times;"), name:="removing", value:=n, formaction:=s"#n"), fb.buildForm(Some(v), n))
      case None => button("Add", name:="adding", value:=n, formaction:=s"#$n")
    }
    def parseForm(f: Form, name: String, adding: Option[String], removing: Option[String]): Option[T] = {
      if (f.contains(name) && !removing.exists(_ == name)) Try(fb.parseForm(f, name, adding, removing)).toOption
      else if (adding.exists(_ == name)) Some(fb.default)
      else None
    }
  }

  given FormBuilder[String] {
    def default: String = ""
    def buildForm(s: Option[String], n: String): Frag = input(id:=n, name:=n, s.map(value:=_))
    def parseForm(f: Form, name: String, adding: Option[String], removing: Option[String]): String = f(name).replace("\r\n", "\n")
  }

  given FormBuilder[Double] {
    def default: Double = 0
    def buildForm(d: Option[Double], n: String): Frag = input(`type`:="number", id:=n, name:=n, d.map(value:=_))
    def parseForm(f: Form, name: String, adding: Option[String], removing: Option[String]): Double = f(name).toDouble
  }

  given FormBuilder[Boolean] {
    def default: Boolean = false
    def buildForm(b: Option[Boolean], n: String): Frag = frag(
      input(`type`:="hidden", name:=n, value:="false"),
      input(`type`:="checkbox", id:=n, name:=n, value:="true", if (b.exists(identity)) checked else frag())
    )
    def parseForm(f: Form, name: String, adding: Option[String], removing: Option[String]): Boolean = f(name) == "true"
  }

  given FormBuilder[LocalDate]    {
    def default: LocalDate = LocalDate.now()
    def buildForm(ld: Option[LocalDate], n: String) = input(`type`:="date", id:=n, name:=n, ld.map(value:=_.toString))
    def parseForm(f: Form, name: String, adding: Option[String], removing: Option[String]): LocalDate = LocalDate.parse(f(name))
  }

  given FormBuilder[LocalDateTime]{
    def default: LocalDateTime = LocalDateTime.now()
    def buildForm(ldt: Option[LocalDateTime], n: String) =input(`type`:="datetime-local", id:=n, name:=n, ldt.map(value:=_.withSecond(0).withNano(0).toString))
    def parseForm(f: Form, name: String, adding: Option[String], removing: Option[String]): LocalDateTime = LocalDateTime.parse(f(name))
  }

  // given FormBuilder[File] {
  //   def default: File = ???
  //   def buildForm(f: Option[File], n: String): Frag = input(`type`:="file", id:=n, name:=n)
  //   def parseForm(f: Form, name: String, adding: Option[String], removing: Option[String]): File = ???
  // }

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

  @annotation.nowarn("name=InlinedAnonClassWarning")
  inline def derived[T](using p: Mirror.ProductOf[T]): FormBuilder[T] = new FormBuilder[T] {
    val formBuilders = getFormBuilders[p.MirroredElemTypes]
    val labels       = getLabels[p.MirroredElemLabels]

    def default: T = p.fromTuple(formBuilders.map(_.default).toTuple.asInstanceOf[p.MirroredElemTypes])

    def buildForm(v: Option[T], name: String): Frag = {
      val elements = v.map(_.asInstanceOf[Product].productIterator.toIndexedSeq)
      ul(id:=name, labels.zip(formBuilders).zipWithIndex.map {
        case ((l, fb), i) =>
          li(s"${prettyLabel(l)}: ", fb.asInstanceOf[FormBuilder[Any]].buildForm(elements.map(_(i)), s"$name.$l"))
      })
    }

    def parseForm(f: Form, name: String, adding: Option[String], removing: Option[String]): T = {
      val pattern = s"^\\Q$name.\\E(.*)$$".r
      val fields = labels.zip(formBuilders).map {
        case (l, fb) => fb.parseForm(f.collect {
          case (pattern(rest), v) => rest -> v
        }, l, adding.map(_.stripPrefix(s"$name.")), removing.map(_.stripPrefix(s"$name.")))
      }
      p.fromTuple(fields.toTuple.asInstanceOf[p.MirroredElemTypes])
    }
  }
}
