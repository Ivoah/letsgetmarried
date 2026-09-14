package net.ivoah.letsgetmarried

import play.api.libs.json.*

import java.time.{LocalDate, LocalDateTime}
import java.io.File
import scala.io.Source
import scala.math.Ordering.Implicits.seqOrdering
import scalatags.Text.all.*

trait StringWrapper[T] extends Format[T] with FormBuilder[T] {
  def init(s: String): T
  def content(v: T): String

  def reads(v: JsValue): JsResult[T] = v match {
    case JsString(str) => JsSuccess(init(str))
    case _ => JsError()
  }
  def writes(s: T): JsValue = JsString(content(s))

  def default: T = init("")
  def buildForm(v: Option[T], n: String): Frag
  def parseForm(f: Form, name: String): T = init(FormBuilder.parseForm[String](f, name))
}

case class Code(code: String)
given StringWrapper[Code] {
  def init(s: String) = Code(s)
  def content(c: Code) = c.code
  def buildForm(c: Option[Code], n: String): Frag = textarea(name:=n, c.map(_.code))
}

case class Image(path: String)
given StringWrapper[Image] {
  def init(s: String) = Image(s)
  def content(i: Image) = i.path
  def buildForm(i: Option[Image], n: String): Frag = input(name:=n, i.map(value:=_.path))
}
given Conversion[Image, Frag] = m => img(src:=m.path)

case class MultilineString(content: String)
given StringWrapper[MultilineString] {
  def init(s: String) = MultilineString(s)
  def content(s: MultilineString): String = s.content
  def buildForm(s: Option[MultilineString], n: String): Frag = textarea(name:=n, s.map(_.content))
}
given Conversion[MultilineString, Frag] = m => StringFrag(m.content)

case class Details(
  general: Details.General,
  home: Details.Home,
  story: Details.Story,
  weddingParty: Details.WeddingParty,
  photos: Details.Photos,
  registry: Details.Registry,
  rsvp: Details.RSVP,
  hotels: Details.Hotels,

  invitation: Details.Invitation,
  program: Details.Program
) derives Format, FormBuilder

object Details {
  case class General(
    underConstruction: Boolean,
    contact: String,
    style: Option[Code],
    headerImages: Seq[Image],
    groom: String,
    bride: String,
    date: LocalDateTime,
    location: String
  ) derives Format, FormBuilder

  case class Home(image: Image, locations: Seq[Home.Location]) derives Format, FormBuilder
  object Home {
    case class Location(name: String, time: String, address: MultilineString, link: String, details: Markdown) derives Format, FormBuilder
  }

  case class Story(title: String, image: Image, body: Markdown) derives Format, FormBuilder
  
  case class WeddingParty(bridesmaids: Seq[WeddingParty.PartyMember], groomsmen: Seq[WeddingParty.PartyMember]) derives Format, FormBuilder
  object WeddingParty {
    case class PartyMember(name: String, role: String, image: Image, bio: Markdown) derives Format, FormBuilder
  }
  
  case class Photos(photos: Seq[Photos.Photo]) derives Format, FormBuilder
  object Photos {
    case class Photo(image: Image, caption: Option[Markdown]) derives Format, FormBuilder
  }
  
  case class Registry(address: MultilineString, notes: Markdown, items: Seq[Registry.Item]) derives Format, FormBuilder {
    require(items.distinctBy(_.id).length == items.length, "Duplicate id in registry list")
  }
  object Registry {
    case class Item(name: String, id: String, link: String, image: Image, price: Option[Double]) derives Format, FormBuilder
  }
  
  case class RSVP(notes: Markdown, invitations: Seq[RSVP.Invitation]) derives Format, FormBuilder
  object RSVP {
    case class Invitation(name: String, people: Seq[String], childrenInvited: Option[Boolean]) derives Format, FormBuilder
  }
  
  case class Hotels(notes: Markdown, hotels: Seq[Hotels.Hotel]) derives Format, FormBuilder
  object Hotels {
    case class Hotel(name: String, address: MultilineString, link: String) derives Format, FormBuilder
  }

  case class Invitation(
    tagline: String,
    parents: String,
    details: Markdown,
    url: String,
    deadline: LocalDate
  ) derives Format, FormBuilder
  
  case class Program(
    ceremony: Seq[Seq[String]],
    pastors: Seq[String],
    pianist: String,
    ushers: Seq[String],
    flowerGirl: String,
    ringBearer: String,
    reception: Seq[Seq[String]],
    thanks: Markdown
  ) derives Format, FormBuilder
}

// val Seating = Source.fromResource("seating.yaml").getLines().mkString("\n").as[Map[String, Seq[String]]] match {
//   case Left(err) => throw err
//   case Right(seating) =>
//     val s = seating.toSeq
//       .flatMap { case (k, vv) => vv.map(v => v -> k) }
//       .sortBy(_._1.split("\\s+").reverse.toSeq)
//     s ++ Seq.fill(30 - s.length % 30)("" -> "")
// }
