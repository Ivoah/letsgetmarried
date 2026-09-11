package net.ivoah.letsgetmarried

import play.api.libs.json.*

import java.time.{LocalDate, LocalDateTime}
import java.io.File
import scala.io.Source
import scala.math.Ordering.Implicits.seqOrdering
import scalatags.Text.all.*

case class Code(code: String)
given Format[Code] {
  def reads(v: JsValue): JsResult[Code] = v match {
    case JsString(str) => JsSuccess(Code(str))
    case _ => JsError()
  }
  def writes(c: Code): JsValue = JsString(c.code)
}
given FormBuilder[Code] = c => textarea(c.map(_.code))

type Image = String

case class Details(
  general: Details.General = Details.General(),
  home: Details.Home = Details.Home(),
  story: Details.Story = Details.Story(),
  weddingParty: Details.WeddingParty = Details.WeddingParty(),
  photos: Details.Photos = Details.Photos(),
  registry: Details.Registry = Details.Registry(),
  rsvp: Details.RSVP = Details.RSVP(),
  hotels: Details.Hotels = Details.Hotels(),

  invitation: Details.Invitation = Details.Invitation(),
  program: Details.Program = Details.Program()
) derives Format, FormBuilder

object Details {
  case class General(
    underConstruction: Boolean = true,
    contact: String = "nobody",
    style: Code = Code(""),
    headerImages: Seq[Image] = Seq(),
    groom: String = "Groom name",
    bride: String = "Bride name",
    date: LocalDateTime = LocalDateTime.now().plusMonths(1),
    location: String = "Nowhere"
  ) derives Format, FormBuilder

  case class Home(image: Image = "", locations: Seq[Home.Location] = Seq()) derives Format, FormBuilder
  object Home {
    case class Location(name: String, time: String, address: String, link: String, details: String) derives Format, FormBuilder
  }

  case class Story(title: String = "Our Story", image: Image = "", body: String = "We met and falled in love.") derives Format, FormBuilder
  
  case class WeddingParty(bridesmaids: Seq[WeddingParty.PartyMember] = Seq(), groomsmen: Seq[WeddingParty.PartyMember] = Seq()) derives Format, FormBuilder
  object WeddingParty {
    case class PartyMember(name: String, role: String, image: Image, bio: String) derives Format, FormBuilder
  }
  
  case class Photos(photos: Seq[Photos.Photo] = Seq()) derives Format, FormBuilder
  object Photos {
    case class Photo(image: Image, caption: Option[String]) derives Format, FormBuilder
  }
  
  case class Registry(address: String = "", notes: String = "", items: Seq[Registry.Item] = Seq()) derives Format, FormBuilder {
    require(items.distinctBy(_.id).length == items.length, "Duplicate id in registry list")
  }
  object Registry {
    case class Item(name: String, id: String, link: String, image: Image, price: Option[Double]) derives Format, FormBuilder
  }
  
  case class RSVP(notes: String = "", invitations: Seq[RSVP.Invitation] = Seq()) derives Format, FormBuilder
  object RSVP {
    case class Invitation(name: String, people: Seq[String], childrenInvited: Option[Boolean]) derives Format, FormBuilder
  }
  
  case class Hotels(notes: String = "", hotels: Seq[Hotels.Hotel] = Seq()) derives Format, FormBuilder
  object Hotels {
    case class Hotel(name: String, address: String, link: String) derives Format, FormBuilder
  }

  case class Invitation(
    tagline: String = "Love endures all things",
    parents: String = "Father and mother of the bride",
    details: String = "It's a wedding, come to it",
    url: String = "https://example.com",
    deadline: LocalDate = LocalDate.now().plusWeeks(1)
  ) derives Format, FormBuilder
  
  case class Program(
    ceremony: Seq[Seq[String]] = Seq(),
    pastors: Seq[String] = Seq(),
    pianist: String = "",
    ushers: Seq[String] = Seq(),
    flowerGirl: String = "flower girl",
    ringBearer: String = "ring bearer",
    reception: Seq[Seq[String]] = Seq(),
    thanks: String = ""
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
