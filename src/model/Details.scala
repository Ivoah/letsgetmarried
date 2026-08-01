package net.ivoah.letsgetmarried
package model

import play.api.libs.json.*

import java.time.{LocalDate, LocalDateTime}
import java.io.File
import scala.io.Source
import scala.math.Ordering.Implicits.seqOrdering

given detailsFormat: Format[Details] = Json.format[Details]
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
)

object Details {
  given Format[General] = Json.format[General]
  case class General(
    underConstruction: Boolean = true,
    contact: String = "nobody",
    style: String = "",
    headerImages: Seq[String] = Seq(),
    groom: String = "Groom name",
    bride: String = "Bride name",
    date: LocalDateTime = LocalDateTime.now().plusMonths(1),
    location: String = "Nowhere"
  )

  given Format[Home] = Json.format[Home]
  case class Home(image: String = "", locations: Seq[Home.Location] = Seq())
  object Home {
    given Format[Location] = Json.format[Location]
    case class Location(name: String, time: String, address: String, link: String, details: String)
  }

  given Format[Story] = Json.format[Story]
  case class Story(title: String = "Our Story", image: String = "", body: String = "We met and falled in love.")
  
  given Format[WeddingParty] = Json.format[WeddingParty]
  case class WeddingParty(bridesmaids: Seq[WeddingParty.PartyMember] = Seq(), groomsmen: Seq[WeddingParty.PartyMember] = Seq())
  object WeddingParty {
    given Format[PartyMember] = Json.format[PartyMember]
    case class PartyMember(name: String, role: String, image: String, bio: String)
  }
  
  given Format[Photos] = Json.format[Photos]
  case class Photos(photos: Seq[Photos.Photo] = Seq())
  object Photos {
    given Format[Photo] = Json.format[Photo]
    case class Photo(image: String, caption: Option[String])
  }
  
  given Format[Registry] = Json.format[Registry]
  case class Registry(address: String = "", notes: String = "", items: Seq[Registry.Item] = Seq()) {
    require(items.distinctBy(_.id).length == items.length, "Duplicate id in registry list")
  }
  object Registry {
    given Format[Item] = Json.format[Item]
    case class Item(name: String, id: String, link: String, image: String, price: Option[Double])
  }
  
  given Format[RSVP] = Json.format[RSVP]
  case class RSVP(notes: String = "", invitations: Seq[RSVP.Invitation] = Seq())
  object RSVP {
    given Format[Invitation] = Json.format[Invitation]
    case class Invitation(name: String, people: Seq[String], childrenInvited: Option[Boolean])
  }
  
  given Format[Hotels] = Json.format[Hotels]
  case class Hotels(notes: String = "", hotels: Seq[Hotels.Hotel] = Seq())
  object Hotels {
    given Format[Hotel] = Json.format[Hotel]
    case class Hotel(name: String, address: String, link: String)
  }

  given Format[Invitation] = Json.format[Invitation]
  case class Invitation(
    tagline: String = "Love endures all things",
    parents: String = "Father and mother of the bride",
    details: String = "It's a wedding, come to it",
    url: String = "https://example.com",
    deadline: LocalDate = LocalDate.now().plusWeeks(1)
  )
  
  given Format[Program] = Json.format[Program]
  case class Program(
    ceremony: Seq[Seq[String]] = Seq(),
    pastors: Seq[String] = Seq(),
    pianist: String = "",
    ushers: Seq[String] = Seq(),
    flowerGirl: String = "flower girl",
    ringBearer: String = "ring bearer",
    reception: Seq[Seq[String]] = Seq(),
    thanks: String = ""
  )

  def fromForm(form: Map[String, String | File]): Details = {
    Details()
  }
}

// val Seating = Source.fromResource("seating.yaml").getLines().mkString("\n").as[Map[String, Seq[String]]] match {
//   case Left(err) => throw err
//   case Right(seating) =>
//     val s = seating.toSeq
//       .flatMap { case (k, vv) => vv.map(v => v -> k) }
//       .sortBy(_._1.split("\\s+").reverse.toSeq)
//     s ++ Seq.fill(30 - s.length % 30)("" -> "")
// }
