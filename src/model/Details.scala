package net.ivoah.letsgetmarried
package model

import view.given

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
given view.FormBuilder[Code] {
  def createForm(c: Code): Frag = textarea(c.code)
}

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
given detailsFormat: Format[Details] = Json.format[Details]
given view.FormBuilder[Details] {
  def createForm(d: Details): Frag = ul(
    li("General: ", view.FormBuilder.createForm(d.general)),
    li("Home: ", view.FormBuilder.createForm(d.home)),
    li("Story: ", view.FormBuilder.createForm(d.story)),
    li("Wedding Party: ", view.FormBuilder.createForm(d.weddingParty)),
    li("Photos: ", view.FormBuilder.createForm(d.photos)),
    li("Registry: ", view.FormBuilder.createForm(d.registry)),
    li("RSVP: ", view.FormBuilder.createForm(d.rsvp)),
    li("Hotels: ", view.FormBuilder.createForm(d.hotels)),
    li("Invitation: ", view.FormBuilder.createForm(d.invitation)),
    li("Program: ", view.FormBuilder.createForm(d.program))
  )
}

object Details {
  case class General(
    underConstruction: Boolean = true,
    contact: String = "nobody",
    style: Code = Code(""),
    headerImages: Seq[String] = Seq(),
    groom: String = "Groom name",
    bride: String = "Bride name",
    date: LocalDateTime = LocalDateTime.now().plusMonths(1),
    location: String = "Nowhere"
  )
  given Format[General] = Json.format[General]
  given view.FormBuilder[General] {
    def createForm(g: General): Frag = ul(
      li("Under construction: ", view.FormBuilder.createForm(g.underConstruction)),
      li("Contact: ", view.FormBuilder.createForm(g.contact)),
      li("Style: ", view.FormBuilder.createForm(g.style)),
      li("Header images: ", view.FormBuilder.createForm(g.headerImages)),
      li("Groom: ", view.FormBuilder.createForm(g.groom)),
      li("Bride: ", view.FormBuilder.createForm(g.bride)),
      li("Date: ", view.FormBuilder.createForm(g.date)),
      li("Location: ", view.FormBuilder.createForm(g.location))
    )
  }

  case class Home(image: String = "", locations: Seq[Home.Location] = Seq())
  object Home {
    case class Location(name: String, time: String, address: String, link: String, details: String)
    given Format[Location] = Json.format[Location]
    given view.FormBuilder[Location] {
      def createForm(l: Location): Frag = ul(
        li("Name: ", view.FormBuilder.createForm(l.name)),
        li("Time: ", view.FormBuilder.createForm(l.time)),
        li("Address: ", view.FormBuilder.createForm(l.address)),
        li("Link: ", view.FormBuilder.createForm(l.link)),
        li("Details: ", view.FormBuilder.createForm(l.details))
      )
    }
  }
  given Format[Home] = Json.format[Home]
  given view.FormBuilder[Home] {
    def createForm(h: Home): Frag = ul(
      li("Image: ", view.FormBuilder.createForm(h.image)),
      li("Locations: ", view.FormBuilder.createForm(h.locations))
    )
  }

  case class Story(title: String = "Our Story", image: String = "", body: String = "We met and falled in love.")
  given Format[Story] = Json.format[Story]
  given view.FormBuilder[Story] {
    def createForm(s: Story): Frag = ul(
      li("Title: ", view.FormBuilder.createForm(s.title)),
      li("Image: ", view.FormBuilder.createForm(s.image)),
      li("Body: ", view.FormBuilder.createForm(s.body))
    )
  }
  
  case class WeddingParty(bridesmaids: Seq[WeddingParty.PartyMember] = Seq(), groomsmen: Seq[WeddingParty.PartyMember] = Seq())
  object WeddingParty {
    case class PartyMember(name: String, role: String, image: String, bio: String)
    given Format[PartyMember] = Json.format[PartyMember]
    given view.FormBuilder[PartyMember] {
      def createForm(pm: PartyMember): Frag = ul(
        li("Name: ", view.FormBuilder.createForm(pm.name)),
        li("Role: ", view.FormBuilder.createForm(pm.role)),
        li("Image: ", view.FormBuilder.createForm(pm.image)),
        li("Bio: ", view.FormBuilder.createForm(pm.bio))
      )
    }
  }
  given Format[WeddingParty] = Json.format[WeddingParty]
  given view.FormBuilder[WeddingParty] {
    def createForm(wp: WeddingParty): Frag = ul(
      li("Bridesmaids: ", view.FormBuilder.createForm(wp.bridesmaids)),
      li("Groomsmen: ", view.FormBuilder.createForm(wp.groomsmen))
    )
  }
  
  case class Photos(photos: Seq[Photos.Photo] = Seq())
  object Photos {
    case class Photo(image: String, caption: Option[String])
    given Format[Photo] = Json.format[Photo]
    given view.FormBuilder[Photo] {
      def createForm(p: Photo): Frag = ul(
        li("Image: ", view.FormBuilder.createForm(p.image)),
        li("Caption: ", view.FormBuilder.createForm(p.caption))
      )
    }
  }
  given Format[Photos] = Json.format[Photos]
  given view.FormBuilder[Photos] {
    def createForm(p: Photos): Frag = ul(
      li("Photos: ", view.FormBuilder.createForm(p.photos))
    )
  }
  
  case class Registry(address: String = "", notes: String = "", items: Seq[Registry.Item] = Seq()) {
    require(items.distinctBy(_.id).length == items.length, "Duplicate id in registry list")
  }
  object Registry {
    case class Item(name: String, id: String, link: String, image: String, price: Option[Double])
    given Format[Item] = Json.format[Item]
    given view.FormBuilder[Item] {
      def createForm(i: Item): Frag = ul(
        li("Name: ", view.FormBuilder.createForm(i.name)),
        li("ID: ", view.FormBuilder.createForm(i.id)),
        li("Link: ", view.FormBuilder.createForm(i.link)),
        li("Image: ", view.FormBuilder.createForm(i.image)),
        li("Price: ", view.FormBuilder.createForm(i.price))
      )
    }
  }
  given Format[Registry] = Json.format[Registry]
  given view.FormBuilder[Registry] {
    def createForm(r: Registry): Frag = ul(
      li("Address: ", view.FormBuilder.createForm(r.address)),
      li("Notes: ", view.FormBuilder.createForm(r.notes)),
      li("Items: ", view.FormBuilder.createForm(r.items))
    )
  }
  
  case class RSVP(notes: String = "", invitations: Seq[RSVP.Invitation] = Seq())
  object RSVP {
    case class Invitation(name: String, people: Seq[String], childrenInvited: Option[Boolean])
    given Format[Invitation] = Json.format[Invitation]
    given view.FormBuilder[Invitation] {
      def createForm(i: Invitation): Frag = ul(
        li("Name: ", view.FormBuilder.createForm(i.name)),
        li("People: ", view.FormBuilder.createForm(i.people)),
        li("Children invited: ", view.FormBuilder.createForm(i.childrenInvited))
      )
    }
  }
  given Format[RSVP] = Json.format[RSVP]
  given view.FormBuilder[RSVP] {
    def createForm(r: RSVP): Frag = ul(
      li("Notes: ", view.FormBuilder.createForm(r.notes)),
      li("Invitation: ", view.FormBuilder.createForm(r.invitations))
    )
  }
  
  case class Hotels(notes: String = "", hotels: Seq[Hotels.Hotel] = Seq())
  object Hotels {
    case class Hotel(name: String, address: String, link: String)
    given Format[Hotel] = Json.format[Hotel]
    given view.FormBuilder[Hotel] {
      def createForm(h: Hotel): Frag = ul(
        li("Name: ", view.FormBuilder.createForm(h.name)),
        li("Address: ", view.FormBuilder.createForm(h.address)),
        li("Link: ", view.FormBuilder.createForm(h.link))
      )
    }
  }
  given Format[Hotels] = Json.format[Hotels]
  given view.FormBuilder[Hotels] {
    def createForm(h: Hotels): Frag = ul(
      li("Notes: ", view.FormBuilder.createForm(h.notes)),
      li("Hotels: ", view.FormBuilder.createForm(h.hotels))
    )
  }

  case class Invitation(
    tagline: String = "Love endures all things",
    parents: String = "Father and mother of the bride",
    details: String = "It's a wedding, come to it",
    url: String = "https://example.com",
    deadline: LocalDate = LocalDate.now().plusWeeks(1)
  )
  given Format[Invitation] = Json.format[Invitation]
  given view.FormBuilder[Invitation] {
    def createForm(i: Invitation): Frag = ul(
      li("Tagline: ", view.FormBuilder.createForm(i.tagline)),
      li("Parents: ", view.FormBuilder.createForm(i.parents)),
      li("Details: ", view.FormBuilder.createForm(i.details)),
      li("URL: ", view.FormBuilder.createForm(i.url)),
      li("Deadline: ", view.FormBuilder.createForm(i.deadline))
    )
  }
  
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
  given Format[Program] = Json.format[Program]
  given view.FormBuilder[Program] {
    def createForm(p: Program): Frag = ul(
      li("Ceremony: ", view.FormBuilder.createForm(p.ceremony)),
      li("Pastors: ", view.FormBuilder.createForm(p.pastors)),
      li("Pianist: ", view.FormBuilder.createForm(p.pianist)),
      li("Ushers: ", view.FormBuilder.createForm(p.ushers)),
      li("Flower girl: ", view.FormBuilder.createForm(p.flowerGirl)),
      li("Ring bearer: ", view.FormBuilder.createForm(p.ringBearer)),
      li("Reception: ", view.FormBuilder.createForm(p.reception)),
      li("Thanks: ", view.FormBuilder.createForm(p.thanks)),
    )
  }

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
