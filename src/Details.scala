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
given FormBuilder[Code] {
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
given Format[Details] = Json.format
given FormBuilder[Details] {
  def createForm(d: Details): Frag = ul(
    li("General: ", FormBuilder.createForm(d.general)),
    li("Home: ", FormBuilder.createForm(d.home)),
    li("Story: ", FormBuilder.createForm(d.story)),
    li("Wedding Party: ", FormBuilder.createForm(d.weddingParty)),
    li("Photos: ", FormBuilder.createForm(d.photos)),
    li("Registry: ", FormBuilder.createForm(d.registry)),
    li("RSVP: ", FormBuilder.createForm(d.rsvp)),
    li("Hotels: ", FormBuilder.createForm(d.hotels)),
    li("Invitation: ", FormBuilder.createForm(d.invitation)),
    li("Program: ", FormBuilder.createForm(d.program))
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
  given Format[General] = Json.format
  given FormBuilder[General] {
    def createForm(g: General): Frag = ul(
      li("Under construction: ", FormBuilder.createForm(g.underConstruction)),
      li("Contact: ", FormBuilder.createForm(g.contact)),
      li("Style: ", FormBuilder.createForm(g.style)),
      li("Header images: ", FormBuilder.createForm(g.headerImages)),
      li("Groom: ", FormBuilder.createForm(g.groom)),
      li("Bride: ", FormBuilder.createForm(g.bride)),
      li("Date: ", FormBuilder.createForm(g.date)),
      li("Location: ", FormBuilder.createForm(g.location))
    )
  }

  case class Home(image: String = "", locations: Seq[Home.Location] = Seq())
  object Home {
    case class Location(name: String, time: String, address: String, link: String, details: String)
    given Format[Location] = Json.format
    given FormBuilder[Location] {
      def createForm(l: Location): Frag = ul(
        li("Name: ", FormBuilder.createForm(l.name)),
        li("Time: ", FormBuilder.createForm(l.time)),
        li("Address: ", FormBuilder.createForm(l.address)),
        li("Link: ", FormBuilder.createForm(l.link)),
        li("Details: ", FormBuilder.createForm(l.details))
      )
    }
  }
  given Format[Home] = Json.format
  given FormBuilder[Home] {
    def createForm(h: Home): Frag = ul(
      li("Image: ", FormBuilder.createForm(h.image)),
      li("Locations: ", FormBuilder.createForm(h.locations))
    )
  }

  case class Story(title: String = "Our Story", image: String = "", body: String = "We met and falled in love.")
  given Format[Story] = Json.format
  given FormBuilder[Story] {
    def createForm(s: Story): Frag = ul(
      li("Title: ", FormBuilder.createForm(s.title)),
      li("Image: ", FormBuilder.createForm(s.image)),
      li("Body: ", FormBuilder.createForm(s.body))
    )
  }
  
  case class WeddingParty(bridesmaids: Seq[WeddingParty.PartyMember] = Seq(), groomsmen: Seq[WeddingParty.PartyMember] = Seq())
  object WeddingParty {
    case class PartyMember(name: String, role: String, image: String, bio: String)
    given Format[PartyMember] = Json.format
    given FormBuilder[PartyMember] {
      def createForm(pm: PartyMember): Frag = ul(
        li("Name: ", FormBuilder.createForm(pm.name)),
        li("Role: ", FormBuilder.createForm(pm.role)),
        li("Image: ", FormBuilder.createForm(pm.image)),
        li("Bio: ", FormBuilder.createForm(pm.bio))
      )
    }
  }
  given Format[WeddingParty] = Json.format
  given FormBuilder[WeddingParty] {
    def createForm(wp: WeddingParty): Frag = ul(
      li("Bridesmaids: ", FormBuilder.createForm(wp.bridesmaids)),
      li("Groomsmen: ", FormBuilder.createForm(wp.groomsmen))
    )
  }
  
  case class Photos(photos: Seq[Photos.Photo] = Seq())
  object Photos {
    case class Photo(image: String, caption: Option[String])
    given Format[Photo] = Json.format
    given FormBuilder[Photo] {
      def createForm(p: Photo): Frag = ul(
        li("Image: ", FormBuilder.createForm(p.image)),
        li("Caption: ", FormBuilder.createForm(p.caption))
      )
    }
  }
  given Format[Photos] = Json.format
  given FormBuilder[Photos] {
    def createForm(p: Photos): Frag = ul(
      li("Photos: ", FormBuilder.createForm(p.photos))
    )
  }
  
  case class Registry(address: String = "", notes: String = "", items: Seq[Registry.Item] = Seq()) {
    require(items.distinctBy(_.id).length == items.length, "Duplicate id in registry list")
  }
  object Registry {
    case class Item(name: String, id: String, link: String, image: String, price: Option[Double])
    given Format[Item] = Json.format
    given FormBuilder[Item] {
      def createForm(i: Item): Frag = ul(
        li("Name: ", FormBuilder.createForm(i.name)),
        li("ID: ", FormBuilder.createForm(i.id)),
        li("Link: ", FormBuilder.createForm(i.link)),
        li("Image: ", FormBuilder.createForm(i.image)),
        li("Price: ", FormBuilder.createForm(i.price))
      )
    }
  }
  given Format[Registry] = Json.format
  given FormBuilder[Registry] {
    def createForm(r: Registry): Frag = ul(
      li("Address: ", FormBuilder.createForm(r.address)),
      li("Notes: ", FormBuilder.createForm(r.notes)),
      li("Items: ", FormBuilder.createForm(r.items))
    )
  }
  
  case class RSVP(notes: String = "", invitations: Seq[RSVP.Invitation] = Seq())
  object RSVP {
    case class Invitation(name: String, people: Seq[String], childrenInvited: Option[Boolean])
    given Format[Invitation] = Json.format
    given FormBuilder[Invitation] {
      def createForm(i: Invitation): Frag = ul(
        li("Name: ", FormBuilder.createForm(i.name)),
        li("People: ", FormBuilder.createForm(i.people)),
        li("Children invited: ", FormBuilder.createForm(i.childrenInvited))
      )
    }
  }
  given Format[RSVP] = Json.format
  given FormBuilder[RSVP] {
    def createForm(r: RSVP): Frag = ul(
      li("Notes: ", FormBuilder.createForm(r.notes)),
      li("Invitation: ", FormBuilder.createForm(r.invitations))
    )
  }
  
  case class Hotels(notes: String = "", hotels: Seq[Hotels.Hotel] = Seq())
  object Hotels {
    case class Hotel(name: String, address: String, link: String)
    given Format[Hotel] = Json.format
    given FormBuilder[Hotel] {
      def createForm(h: Hotel): Frag = ul(
        li("Name: ", FormBuilder.createForm(h.name)),
        li("Address: ", FormBuilder.createForm(h.address)),
        li("Link: ", FormBuilder.createForm(h.link))
      )
    }
  }
  given Format[Hotels] = Json.format
  given FormBuilder[Hotels] {
    def createForm(h: Hotels): Frag = ul(
      li("Notes: ", FormBuilder.createForm(h.notes)),
      li("Hotels: ", FormBuilder.createForm(h.hotels))
    )
  }

  case class Invitation(
    tagline: String = "Love endures all things",
    parents: String = "Father and mother of the bride",
    details: String = "It's a wedding, come to it",
    url: String = "https://example.com",
    deadline: LocalDate = LocalDate.now().plusWeeks(1)
  )
  given Format[Invitation] = Json.format
  given FormBuilder[Invitation] {
    def createForm(i: Invitation): Frag = ul(
      li("Tagline: ", FormBuilder.createForm(i.tagline)),
      li("Parents: ", FormBuilder.createForm(i.parents)),
      li("Details: ", FormBuilder.createForm(i.details)),
      li("URL: ", FormBuilder.createForm(i.url)),
      li("Deadline: ", FormBuilder.createForm(i.deadline))
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
  given Format[Program] = Json.format
  given FormBuilder[Program] {
    def createForm(p: Program): Frag = ul(
      li("Ceremony: ", FormBuilder.createForm(p.ceremony)),
      li("Pastors: ", FormBuilder.createForm(p.pastors)),
      li("Pianist: ", FormBuilder.createForm(p.pianist)),
      li("Ushers: ", FormBuilder.createForm(p.ushers)),
      li("Flower girl: ", FormBuilder.createForm(p.flowerGirl)),
      li("Ring bearer: ", FormBuilder.createForm(p.ringBearer)),
      li("Reception: ", FormBuilder.createForm(p.reception)),
      li("Thanks: ", FormBuilder.createForm(p.thanks))
    )
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
