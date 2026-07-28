package net.ivoah.letsgetmarried
package model

import org.virtuslab.yaml.*

import java.time.{LocalDate, LocalDateTime}
import scala.io.Source
import scala.math.Ordering.Implicits.seqOrdering

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
) derives YamlCodec

object Details {
  case class General(underConstruction: Boolean, contact: String, style: Option[String], headerImages: Seq[String], groom: String, bride: String, date: LocalDateTime, location: String) derives YamlCodec

  case class Home(image: String, locations: Seq[Home.Location]) derives YamlCodec

  object Home {
    case class Location(name: String, time: String, address: String, link: String, details: String) derives YamlCodec
  }

  case class Story(title: String, image: String, body: String) derives YamlCodec
  
  case class WeddingParty(bridesmaids: Seq[WeddingParty.PartyMember], groomsmen: Seq[WeddingParty.PartyMember]) derives YamlCodec
  object WeddingParty {
    case class PartyMember(name: String, role: String, image: String, bio: String) derives YamlCodec
  }
  
  case class Photos(photos: Seq[Photos.Photo]) derives YamlCodec
  object Photos {
    case class Photo(image: String, caption: Option[String]) derives YamlCodec
  }
  
  case class Registry(address: String,notes: String,items: Seq[Registry.Item]) derives YamlCodec {
    require(items.distinctBy(_.id).length == items.length, "Duplicate id in registry list")
  }
  object Registry {
    case class Item(name: String, id: String, link: String, image: String, price: Option[Double]) derives YamlCodec
  }
  
  case class RSVP(notes: String,invitations: Seq[RSVP.Invitation]) derives YamlCodec
  object RSVP {
    case class Invitation(name: String, people: Seq[String], childrenInvited: Option[Boolean]) derives YamlCodec
  }
  
  case class Hotels(notes: String, hotels: Seq[Hotels.Hotel]) derives YamlCodec
  object Hotels {
    case class Hotel(name: String, address: String, link: String) derives YamlCodec
  }

  case class Invitation(tagline: String, parents: String, details: String, url: String, deadline: LocalDate) derives YamlCodec
  
  case class Program(ceremony: Seq[Seq[String]], pastors: Seq[String], pianist: String, ushers: Seq[String], flowerGirl: String, ringBearer: String, reception: Seq[Seq[String]], thanks: String) derives YamlCodec
}

val Seating = Source.fromResource("seating.yaml").getLines().mkString("\n").as[Map[String, Seq[String]]] match {
  case Left(err) => throw err
  case Right(seating) =>
    val s = seating.toSeq
      .flatMap { case (k, vv) => vv.map(v => v -> k) }
      .sortBy(_._1.split("\\s+").reverse.toSeq)
    s ++ Seq.fill(30 - s.length % 30)("" -> "")
}
