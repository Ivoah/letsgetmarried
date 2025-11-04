package net.ivoah.letsgetmarried

import org.virtuslab.yaml.*

import java.time.LocalDateTime
import scala.io.Source

private given YamlDecoder[LocalDateTime] = YamlDecoder[LocalDateTime] {
  case node: Node => node.as[String].left.map(_.asInstanceOf[ConstructError]).map(LocalDateTime.parse)
}

given Database("jdbc:sqlite:database.db")

private case class YamlDetails(
  underConstruction: Boolean,
  groom: String,
  bride: String,
  image: String,
  date: LocalDateTime,
  location: String,
  invitation: Invitation,
  locations: Seq[Location],
  story: Story,
  bridesmaids: Seq[PartyMember],
  groomsmen: Seq[PartyMember],
  registry: Seq[RegistryItem],
  invitees: Seq[Invitee]
) derives YamlDecoder

case class Invitation(tagline: String, parents: String, details: String, url: String) derives YamlCodec
case class Location(name: String, time: String, address: String, link: String, details: String) derives YamlCodec
case class Story(title: String, body: String) derives YamlCodec
case class PartyMember(name: String, role: String, image: String, bio: String) derives YamlCodec
case class RegistryItem(name: String, link: String, image: String, count: Int, price: Double) derives YamlCodec

case class Invitee(name: String, linked: Seq[String]) derives YamlCodec {
  def findRSVP(): Option[RSVP] = {
    val links = sql"SELECT linked FROM link WHERE original=$name".query(_.getString("linked"))
    sql"SELECT * FROM rsvp WHERE name=$name".query { r => RSVP(
      r.getString("name"),
      r.getBoolean("attending"),
      r.getInt("infants"),
      r.getInt("children"),
      links
    )}.headOption
  }
}

case class RSVP(name: String, attending: Boolean, infants: Int, children: Int, links: Seq[String]) {
  def saveToDatabase(): Unit = {
    sql"""
    INSERT INTO rsvp
    VALUES ($name, $attending, $infants, $children, datetime('now', 'localtime'))
    ON CONFLICT(name)
    DO UPDATE SET
      attending=$attending,
      infants=$infants,
      children=$children,
      updated=datetime('now', 'localtime')
  """.update()

    (sql"INSERT INTO link VALUES " + links.map(linked => sql"($name, $linked)").reduce(_ + _)).update()
  }
}

val Details = Source.fromResource("details.yaml").getLines().mkString("\n").as[YamlDetails] match {
  case Left(err) => throw err
  case Right(details) => details
}
