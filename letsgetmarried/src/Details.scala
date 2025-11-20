package net.ivoah.letsgetmarried

import org.virtuslab.yaml.*

import java.time.{LocalDate, LocalDateTime}
import scala.io.Source

private given YamlDecoder[LocalDate] = YamlDecoder[LocalDate] {
  case node: Node => node.as[String].left.map(_.asInstanceOf[ConstructError]).map(LocalDate.parse)
}

private given YamlDecoder[LocalDateTime] = YamlDecoder[LocalDateTime] {
  case node: Node => node.as[String].left.map(_.asInstanceOf[ConstructError]).map(LocalDateTime.parse)
}

given Database("jdbc:sqlite:database.db")

private case class YamlDetails(
  underConstruction: Boolean,
  contact: String,
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
  photos: Seq[Photo],
  registry: Seq[RegistryItem],
  invitees: Seq[Invitee]
) derives YamlDecoder

case class Invitation(tagline: String, parents: String, details: String, url: String, deadline: LocalDate) derives YamlDecoder
case class Location(name: String, time: String, address: String, link: String, details: String) derives YamlCodec
case class Story(title: String, image: String, body: String) derives YamlCodec
case class PartyMember(name: String, role: String, image: String, bio: String) derives YamlCodec
case class Photo(image: String, caption: String) derives YamlCodec
case class RegistryItem(name: String, id: String, link: String, image: String, count: Int, price: Double) derives YamlCodec {
  def purchased(): Int = sql"SELECT sum(amount) purchased FROM registryPurchase WHERE id=$id".query(_.getInt("purchased")).headOption.getOrElse(0)

  def purchase(purchasedBy: String) = {
    sql"""
      INSERT INTO registryPurchase
      VALUES ($id, datetime('now', 'localtime'), purchasedBy)
    """
  }
}

case class Invitee(name: String, linked: Seq[String]) derives YamlCodec {
  def findRSVP(): Option[RSVP] = {
    val links = sql"SELECT linked FROM rsvpLink WHERE original=$name".query(_.getString("linked"))
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

    (sql"INSERT INTO rsvpLink VALUES " + links.map(linked => sql"($name, $linked)").reduce(_ + _)).update()
  }
}

val Details = Source.fromResource("details.yaml").getLines().mkString("\n").as[YamlDetails] match {
  case Left(err) => throw err
  case Right(details) => details
}
