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

private given YamlDecoder[InviteStatus] = YamlDecoder[InviteStatus] {
  case node: Node => node.as[String]
    .left.map(_.asInstanceOf[ConstructError])
    .flatMap(s => InviteStatus.values.find(_.key == s).toRight(ConstructError(s"Could not decode InviteStatus", Some(node), Some(InviteStatus.values.map(_.key).mkString(", ")))))
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
  invitationDetails: InvitationDetails,
  locations: Seq[Location],
  story: Story,
  bridesmaids: Seq[PartyMember],
  groomsmen: Seq[PartyMember],
  photos: Seq[Photo],
  registryAddress: String,
  registry: Seq[RegistryItem],
  invitations: Seq[Invitation]
) derives YamlDecoder {
  require(registry.distinctBy(_.id).length == registry.length, "Duplicate id in registry list")
}

case class InvitationDetails(tagline: String, parents: String, details: String, url: String, deadline: LocalDate) derives YamlDecoder
case class Location(name: String, time: String, address: String, link: String, details: String) derives YamlCodec
case class Story(title: String, image: String, body: String) derives YamlCodec
case class PartyMember(name: String, role: String, image: String, bio: String) derives YamlCodec
case class Photo(image: String, caption: Option[String]) derives YamlCodec
case class RegistryItem(name: String, id: String, link: String, image: String, price: Option[Double]) derives YamlCodec {
  def purchased(): Boolean = price.nonEmpty && sql"SELECT count(*) > 0 FROM registryPurchase WHERE id=$id".query(_.getBoolean(1)).headOption.getOrElse(false)

  def purchase(purchasedBy: String, amount: Option[Double]): Boolean = {
    sql"""
      INSERT INTO registryPurchase
      VALUES ($id, datetime('now', 'localtime'), $purchasedBy, ${amount.orNull})
    """.update() == 1
  }
}

enum InviteStatus(val key: String) {
  case Invited extends InviteStatus("invited")
  case NotInvited extends InviteStatus("not-invited")
  case NotApplicable extends InviteStatus("n/a")
}

case class Invitation(name: String, people: Seq[String], children: InviteStatus, plusone: Boolean) derives YamlDecoder {
  def findRSVP(): Option[RSVP] = {
    sql"SELECT * FROM rsvp WHERE name=$name".query(r => RSVP(
      r.getString("name"),
      r.getInt("adults"),
      r.getInt("children"),
      r.getInt("infants")
    )).headOption
  }
}

case class RSVP(name: String, adults: Int, children: Int, infants: Int) {
  def saveToDatabase(): Boolean = {
    sql"""
      INSERT INTO rsvp
      VALUES ($name, $adults, $children, $infants, datetime('now', 'localtime'))
      ON CONFLICT(name)
      DO UPDATE SET
        adults=$adults,
        children=$children,
        infants=$infants,
        updated=datetime('now', 'localtime')
    """.update() == 1
  }
}

val Details = Source.fromResource("details.yaml").getLines().mkString("\n").as[YamlDetails] match {
  case Left(err) => throw err
  case Right(details) => details
}
