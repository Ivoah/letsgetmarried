package net.ivoah.letsgetmarried
package model

import org.virtuslab.yaml.*

import java.time.LocalDate
import scala.io.Source
import scala.math.Ordering.Implicits.seqOrdering

private given YamlCodec[InviteStatus] = new YamlCodec[InviteStatus] {
  override def construct(node: Node)(implicit settings: LoadSettings): Either[ConstructError, InviteStatus] = {
    node.as[String]
      .left.map(_.asInstanceOf[ConstructError])
      .flatMap(s => InviteStatus.values.find(_.key == s).toRight(ConstructError(s"Could not decode InviteStatus", Some(node), Some(InviteStatus.values.map(_.key).mkString(", ")))))
  }

  override def asNode(status: InviteStatus): Node = Node.ScalarNode(status.key)
}

case class Details(
  underConstruction: Boolean,
  contact: String,
  style: Option[String],
  headerImages: Seq[String],
  groom: String,
  bride: String,
  image: String,
  date: LocalDate,
  location: String,
  invitationDetails: Option[InvitationDetails],
  programDetails: Option[ProgramDetails],
  locations: Seq[Location],
  story: Story,
  bridesmaids: Seq[PartyMember],
  groomsmen: Seq[PartyMember],
  photos: Seq[Photo],
  registryAddress: String,
  registryNotes: String,
  registry: Seq[RegistryItem],
  rsvpNotes: String,
  invitations: Seq[Invitation],
  hotelNotes: String,
  hotels: Seq[Hotel]
) derives YamlCodec {
  require(registry.distinctBy(_.id).length == registry.length, "Duplicate id in registry list")

  def serialize: String = this.asYaml
}

case class InvitationDetails(tagline: String, parents: String, details: String, url: String, deadline: LocalDate) derives YamlCodec
case class ProgramDetails(ceremony: Seq[Seq[String]], pastors: Seq[String], pianist: String, ushers: Seq[String], flowerGirl: String, ringBearer: String, reception: Seq[Seq[String]], thanks: String) derives YamlCodec
case class Location(name: String, time: String, address: String, link: String, details: String) derives YamlCodec
case class Story(title: String, image: String, body: String) derives YamlCodec
case class PartyMember(name: String, role: String, image: String, bio: String) derives YamlCodec
case class Photo(image: String, caption: Option[String]) derives YamlCodec
case class RegistryItem(name: String, id: String, link: String, image: String, price: Option[Double]) derives YamlCodec

enum InviteStatus(val key: String) {
  case Invited extends InviteStatus("invited")
  case NotInvited extends InviteStatus("not-invited")
  case NotApplicable extends InviteStatus("n/a")
}

case class Invitation(name: String, people: Seq[String], children: InviteStatus) derives YamlCodec
case class Hotel(name: String, address: String, link: String) derives YamlCodec

val Seating = Source.fromResource("seating.yaml").getLines().mkString("\n").as[Map[String, Seq[String]]] match {
  case Left(err) => throw err
  case Right(seating) =>
    val s = seating.toSeq
      .flatMap { case (k, vv) => vv.map(v => v -> k) }
      .sortBy(_._1.split("\\s+").reverse.toSeq)
    s ++ Seq.fill(30 - s.length % 30)("" -> "")
}
