package net.ivoah.letsgetmarried

import org.virtuslab.yaml.*
import scalatags.Text.all.*

import java.time.LocalDate
import scala.io.Source

private given YamlDecoder[LocalDate] = YamlDecoder[LocalDate] {
  case node: Node => node.as[String].left.map(_.asInstanceOf[ConstructError]).map(LocalDate.parse)
}

private case class YamlDetails(
  groom: String,
  bride: String,
  hero: String,
  date: LocalDate,
  location: String,
  locations: Seq[Location],
  story: Story,
  bridesmaids: Seq[PartyMember],
  groomsmen: Seq[PartyMember]
) derives YamlDecoder
case class Location(name: String, time: String, address: String, link: String, details: String) derives YamlCodec
case class Story(title: String, body: String) derives YamlCodec
case class PartyMember(name: String, role: String, bio: String) derives YamlCodec {
  def render: Frag = div(
    div(
      h3(s"$name - $role"),
      Markdown.render(bio)
    )
  )
}

val Details = Source.fromResource("details.yaml").getLines().mkString("\n").as[YamlDetails] match {
  case Left(err) => throw err
  case Right(details) => details
}
