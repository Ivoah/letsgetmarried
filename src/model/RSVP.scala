package net.ivoah.letsgetmarried
package model

import scalatags.Text.all.*
import java.sql.ResultSet

case class RSVP(name: String, people: Seq[String], children: Int, infants: Int, regards: String) {
  val total: Int = people.length + children + infants

  def details: Frag = div(cls:="rsvpDetails",
    p(s"Received RSVP for $name."),
    ul(
      li(s"Adults: ${if (people.nonEmpty) people.mkString(", ") else "None :("}"),
      li(s"Children: $children"),
      li(s"Infants: $infants"),
    ),
    p(regards)
  )
}

object RSVP {
  def fromResultSet(r: ResultSet): RSVP = RSVP(
    r.getString("name"),
    r.getString("people").split(",").filter(_.nonEmpty),
    r.getInt("children"),
    r.getInt("infants"),
    r.getString("regards")
  )
}
