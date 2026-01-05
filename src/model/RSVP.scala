package net.ivoah.letsgetmarried
package model

import java.sql.ResultSet

case class RSVP(name: String, people: Seq[String], children: Int, infants: Int, regards: String) {
  val total: Int = people.length + children + infants
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
