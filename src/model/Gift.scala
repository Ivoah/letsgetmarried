package net.ivoah.letsgetmarried
package model

import scalatags.Text.all.*
import java.sql.ResultSet
import java.time.LocalDate

case class Gift(id: String, purchasedAt: LocalDate, purchasedBy: String, amount: Option[Double], notes: String)

object Gift {
  def fromResultSet(r: ResultSet): Gift = Gift(
    r.getString("id"),
    r.getDate("purchasedAt").toLocalDate,
    r.getString("purchasedBy"),
    {val amount = r.getDouble("amount"); if (r.wasNull) None else Some(amount)},
    r.getString("notes")
  )
}
