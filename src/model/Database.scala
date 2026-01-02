package net.ivoah.letsgetmarried
package model

import sql.*

object Database {
  given Database = model.sql.Database("jdbc:sqlite:database.db")

  def allRSVPs(): Seq[RSVP] = sql"SELECT * FROM rsvp".query(RSVP.fromResultSet)
  def findRSVP(name: String): Option[RSVP] = sql"SELECT * FROM rsvp WHERE name=$name".query(RSVP.fromResultSet).headOption
  def saveRSVP(rsvp: RSVP): Boolean = {
    sql"""
      INSERT INTO rsvp
      VALUES (${rsvp.name}, ${rsvp.people.mkString(",")}, ${rsvp.children}, ${rsvp.infants}, datetime('now', 'localtime'))
      ON CONFLICT(name)
      DO UPDATE SET
        people=${rsvp.people.mkString(",")},
        children=${rsvp.children},
        infants=${rsvp.infants},
        updated=datetime('now', 'localtime')
    """.update() == 1
  }

  def getRegistryItemPurchased(item: RegistryItem): Boolean = item.price.nonEmpty && sql"SELECT count(*) > 0 FROM gift WHERE id=${item.id}".query(_.getBoolean(1)).headOption.getOrElse(false)
  def markRegistryItemPurchased(item: RegistryItem, purchasedBy: String, amount: Option[Double]): Boolean = {
    sql"""
      INSERT INTO gift
      VALUES (${item.id}, datetime('now', 'localtime'), $purchasedBy, ${amount.orNull})
    """.update() == 1
  }
}
