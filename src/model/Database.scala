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
      VALUES (${rsvp.name}, ${rsvp.people.mkString(",")}, ${rsvp.children}, ${rsvp.infants}, datetime('now', 'localtime'), ${rsvp.regards})
      ON CONFLICT(name)
      DO UPDATE SET
        people=${rsvp.people.mkString(",")},
        children=${rsvp.children},
        infants=${rsvp.infants},
        updated=datetime('now', 'localtime'),
        regards=${rsvp.regards}
    """.update() == 1
  }

  def getRegistryItemPurchase(item: RegistryItem): Option[String] = {
    if (item.price.isEmpty) None
    else sql"SELECT purchasedBy FROM gift WHERE id=${item.id}".query(_.getString(1)).headOption
  }
  def addRegistryItemPurchase(item: RegistryItem, purchasedBy: String, amount: Option[Double], notes: String): Boolean = {
    sql"""
      INSERT INTO gift
      VALUES (${item.id}, datetime('now', 'localtime'), $purchasedBy, ${amount.orNull}, $notes)
    """.update() == 1
  }
  def removeRegistryItemPurchase(item: RegistryItem): Boolean = sql"""DELETE FROM gift WHERE id=${item.id}""".update() == 1
}
