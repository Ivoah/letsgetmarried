package net.ivoah.letsgetmarried

import net.ivoah.squall.*
import play.api.libs.json.*
import scala.io.Source
import java.nio.file.{Files, Paths}

object Database {
  given Connector = {
    if (Files.notExists(Paths.get("database.db"))) {
      given Connector("jdbc:sqlite:database.db")
      val schema = Source.fromResource("schema.sql").getLines().mkString("\n")
      schema.sql.execute()
      summon[Connector]
    } else Connector("jdbc:sqlite:database.db")
  }

  def getDetails(): Details = {
    sql"SELECT details FROM details ORDER BY date DESC LIMIT 1"
      .query(r => Json.parse(r.getString("details")).asOpt[Details]).flatten.headOption.getOrElse(FormBuilder.default[Details])
  }

  def saveDetails(details: Details): Boolean = {
    sql"INSERT INTO details (details, date) VALUES (${Json.stringify(Json.toJson(details))}, datetime('now', 'localtime'))".update() == 1
  }

  def getAllRSVPs(): Seq[RSVP] = sql"SELECT * FROM rsvp".query(RSVP.fromResultSet)
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

  def getAllGifts(): Seq[Gift] = sql"SELECT * FROM gift".query(Gift.fromResultSet)
  def getRegistryItemPurchase(item: Details.Registry.Item): Option[String] = {
    if (item.price.isEmpty) None
    else sql"SELECT purchasedBy FROM gift WHERE id=${item.id}".query(_.getString(1)).headOption
  }
  def addRegistryItemPurchase(item: Details.Registry.Item, purchasedBy: String, amount: Option[Double], notes: String): Boolean = {
    sql"""
      INSERT INTO gift
      VALUES (${item.id}, datetime('now', 'localtime'), $purchasedBy, ${amount.orNull}, $notes)
    """.update() == 1
  }
  def removeRegistryItemPurchase(item: Details.Registry.Item): Boolean = sql"""DELETE FROM gift WHERE id=${item.id}""".update() == 1
}
