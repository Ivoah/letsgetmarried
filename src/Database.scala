package net.ivoah.letsgetmarried

import net.ivoah.squall.*
import play.api.libs.json.*
import scala.io.Source
import java.nio.file.{Files, Paths}
import scala.jdk.StreamConverters.*

object Database {
  given Connector = {
    if (Files.notExists(Paths.get("database.db"))) {
      val conn = Connector("jdbc:sqlite:database.db")
      val schema = Source.fromResource("schema.sql").getLines().mkString("\n")
      val version = getMigrations().map(_._1).maxOption.getOrElse(0)
      migrate(version, schema)(using conn)
      conn
    } else Connector("jdbc:sqlite:database.db")
  }

  private def getMigrations(laterThan: Int = 0): Map[Int, String] = {
    Files.list(Paths.get("schema_migrations"))
      .toScala(List)
      .collect(p => p.getFileName.toString match {
        case s"v${IntExtractor(version)}.sql" if version > laterThan => version -> Files.readString(p)
      })
      .toMap
  }

  def migrateAll(): Unit = {
    val current_version = sql"SELECT coalesce(max(version), 0) version FROM schema_migrations".query(_.getInt("version")).head
    getMigrations(current_version).toSeq.sortBy(_._1).foreach(migrate.tupled)
  }

  private def migrate(version: Int, script: String)(using Connector): Unit = {
    println(s"Migrating schema to v$version:")
    println(script)
    try {
      script.split(";").foreach(_.sql.execute())
      sql"INSERT INTO schema_migrations (version, script) VALUES ($version, $script)".execute()
      println(s"Successfully migrated schema to v$version")
    } catch {
      case e: Exception =>
        println(s"Could not migrate schema to v$version")
        e.printStackTrace()
        System.exit(1)
    }
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
